package com.jdc.recipe_service.dev.service.image;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.LogSanitizer;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * OpenAI 이미지 생성 (gpt-image-2 등) 호출 + S3 원본 업로드.
 * 응답 contract는 GeminiImageService와 동일한 S3 webp URL 리스트.
 *
 * apiModel/quality는 호출자(DevImageGenRouterService)가 DevImageGenModel에서 결정해 넘긴다.
 * (전역 default model 설정에 의존하지 않음 — 모델 선택은 화이트리스트 enum이 single source of truth.)
 *
 * 외부 호출 정책 (external-api 스킬):
 *  - timeout: RestTemplate bean 기본 사용
 *  - retry: 5xx만 RestClientException 1회 backoff (총 2 attempts)
 *  - 401/403: CustomException(INTERNAL_SERVER_ERROR)로 throw — 설정 오류는 묻지 않고 시끄럽게
 *  - 429: warn log + DEFAULT_IMAGE_URL fallback (rate limit은 graceful degrade)
 *  - 그 외 4xx, 응답 파싱 실패, b64 decode 실패, S3 업로드 실패: warn log + DEFAULT_IMAGE_URL
 *  - secret: ${app.openai.api-key}로 env에서만 주입. 로그/응답에 토큰 노출 금지
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevGptImageService {

    private final RestTemplate restTemplate;
    private final S3Util s3Util;

    @Value("${app.openai.api-key:}")
    private String openAiApiKey;

    @Value("${app.openai.image.size:1024x1024}")
    private String openAiImageSize;

    @Value("${app.openai.image.endpoint:https://api.openai.com/v1/images/generations}")
    private String openAiImageEndpoint;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    @Retryable(
            retryFor = { RestClientException.class },
            // HttpClientErrorException(4xx)만 제외. HttpServerErrorException(5xx)은 RestClientException
            // 자식이지만 noRetryFor에서 제외시키지 않아야 재시도 대상이 된다.
            // (HttpStatusCodeException로 noRetryFor를 두면 5xx까지 같이 빠져버려서 재시도가 안 됨.)
            noRetryFor = { HttpClientErrorException.class },
            maxAttempts = 2,
            backoff = @Backoff(delay = 1000)
    )
    public List<String> generateImageUrls(String prompt, String apiModel, String quality,
                                          Long userId, Object recipeId) {
        if (openAiApiKey == null || openAiApiKey.isBlank()) {
            log.error("[DevGptImageService] OPENAI_API_KEY 미설정. recipeId={}", recipeId);
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }

        log.info("[DevGptImageService] 이미지 생성 요청. apiModel={}, quality={}, recipeId={}",
                apiModel, quality, recipeId);

        ResponseEntity<Map> response;
        try {
            response = callOpenAi(prompt, apiModel, quality);
        } catch (HttpStatusCodeException e) {
            return handleHttpError(e, recipeId);
        }

        // 응답 body 파싱 + S3 업로드는 별도 try-catch.
        // 여기서 발생하는 ClassCastException/NPE/Base64/S3 예외는 retry 가치 없으므로 fallback만.
        try {
            return parseAndUpload(response, userId, recipeId);
        } catch (RuntimeException e) {
            log.error("[DevGptImageService] 응답 파싱/업로드 실패. recipeId={}, error={}",
                    recipeId, LogSanitizer.mask(e.getMessage()));
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }
    }

    private ResponseEntity<Map> callOpenAi(String prompt, String apiModel, String quality) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(openAiApiKey);

        Map<String, Object> body = Map.of(
                "model", apiModel,
                "prompt", prompt,
                "n", 1,
                "size", openAiImageSize,
                "quality", quality
        );

        return restTemplate.postForEntity(
                openAiImageEndpoint,
                new HttpEntity<>(body, headers),
                Map.class
        );
    }

    private List<String> handleHttpError(HttpStatusCodeException e, Object recipeId) {
        int code = e.getStatusCode().value();
        String body = LogSanitizer.mask(shorten(e.getResponseBodyAsString()));

        if (code == 401 || code == 403) {
            // 설정/권한 오류는 silent fallback 금지 — 운영에서 알아채야 함
            log.error("[DevGptImageService] OpenAI 인증 실패 ({}): recipeId={}, body={}",
                    code, recipeId, body);
            throw new CustomException(
                    ErrorCode.INTERNAL_SERVER_ERROR,
                    "OpenAI 이미지 API 인증 실패 (HTTP " + code + ")"
            );
        }

        if (code == 429) {
            log.warn("[DevGptImageService] OpenAI rate limited (429): recipeId={}, body={}",
                    recipeId, body);
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }

        if (e.getStatusCode().is5xxServerError()) {
            // @Retryable이 잡도록 다시 던짐
            log.warn("[DevGptImageService] OpenAI 5xx ({}): recipeId={}, body={}", code, recipeId, body);
            throw e;
        }

        // 그 외 4xx (400 등) — 입력 문제일 가능성, fallback
        log.warn("[DevGptImageService] OpenAI HTTP {}: recipeId={}, body={}", code, recipeId, body);
        return Collections.singletonList(DEFAULT_IMAGE_URL);
    }

    @SuppressWarnings("unchecked")
    private List<String> parseAndUpload(ResponseEntity<Map> response, Long userId, Object recipeId) {
        Map<String, Object> resp = response.getBody();
        if (resp == null || !resp.containsKey("data")) {
            log.error("[DevGptImageService] 응답에 data 없음. recipeId={}", recipeId);
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }

        List<Map<String, Object>> data = (List<Map<String, Object>>) resp.get("data");
        if (data == null || data.isEmpty()) {
            log.error("[DevGptImageService] data 배열 비어 있음. recipeId={}", recipeId);
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }

        Object b64Obj = data.get(0).get("b64_json");
        if (!(b64Obj instanceof String b64) || b64.isBlank()) {
            log.error("[DevGptImageService] b64_json 누락 또는 형식 오류. recipeId={}", recipeId);
            return Collections.singletonList(DEFAULT_IMAGE_URL);
        }

        String webpUrl = uploadOriginalToS3(b64, userId, recipeId);
        log.info("[DevGptImageService] 이미지 생성 완료. recipeId={}", recipeId);
        return Collections.singletonList(webpUrl);
    }

    @Recover
    public List<String> recover(RestClientException e, String prompt, String apiModel, String quality,
                                Long userId, Object recipeId) {
        log.error("[DevGptImageService] 재시도 소진. 기본 이미지 반환. recipeId={}, error={}",
                recipeId, LogSanitizer.mask(e.getMessage()));
        return Collections.singletonList(DEFAULT_IMAGE_URL);
    }

    private String uploadOriginalToS3(String base64, Long userId, Object recipeId) {
        byte[] bytes = Base64.getDecoder().decode(base64);

        // GeminiImageService와 동일한 키 패턴: original 업로드 → Lambda가 webp로 변환
        String originalKey = String.format("original/images/recipes/%d/%s/main.png", userId, recipeId);
        String finalWebpKey = String.format("images/recipes/%d/%s/main.webp", userId, recipeId);

        s3Util.upload(bytes, originalKey, "image/png");

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, finalWebpKey);
    }

    private String shorten(String s) {
        if (s == null) return "null";
        return s.length() > 300 ? s.substring(0, 300) + "..." : s;
    }
}
