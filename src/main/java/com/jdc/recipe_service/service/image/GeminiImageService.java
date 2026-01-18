package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.*;
import java.util.Base64;

@Service
@RequiredArgsConstructor
@Slf4j
public class GeminiImageService {

    private final RestTemplate restTemplate;
    private final S3Util s3Util;

    @Value("${gemini.api-key}")
    private String geminiApiKey;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    private static final String GCP_PROJECT_ID = "gen-lang-client-0326396795";
    private static final String GEMINI_MODEL_ID = "gemini-2.5-flash-image";

    private static final String VERTEX_GEMINI_URL =
            "https://aiplatform.googleapis.com/v1/projects/" + GCP_PROJECT_ID
                    + "/locations/global/publishers/google/models/" + GEMINI_MODEL_ID
                    + ":generateContent?key={key}";

    static class NoImageGeneratedException extends RuntimeException {
        NoImageGeneratedException(String msg) { super(msg); }
    }

    @Retryable(
            retryFor = { RestClientException.class },
            maxAttempts = 3,
            backoff = @Backoff(delay = 2000)
    )
    public List<String> generateImageUrls(String prompt, Long userId, Long recipeId) {
        log.info("[GeminiImageService] Vertex AI - Gemini 2.5 flash");

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        String url = VERTEX_GEMINI_URL.replace("{key}", geminiApiKey);

        Map<String, Object> body1 = buildRequestBody(prompt);
        Map<String, Object> resp1 = post(url, headers, body1);

        try {
            return parseVertexResponse(resp1, userId, recipeId);
        } catch (NoImageGeneratedException e) {
            String safePrompt = buildSafePrompt(prompt);
            log.warn("⚠️ 이미지 0장 -> 세이프 프롬프트로 1회 재시도. recipeId={}, promptHash={}",
                    recipeId, promptHash(prompt));

            try {
                Map<String, Object> body2 = buildRequestBody(safePrompt);
                Map<String, Object> resp2 = post(url, headers, body2);
                return parseVertexResponse(resp2, userId, recipeId);
            } catch (NoImageGeneratedException e2) {
                log.error("❌ 세이프 프롬프트도 이미지 0장. 기본 이미지로 폴백. recipeId={}, 원인={}",
                        recipeId, e2.getMessage());
                return Collections.singletonList(DEFAULT_IMAGE_URL);
            }
        }
    }

    @Recover
    public List<String> recover(RestClientException e, String prompt, Long userId, Long recipeId) {
        log.error("❌ 이미지 생성 최종 실패 (네트워크 재시도 소진). 기본 이미지를 반환합니다. recipeId={}, 원인={}",
                recipeId, e.getMessage());
        return Collections.singletonList(DEFAULT_IMAGE_URL);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> post(String url, HttpHeaders headers, Map<String, Object> body) {
        try {
            ResponseEntity<Map> response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);
            return (Map<String, Object>) response.getBody();
        } catch (RestClientException e) {
            log.warn("⚠️ Vertex AI 호출 실패 (재시도 예정): {}", e.getMessage());
            throw e;
        }
    }

    private Map<String, Object> buildRequestBody(String prompt) {
        Map<String, Object> imageConfig = Map.of("aspectRatio", "1:1");

        Map<String, Object> generationConfig = Map.of(
                "responseModalities", List.of("IMAGE"),
                "candidateCount", 1,
                "imageConfig", imageConfig
        );

        String enhancedPrompt = prompt + " , high quality, photorealistic food photography, 1:1 aspect ratio";

        return Map.of(
                "contents", List.of(
                        Map.of("role", "user", "parts", List.of(Map.of("text", enhancedPrompt)))
                ),
                "generationConfig", generationConfig
        );
    }

    private String buildSafePrompt(String prompt) {
        return prompt
                + " , photorealistic plated food on a clean table, studio lighting"
                + " , no text, no logos, no people, no hands, no labels";
    }

    private int promptHash(String prompt) {
        return prompt == null ? 0 : prompt.hashCode();
    }

    @SuppressWarnings("unchecked")
    private List<String> parseVertexResponse(Map<String, Object> responseBody, Long userId, Long recipeId) {
        if (responseBody == null || !responseBody.containsKey("candidates")) {
            log.error("🚨 응답 오류: {}", safeToString(responseBody));
            throw new NoImageGeneratedException("Gemini 응답에 candidates가 없습니다.");
        }

        List<Map<String, Object>> candidates = (List<Map<String, Object>>) responseBody.get("candidates");
        List<String> imageUrls = new ArrayList<>();

        for (int i = 0; i < candidates.size(); i++) {
            Map<String, Object> candidate = candidates.get(i);

            if (candidate.containsKey("finishReason")) {
                String finishReason = String.valueOf(candidate.get("finishReason"));
                if (!"STOP".equals(finishReason)) {
                    log.warn("⚠️ 이미지 생성 중단됨. recipeId={}, candidateIndex={}, FinishReason={}",
                            recipeId, i, finishReason);
                }
            }

            Map<String, Object> content = (Map<String, Object>) candidate.get("content");
            if (content == null || !content.containsKey("parts")) {
                log.warn("⚠️ 생성된 컨텐츠가 비어있습니다(안전/차단 가능). recipeId={}, candidateIndex={}, promptFeedback={}, safetyRatings={}",
                        recipeId, i, safeToString(responseBody.get("promptFeedback")), safeToString(candidate.get("safetyRatings")));
                continue;
            }

            List<Map<String, Object>> parts = (List<Map<String, Object>>) content.get("parts");
            for (Map<String, Object> part : parts) {
                if (!part.containsKey("inlineData")) continue;

                Map<String, Object> inlineData = (Map<String, Object>) part.get("inlineData");
                Object dataObj = inlineData == null ? null : inlineData.get("data");
                if (!(dataObj instanceof String)) {
                    log.warn("⚠️ inlineData.data 비정상. recipeId={}, partKeys={}, inlineKeys={}",
                            recipeId, part.keySet(), inlineData == null ? "null" : inlineData.keySet());
                    continue;
                }

                String base64Data = (String) dataObj;

                if (base64Data.startsWith("data:")) {
                    int comma = base64Data.indexOf(',');
                    if (comma > 0 && comma + 1 < base64Data.length()) {
                        base64Data = base64Data.substring(comma + 1);
                    }
                }

                imageUrls.add(uploadOriginalToS3(base64Data, userId, recipeId));
            }
        }

        if (imageUrls.isEmpty()) {
            Map<String, Object> c0 = candidates.isEmpty() ? null : candidates.get(0);
            log.warn("🚨 이미지 0장. recipeId={}, promptFeedback={}, candidate0.finishReason={}, candidate0.safetyRatings={}",
                    recipeId,
                    safeToString(responseBody.get("promptFeedback")),
                    c0 == null ? "null" : safeToString(c0.get("finishReason")),
                    c0 == null ? "null" : safeToString(c0.get("safetyRatings"))
            );
            throw new NoImageGeneratedException("❌ 이미지가 생성되지 않았습니다.");
        }

        log.info("✅ Gemini 이미지 생성 완료: {}장 (recipeId={})", imageUrls.size(), recipeId);
        return imageUrls;
    }

    private String safeToString(Object o) {
        if (o == null) return "null";
        String s = String.valueOf(o);
        return s.length() > 600 ? s.substring(0, 600) + "..." : s;
    }

    /**
     * 1) 'original/' 폴더에 원본(JPG) 업로드 -> Lambda가 감지하고 동작함
     * 2) DB에는 'images/' 폴더의 WebP URL을 미리 반환 (Lambda가 변환해서 거기에 넣어둘 것이므로)
     */
    private String uploadOriginalToS3(String base64, Long userId, Long recipeId) {
        byte[] bytes = Base64.getDecoder().decode(base64);

        String originalKey = String.format("original/images/recipes/%d/%d/main.jpg", userId, recipeId);
        String finalWebpKey = String.format("images/recipes/%d/%d/main.webp", userId, recipeId);

        s3Util.upload(bytes, originalKey, "image/jpeg");

        log.info("📤 원본 업로드 완료 (-> Lambda 변환 대기): {}", originalKey);
        log.info("🔗 DB 저장 예정 URL: {}", finalWebpKey);

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, finalWebpKey);
    }
}
