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
import org.springframework.web.client.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
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

    @Value("#{'${app.vertex.locations:global,asia-northeast3,us-central1}'.split(',')}")
    private List<String> vertexLocations;

    @Value("${app.vertex.cooldown-ms:30000}")
    private long cooldownMs;

    private final Map<String, Long> cooldownUntil = new ConcurrentHashMap<>();

    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    private static final String GCP_PROJECT_ID = "gen-lang-client-0326396795";
    private static final String GEMINI_MODEL_ID = "gemini-2.5-flash-image";

    static class NoImageGeneratedException extends RuntimeException {
        NoImageGeneratedException(String msg) { super(msg); }
    }

    /** ✅ 추천: 고정 2초 3회 대신 지수 백오프(폭주 시 효과 큼) */
    @Retryable(
            retryFor = { RestClientException.class },
            maxAttempts = 2,
            backoff = @Backoff(delay = 1000, multiplier = 2.0, maxDelay = 20000, random = true)
    )
    public List<String> generateImageUrls(String prompt, Long userId, Long recipeId) {
        log.info("[GeminiImageService] Vertex AI - Gemini 2.5 flash (failover enabled), recipeId={}", recipeId);

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        Map<String, Object> body1 = buildRequestBody(prompt);
        Map<String, Object> resp1 = postWithFailover(headers, body1, recipeId);

        try {
            return parseVertexResponse(resp1, userId, recipeId);
        } catch (NoImageGeneratedException e) {
            String safePrompt = buildSafePrompt(prompt);
            log.warn("⚠️ 이미지 0장 -> 세이프 프롬프트로 1회 재시도. recipeId={}, promptHash={}",
                    recipeId, promptHash(prompt));

            try {
                Map<String, Object> body2 = buildRequestBody(safePrompt);
                Map<String, Object> resp2 = postWithFailover(headers, body2, recipeId);
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
        log.error("❌ 이미지 생성 최종 실패 (재시도 소진). 기본 이미지를 반환합니다. recipeId={}, 원인={}",
                recipeId, e.getMessage());
        return Collections.singletonList(DEFAULT_IMAGE_URL);
    }

    /** ✅ location별 URL 생성 */
    private String vertexUrl(String location) {
        String loc = location.trim();
        return "https://aiplatform.googleapis.com/v1/projects/" + GCP_PROJECT_ID
                + "/locations/" + loc
                + "/publishers/google/models/" + GEMINI_MODEL_ID
                + ":generateContent?key=" + geminiApiKey;
    }

    private boolean inCooldown(String loc) {
        Long until = cooldownUntil.get(loc);
        return until != null && until > System.currentTimeMillis();
    }

    private void markCooldown(String loc) {
        cooldownUntil.put(loc, System.currentTimeMillis() + cooldownMs);
    }

    /** ✅ 핵심: global -> region 순으로 failover */
    @SuppressWarnings("unchecked")
    private Map<String, Object> postWithFailover(HttpHeaders headers, Map<String, Object> body, Long recipeId) {
        RuntimeException last = null;

        for (String locRaw : vertexLocations) {
            String loc = locRaw.trim();
            if (loc.isEmpty()) continue;
            if (inCooldown(loc)) continue;

            String url = vertexUrl(loc);

            try {
                ResponseEntity<Map> response =
                        restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);

                Map<String, Object> respBody = (Map<String, Object>) response.getBody();
                log.info("✅ Vertex 호출 성공 (location={}, recipeId={})", loc, recipeId);
                return respBody;

            } catch (HttpStatusCodeException e) {
                int code = e.getStatusCode().value();

                // ✅ 429 / 5xx면 다른 region으로 넘어감
                if (code == 429 || code == 404 || (code >= 500 && code <= 599)) {
                    log.warn("⚠️ Vertex 실패 -> failover (location={}, code={}, recipeId={}, msg={})",
                            loc, code, recipeId, shorten(e.getResponseBodyAsString()));
                    markCooldown(loc);
                    last = e;
                    continue;
                }

                // ✅ 그 외 4xx는 요청 자체 문제일 확률이 커서 failover 의미 없음
                throw e;

            } catch (ResourceAccessException e) {
                // ✅ 네트워크 타임아웃/연결 실패류도 failover
                log.warn("⚠️ Vertex 네트워크 실패 -> failover (location={}, recipeId={}, msg={})",
                        loc, recipeId, e.getMessage());
                markCooldown(loc);
                last = e;

            } catch (RestClientException e) {
                // ✅ 기타 RestTemplate 예외도 일단 failover 시도
                log.warn("⚠️ Vertex 호출 실패 -> failover (location={}, recipeId={}, msg={})",
                        loc, recipeId, e.getMessage());
                markCooldown(loc);
                last = e;
            }
        }

        // 모든 location이 실패하면 Retryable로 넘겨서 백오프 재시도
        if (last instanceof RestClientException re) throw re;
        throw new RestClientException("All Vertex locations failed", last);
    }

    private String shorten(String s) {
        if (s == null) return "null";
        return s.length() > 300 ? s.substring(0, 300) + "..." : s;
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
