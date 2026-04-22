package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.util.LogSanitizer;
import com.jdc.recipe_service.util.S3Util;
import jakarta.annotation.PostConstruct;
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

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    @Value("#{'${app.vertex.locations:global,us-central1,asia-northeast3}'.split(',')}")
    private List<String> vertexLocations;

    @Value("#{'${app.vertex.credentials}'.split(',')}")
    private List<String> rawCredentials;

    private final List<VertexCredential> credentials = new ArrayList<>();

    @Value("${app.vertex.cooldown-ms:10000}")
    private long cooldownMs;

    private final Map<String, Long> cooldownMap = new ConcurrentHashMap<>();

    private static final String GEMINI_MODEL_ID = "gemini-2.5-flash-image";
    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    private record VertexCredential(String projectId, String apiKey) {}

    static class NoImageGeneratedException extends RuntimeException {
        NoImageGeneratedException(String msg) { super(msg); }
    }

    @PostConstruct
    public void init() {
        credentials.clear();
        for (String raw : rawCredentials) {
            String[] parts = raw.split(":");
            if (parts.length == 2) {
                credentials.add(new VertexCredential(parts[0].trim(), parts[1].trim()));
            }
        }
        log.info("🔥 Vertex AI 로테이션 준비 완료: {}개의 프로젝트 계정 대기 중", credentials.size());
    }

    /** ✅ 재시도: 키가 여러 개이므로 대기 시간을 0.5초로 단축 (빠른 전환) */
    @Retryable(
            retryFor = { RestClientException.class },
            maxAttempts = 2,
            backoff = @Backoff(delay = 500)
    )
    public List<String> generateImageUrls(String prompt, Long userId, Object recipeId) {
        log.info("[GeminiImageService] 이미지 생성 요청 (Multi-Project Rotation), recipeId={}", recipeId);

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        try {
            Map<String, Object> body1 = buildRequestBody(prompt);
            Map<String, Object> resp1 = postWithKeyRotation(headers, body1, recipeId);
            return parseVertexResponse(resp1, userId, recipeId);

        } catch (Exception e) {
            log.warn("⚠️ 1차 생성 실패 (원인: {}) -> Safe Prompt로 재시도. recipeId={}", e.getMessage(), recipeId);

            try {
                String safePrompt = buildSafePrompt(prompt);
                Map<String, Object> body2 = buildRequestBody(safePrompt);
                Map<String, Object> resp2 = postWithKeyRotation(headers, body2, recipeId);
                return parseVertexResponse(resp2, userId, recipeId);
            } catch (Exception e2) {
                log.error("❌ 세이프 프롬프트도 실패. 기본 이미지 반환. recipeId={}, 원인={}", recipeId, e2.getMessage());
                return Collections.singletonList(DEFAULT_IMAGE_URL);
            }
        }
    }

    @Recover
    public List<String> recover(RestClientException e, String prompt, Long userId, Object recipeId) {
        log.error("❌ 모든 계정/리전 실패 (재시도 소진). 기본 이미지 반환. recipeId={}, error={}", recipeId, LogSanitizer.mask(e.getMessage()));
        return Collections.singletonList(DEFAULT_IMAGE_URL);
    }

    /** * ✅ 핵심 로직 수정: (리전 Loop) x (계정 Loop)
     * - [수정됨] 매 요청마다 계정 순서를 섞어서(Shuffle) 특정 계정만 429가 뜨는 현상 방지
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> postWithKeyRotation(HttpHeaders headers, Map<String, Object> body, Object recipeId) {
        RuntimeException lastException = null;

        List<VertexCredential> shuffledCredentials = new ArrayList<>(this.credentials);
        Collections.shuffle(shuffledCredentials);

        for (String loc : vertexLocations) {
            loc = loc.trim();
            if (loc.isEmpty()) continue;

            for (VertexCredential cred : shuffledCredentials) {
                String cacheKey = cred.projectId + "-" + loc;

                if (isCooldown(cacheKey)) continue;

                String url = buildUrl(loc, cred);

                try {
                    ResponseEntity<Map> response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);

                    log.info("✅ Gemini API 성공! (Project={}, Location={}, recipeId={})", cred.projectId, loc, recipeId);
                    return (Map<String, Object>) response.getBody();

                } catch (HttpStatusCodeException e) {
                    int code = e.getStatusCode().value();
                    String msg = shorten(e.getResponseBodyAsString());

                    if (code == 429) {
                        log.warn("⚠️ Quota 초과(429) -> Next Key! (Project={}, Location={}, msg={})", cred.projectId, loc, msg);
                        markCooldown(cacheKey);
                        lastException = e;
                        continue;
                    }

                    if (code >= 500) {
                        log.warn("⚠️ 서버 장애({}) -> Next Region! (Location={}, msg={})", code, loc, msg);
                        lastException = e;
                        break;
                    }

                    throw e;

                } catch (Exception e) {
                    log.warn("⚠️ 연결 오류 -> 재시도 (Project={}, Location={}, Msg={})", cred.projectId, loc, LogSanitizer.mask(e.getMessage()));
                    lastException = new RestClientException("Connection Error", e);
                }
            }
        }

        if (lastException != null) {
            if (lastException instanceof RestClientException re) throw re;
            throw new RestClientException("All Vertex calls failed", lastException);
        }
        throw new RestClientException("No available credentials or regions configured");
    }


    private String buildUrl(String loc, VertexCredential cred) {
        if (cred.apiKey.startsWith("AIza")) {
            return String.format("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
                    "gemini-2.5-flash-image",
                    cred.apiKey);
        }

        return "https://aiplatform.googleapis.com/v1/projects/" + cred.projectId
                + "/locations/" + loc
                + "/publishers/google/models/" + GEMINI_MODEL_ID
                + ":generateContent?key=" + cred.apiKey;
    }

    private boolean isCooldown(String key) {
        Long until = cooldownMap.get(key);
        return until != null && until > System.currentTimeMillis();
    }

    private void markCooldown(String key) {
        cooldownMap.put(key, System.currentTimeMillis() + cooldownMs);
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
        List<Map<String, String>> safetySettings = List.of(
                Map.of("category", "HARM_CATEGORY_HATE_SPEECH", "threshold", "BLOCK_ONLY_HIGH"),
                Map.of("category", "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold", "BLOCK_ONLY_HIGH"),
                Map.of("category", "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold", "BLOCK_ONLY_HIGH"),
                Map.of("category", "HARM_CATEGORY_HARASSMENT", "threshold", "BLOCK_ONLY_HIGH")
        );
        String enhancedPrompt = prompt + " , high quality, photorealistic food photography, 1:1 aspect ratio";
        return Map.of(
                "contents", List.of(Map.of("role", "user", "parts", List.of(Map.of("text", enhancedPrompt)))),
                "generationConfig", generationConfig,
                "safetySettings", safetySettings
        );
    }

    private String buildSafePrompt(String prompt) {
        return prompt
                + " , photorealistic plated food on a clean table, studio lighting"
                + " , no text, no logos, no people, no hands, no labels";
    }

    private int promptHash(String prompt) { return prompt == null ? 0 : prompt.hashCode(); }

    @SuppressWarnings("unchecked")
    private List<String> parseVertexResponse(Map<String, Object> responseBody, Long userId, Object recipeId) {
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
                    log.warn("⚠️ 이미지 생성 중단됨. recipeId={}, reason={}", recipeId, finishReason);
                }
            }

            Map<String, Object> content = (Map<String, Object>) candidate.get("content");
            if (content == null) continue;

            if (!content.containsKey("parts")) continue;
            List<Map<String, Object>> parts = (List<Map<String, Object>>) content.get("parts");
            if (parts == null) continue;

            for (Map<String, Object> part : parts) {
                if (part.containsKey("inlineData")) {
                    Map<String, Object> inline = (Map<String, Object>) part.get("inlineData");
                    Object data = inline.get("data");
                    if (data instanceof String base64) {
                        imageUrls.add(uploadOriginalToS3(base64, userId, recipeId));
                    }
                }
            }
        }

        if (imageUrls.isEmpty()) {
            throw new NoImageGeneratedException("이미지가 생성되지 않았습니다 (Safety Block 등).");
        }

        log.info("✅ Gemini 이미지 생성 완료: {}장 (recipeId={})", imageUrls.size(), recipeId);
        return imageUrls;
    }

    private String safeToString(Object o) {
        if (o == null) return "null";
        String s = String.valueOf(o);
        return s.length() > 600 ? s.substring(0, 600) + "..." : s;
    }

    private String uploadOriginalToS3(String base64, Long userId, Object recipeId) {
        byte[] bytes = Base64.getDecoder().decode(base64);

        String originalKey = String.format("original/images/recipes/%d/%s/main.jpg", userId, recipeId);
        String finalWebpKey = String.format("images/recipes/%d/%s/main.webp", userId, recipeId);

        s3Util.upload(bytes, originalKey, "image/jpeg");

        log.info("📤 원본 업로드 완료 (-> Lambda 변환 대기): {}", originalKey);
        log.info("🔗 DB 저장 예정 URL: {}", finalWebpKey);

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, finalWebpKey);
    }
}