package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
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

    private static final String GCP_PROJECT_ID = "gen-lang-client-0326396795";
    private static final String GCP_REGION = "global";

    private static final String GEMINI_MODEL_ID = "gemini-3-pro-image-preview";

    private static final String VERTEX_GEMINI_URL =
            "https://aiplatform.googleapis.com/v1/projects/" + GCP_PROJECT_ID + "/locations/global/publishers/google/models/" + GEMINI_MODEL_ID + ":generateContent?key={key}";

    public List<String> generateImageUrls(String prompt, Long userId, Long recipeId) {
        log.info("[GeminiImageService] Vertex AI - Gemini 3 Pro (Final Attempt)");

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        Map<String, Object> imageConfig = Map.of(
                "aspectRatio", "1:1"
        );

        Map<String, Object> generationConfig = Map.of(
                "responseModalities", List.of("IMAGE"),
                "candidateCount", 1,
                "imageConfig", imageConfig
        );


        String enhancedPrompt = prompt + " , high quality, photorealistic food photography, 1:1 aspect ratio";

        Map<String, Object> body = Map.of(
                "contents", List.of(
                        Map.of("role", "user", "parts", List.of(Map.of("text", enhancedPrompt)))
                ),
                "generationConfig", generationConfig
        );

        String url = VERTEX_GEMINI_URL.replace("{key}", geminiApiKey);

        ResponseEntity<Map> response;
        try {
            response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);
        } catch (RestClientException e) {
            log.error("❌ Vertex AI 최종 호출 실패: {}", e.getMessage());
            throw new RuntimeException("Gemini 3 Pro Image API 호출 실패 (접근 권한 문제 유력)", e);
        }

        return parseVertexResponse(response.getBody(), userId, recipeId);
    }

    private List<String> parseVertexResponse(Map<String, Object> responseBody, Long userId, Long recipeId) {
        if (responseBody == null || !responseBody.containsKey("candidates")) {
            log.error("🚨 응답 오류: {}", responseBody);
            throw new RuntimeException("Gemini 응답에 candidates가 없습니다.");
        }

        List<Map<String, Object>> candidates = (List<Map<String, Object>>) responseBody.get("candidates");
        List<String> imageUrls = new ArrayList<>();

        for (Map<String, Object> candidate : candidates) {
            Map<String, Object> content = (Map<String, Object>) candidate.get("content");
            if (content == null || !content.containsKey("parts")) continue;

            List<Map<String, Object>> parts = (List<Map<String, Object>>) content.get("parts");

            for (Map<String, Object> part : parts) {
                if (part.containsKey("inlineData")) {
                    Map<String, Object> inlineData = (Map<String, Object>) part.get("inlineData");
                    String base64Data = (String) inlineData.get("data");
                    imageUrls.add(uploadOriginalToS3(base64Data, userId, recipeId));
                }
            }
        }

        if (imageUrls.isEmpty()) {
            throw new RuntimeException("❌ 이미지가 생성되지 않았습니다.");
        }

        log.info("✅ Gemini 3 Pro 이미지 생성 완료: {}장", imageUrls.size());
        return imageUrls;
    }

    /**
     * [Lambda Trigger용 업로드 메서드]
     * 1. 'original/' 폴더에 원본(JPG) 업로드 -> Lambda가 감지하고 동작함
     * 2. DB에는 'images/' 폴더의 WebP URL을 미리 반환 (Lambda가 변환해서 거기에 넣어둘 것이므로)
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

//    private String uploadToS3(String base64, Long userId, Long recipeId) {
//        byte[] bytes = Base64.getDecoder().decode(base64);
//        String s3Key = String.format("images/recipes/%d/%d/main.jpg", userId, recipeId);
//        s3Util.upload(bytes, s3Key);
//        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, s3Key);
//    }
}