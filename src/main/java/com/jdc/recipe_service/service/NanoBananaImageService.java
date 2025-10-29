package com.jdc.recipe_service.service;

import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.client.RestClientException;

import java.util.*;
import java.util.Base64;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
@Slf4j
public class NanoBananaImageService {

    private final RestTemplate restTemplate;
    private final S3Util s3Util;

    @Value("${gemini.api-key}")
    private String geminiApiKey;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private static final String GCP_PROJECT_ID = "gen-lang-client-0326396795";
    private static final String GCP_REGION = "us-central1";

    private static final int MAX_IMAGES_PER_REQUEST = 1;

    private static final String IMAGEN_MODEL_FULL = "imagen-3.0-generate-002";
    private static final String IMAGEN_API_URL =
            "https://" + GCP_REGION + "-aiplatform.googleapis.com/v1/projects/" + GCP_PROJECT_ID + "/locations/" + GCP_REGION + "/publishers/google/models/" + IMAGEN_MODEL_FULL + ":predict?key={key}";

    @SuppressWarnings("unchecked")
    public List<String> generateImageUrls(String prompt, Long userId, Long recipeId) {
        log.info("[NanoBananaImageService] Vertex AI Imagen API 이미지 생성 시작 – prompt={}", prompt);

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        Map<String, Object> parameters = new HashMap<>();
        parameters.put("number_of_images", Math.min(1, MAX_IMAGES_PER_REQUEST));
        parameters.put("aspect_ratio", "1:1");
        parameters.put("sampleCount", 1);

        Map<String, Object> body = Map.of(
                "instances", List.of(
                        Map.of("prompt", prompt)
                ),
                "parameters", parameters
        );

        String url = IMAGEN_API_URL.replace("{key}", geminiApiKey);

        ResponseEntity<Map> response;
        try {
            response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);
        } catch (RestClientException e) {
            log.error("❌ Vertex AI REST 호출 실패: {}", e.getMessage(), e);
            throw new RuntimeException("❌ Vertex AI REST 호출 실패", e);
        }

        if (response.getStatusCode() != HttpStatus.OK || response.getBody() == null) {
            throw new RuntimeException("⚠️ Vertex AI 응답 오류: HTTP " + response.getStatusCode());
        }

        Map<String, Object> responseBody = response.getBody();
        log.debug("✅ Vertex AI Raw Response: {}", responseBody);

        if (responseBody.containsKey("error")) {
            Map<String, Object> errorMap = (Map<String, Object>) responseBody.get("error");
            String errorMessage = (String) errorMap.getOrDefault("message", "상세 메시지 없음");
            int errorCode = (int) errorMap.getOrDefault("code", 0);

            throw new RuntimeException(String.format("🚨 Vertex AI API 에러 응답 (Code %d): %s", errorCode, errorMessage));
        }

        List<Map<String, Object>> predictions = (List<Map<String, Object>>) responseBody.get("predictions");

        if (predictions == null || predictions.isEmpty()) {
            throw new RuntimeException("⚠️ 이미지 생성 실패: predictions 필드 없음");
        }

        List<String> dataUris = predictions.stream()
                .map(pred -> (String) pred.get("bytesBase64Encoded"))
                .filter(Objects::nonNull)
                .map(base64 -> {
                    byte[] bytes = Base64.getDecoder().decode(base64);
                    String s3Key = String.format("images/recipes/%d/%d/main.jpg", userId, recipeId);
                    s3Util.upload(bytes, s3Key);
                    return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, s3Key);
                })
                .collect(Collectors.toList());

        if (dataUris.isEmpty()) {
            throw new RuntimeException("❌ 이미지 생성 실패: Base64 데이터가 추출되지 않았습니다.");
        }

        log.info("✅ Vertex AI 이미지 생성 완료 – {}개 생성됨", dataUris.size());
        return dataUris;
    }
}