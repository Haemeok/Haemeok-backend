package com.jdc.recipe_service.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class NanoBananaImageService {

    private final RestTemplate restTemplate;

    @Value("${gemini.api-key}")
    private String geminiApiKey;

    private static final String IMAGEN_MODEL = "imagen-3.0-generate-002";
    private static final String IMAGEN_API_URL = "https://generativelanguage.googleapis.com/v1beta/models/{model}:predict?key={key}";

    @SuppressWarnings("unchecked")
    public List<String> generateImageUrls(String prompt, int n, String size) {

        // size(예: "1024x1024")를 API가 요구하는 aspectRatio 비율 문자열로 변환
        String aspectRatio;
        if (size.contains("1024x1024")) {
            aspectRatio = "1:1";
        } else {
            aspectRatio = "1:1";
        }

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        // predict 엔드포인트 요청 본문 구조
        Map<String, Object> body = Map.of(
                "instances", List.of(
                        Map.of("prompt", prompt)
                ),
                "parameters", Map.of(
                        "sampleCount", n,
                        "aspectRatio", aspectRatio
                )
        );

        String url = IMAGEN_API_URL.replace("{model}", IMAGEN_MODEL).replace("{key}", geminiApiKey);

        ResponseEntity<Map> response;
        try {
            response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);
        } catch (RestClientException e) {
            throw new RuntimeException("Imagen 이미지 생성 API 호출 실패: " + e.getMessage(), e);
        }

        if (response.getStatusCode() != HttpStatus.OK || response.getBody() == null) {
            throw new RuntimeException("Imagen 이미지 생성 API 응답 오류: HTTP " + response.getStatusCode());
        }

        Map<String, Object> responseBody = response.getBody();
        List<Map<String, Object>> predictions = (List<Map<String, Object>>) responseBody.get("predictions");

        if (predictions == null || predictions.isEmpty()) {
            throw new RuntimeException("Imagen 이미지 생성 API에서 생성된 이미지가 없습니다 (predictions 필드 확인 필요).");
        }

        // Base64 데이터를 추출하여 data URI 형식으로 변환
        return predictions.stream()
                .map(prediction -> (Map<String, Object>) prediction.get("image"))
                .map(image -> (String) image.get("imageBytes"))
                .map(b64Data -> "data:image/jpeg;base64," + b64Data)
                .collect(Collectors.toList());
    }
}