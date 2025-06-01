package com.jdc.recipe_service.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class GptImageService {

    private final RestTemplate restTemplate;

    @Value("${openai.api-key}")
    private String openAiApiKey;

    @SuppressWarnings("unchecked")
    public List<String> generateImageUrls(String prompt, int n, String size) {
        String url = "https://api.openai.com/v1/images/generations";

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(openAiApiKey);

        // ★ model: "gpt-image-1" 항목을 추가합니다.
        Map<String, Object> body = Map.of(
                "model", "dall-e-3",
                "prompt", prompt,
                "n", n,
                "size", size
        );

        ResponseEntity<Map> response;
        try {
            response = restTemplate.postForEntity(url, new HttpEntity<>(body, headers), Map.class);
        } catch (RestClientException e) {
            throw new RuntimeException("OpenAI 이미지 생성 API 호출 실패: " + e.getMessage(), e);
        }

        if (response.getStatusCode() != HttpStatus.OK || response.getBody() == null) {
            throw new RuntimeException("OpenAI 이미지 생성 API 응답 오류: HTTP " + response.getStatusCode());
        }

        Map<String, Object> respBody = response.getBody();
        if (!respBody.containsKey("data")) {
            throw new RuntimeException("OpenAI 이미지 생성 응답 포맷 오류: data 필드가 없습니다.");
        }

        List<Map<String, Object>> dataList = (List<Map<String, Object>>) respBody.get("data");
        return dataList.stream()
                .map(item -> (String) item.get("url"))
                .toList();
    }
}
