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

        Map<String, Object> body = Map.of(
                "model", "gpt-image-1",
                "prompt", prompt,
                "n", n,
                "size", size,
                "response_format", "b64_json",
                "quality", "low"
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

        List<Map<String, String>> dataList = (List<Map<String, String>>) response.getBody().get("data");
        return dataList.stream()
                .map(item -> "data:image/png;base64," + item.get("b64_json"))
                .toList();
    }
}
