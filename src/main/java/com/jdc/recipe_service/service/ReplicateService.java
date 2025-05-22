package com.jdc.recipe_service.service;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
public class ReplicateService {

    private final RestTemplate restTemplate;

    @Value("${REPLICATE_TOKEN}")
    private String apiToken;

    public String generateRecipeJson(String prompt) throws InterruptedException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(apiToken);

        // ✅ Deployment 기반 endpoint
        String url = "https://api.replicate.com/v1/deployments/haemeok/qwen3-recipe-deploy/predictions";

        Map<String, Object> body = Map.of(
                "input", Map.of("prompt", prompt)
        );

        try {
            System.out.println("📤 Replicate POST 요청 시작");

            // 1) 모델 실행 요청
            ResponseEntity<Map> response = restTemplate.postForEntity(
                    url,
                    new HttpEntity<>(body, headers),
                    Map.class
            );

            Map<String, Object> responseBody = response.getBody();
            if (responseBody == null || !responseBody.containsKey("id")) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 응답이 비정상적입니다: " + response.getStatusCode()
                );
            }

            String predictionId = (String) responseBody.get("id");
            String getUrl = (String) ((Map<?, ?>) responseBody.get("urls")).get("get");
            System.out.println("🆔 생성된 prediction id = " + predictionId);

            // 2) 상태 폴링
            String status;
            List<String> output = null;
            do {
                Thread.sleep(2000);
                ResponseEntity<Map> poll = restTemplate.exchange(
                        getUrl,
                        HttpMethod.GET,
                        new HttpEntity<>(headers),
                        Map.class
                );
                Map<String, Object> pollBody = poll.getBody();
                status = (String) pollBody.get("status");
                output = (List<String>) pollBody.get("output");

                System.out.println("🔄 status = " + status);
                System.out.println("📦 output = " + output);
            } while ("starting".equals(status) || "processing".equals(status));

            // 3) 실패 시 CustomException 으로 던지기
            if (!"succeeded".equals(status)) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 실행 실패: " + status
                );
            }

            return output != null && !output.isEmpty() ? output.get(0) : null;

        } catch (RestClientException e) {
            e.printStackTrace();
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate API 호출 실패: " + e.getMessage()
            );
        }
    }
}
