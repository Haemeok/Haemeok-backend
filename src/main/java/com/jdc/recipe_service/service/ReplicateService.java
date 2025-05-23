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

    public String generateRecipeJson(String prompt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(apiToken);

        String url = "https://api.replicate.com/v1/deployments/haemeok/qwen3-recipe-deploy/predictions";

        Map<String, Object> body = Map.of(
                "input", Map.of("prompt", prompt)
        );

        try {
            System.out.println("📤 Replicate POST 요청 시작");

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
            Map<?, ?> urlsMap = (Map<?, ?>) responseBody.get("urls");
            if (urlsMap == null || urlsMap.get("get") == null) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 응답에서 get URL을 찾을 수 없습니다."
                );
            }
            String getUrl = (String) urlsMap.get("get");
            System.out.println("🆔 생성된 prediction id = " + predictionId);

            String status;
            String output = null;
            int pollCount = 0;
            int maxPolls = 180;

            Map<String, Object> pollBody = null;

            do {
                if (pollCount++ > maxPolls) {
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Replicate 처리 시간 초과 (폴링 횟수 제한 도달)"
                    );
                }
                Thread.sleep(2000);
                ResponseEntity<Map> poll = restTemplate.exchange(
                        getUrl,
                        HttpMethod.GET,
                        new HttpEntity<>(headers),
                        Map.class
                );
                pollBody = poll.getBody();
                if (pollBody == null) {
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Replicate 폴링 응답이 비어있습니다."
                    );
                }
                status = (String) pollBody.get("status");

                if (pollBody.get("output") != null) {
                    if (pollBody.get("output") instanceof List) {
                        List<?> outputList = (List<?>) pollBody.get("output");
                        if (!outputList.isEmpty() && outputList.get(0) instanceof String) {
                            output = (String) outputList.get(0);
                        }
                    } else if (pollBody.get("output") instanceof String) {
                        output = (String) pollBody.get("output");
                    }
                }

                System.out.println("🔄 status = " + status);
                System.out.println("📦 output = " + output);
            } while ("starting".equals(status) || "processing".equals(status));

            if (!"succeeded".equals(status)) {
                Object errorDetails = pollBody != null ? pollBody.get("error") : "N/A";
                System.err.println("Replicate 실행 실패. Status: " + status + ", Error: " + errorDetails);
                Object replicateLogs = pollBody != null ? pollBody.get("logs") : "N/A";
                if (replicateLogs instanceof String && !((String) replicateLogs).isEmpty()) {
                    System.err.println("Replicate Logs: " + replicateLogs);
                }
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 실행 실패: " + status + (errorDetails != null ? " - " + errorDetails.toString() : "")
                );
            }

            return output;

        } catch (RestClientException e) {
            e.printStackTrace();
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate API 호출 실패: " + e.getMessage()
            );
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            e.printStackTrace();
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate 폴링 중 인터럽트 발생: " + e.getMessage()
            );
        }
    }
}