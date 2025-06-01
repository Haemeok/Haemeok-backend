package com.jdc.recipe_service.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Value("${REPLICATE_TOKEN}")
    private String apiToken;

    public String generateRecipeJsonWithRetry(String prompt) {
        int retries = 2;
        for (int attempt = 1; attempt <= retries; attempt++) {
            try {
                return generateRecipeJson(prompt);
            } catch (CustomException e) {
                System.err.println("Attempt " + attempt + " failed: " + e.getMessage());
                if (attempt == retries) {
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "AI 생성에 실패했습니다. 잠시 후 다시 시도해 주세요."
                    );
                }
                try { Thread.sleep(2000); } catch (InterruptedException ie) { Thread.currentThread().interrupt(); }
            }
        }
        throw new CustomException(
                ErrorCode.AI_RECIPE_GENERATION_FAILED,
                "알 수 없는 오류로 모든 시도가 실패했습니다."
        );
    }

    private String generateRecipeJson(String prompt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(apiToken);

        Map<String, Object> body = Map.of(
                "input", Map.of(
                        "prompt", prompt,
                        "max_new_tokens", 1200,
                        "temperature", 0.0,
                        "do_sample", false,
                        "enable_thinking", true
                )
        );

        String url = "https://api.replicate.com/v1/deployments/haemeok/qwen3-recipe-deploy/predictions";

        try {
            System.out.println("📤 Replicate POST 요청 시작...");
            ResponseEntity<Map> response = restTemplate.postForEntity(
                    url,
                    new HttpEntity<>(body, headers),
                    Map.class
            );

            Map<String, Object> responseBody = response.getBody();
            if (responseBody == null || !responseBody.containsKey("id")) {
                System.err.println("Replicate 응답이 비정상적입니다: " + response.getStatusCode() + " | Body: " + responseBody);
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 응답이 비정상적입니다 (ID 없음)"
                );
            }

            String predictionId = (String) responseBody.get("id");
            String getUrl = (String) ((Map<?, ?>) responseBody.get("urls")).get("get");
            System.out.println("🆔 생성된 prediction id = " + predictionId + ", GET URL: " + getUrl);

            String status;
            String output = null;
            int pollCount = 0;
            int maxPolls = 600;
            Map<String, Object> pollBody;

            do {
                if (pollCount++ > maxPolls) {
                    System.err.println("Replicate 처리 시간 초과. ID: " + predictionId);
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Replicate 처리 시간 초과 (ID: " + predictionId + ")"
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
                    System.err.println("Replicate 폴링 응답이 null. ID: " + predictionId);
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Replicate 폴링 응답 null (ID: " + predictionId + ")"
                    );
                }
                status = (String) pollBody.get("status");

                Object outObj = pollBody.get("output");
                if (outObj instanceof List<?> list && !list.isEmpty()) {
                    StringBuilder sb = new StringBuilder();
                    for (Object segment : list) {
                        sb.append(String.valueOf(segment));
                    }
                    output = sb.toString();
                } else if (outObj instanceof String str) {
                    output = str;
                }
            } while ("starting".equals(status) || "processing".equals(status));

            if (!"succeeded".equals(status) || output == null) {
                Object errorDetails = pollBody.getOrDefault("error", "Unknown error");
                System.err.println("Replicate 실행 실패. ID=" + predictionId + ", Status=" + status + ", Error=" + errorDetails + ", Output=" + output);
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 실행 실패 (ID: " + predictionId + "), 상태=" + status
                );
            }

            System.out.println("📥 Replicate 전체 응답 (ID: " + predictionId + "):\n" + output);
            return extractSingleValidJsonObject(output, predictionId);

        } catch (RestClientException e) {
            System.err.println("Replicate API 호출 실패: " + e.getMessage());
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate API 호출 실패: " + e.getMessage()
            );
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println("Replicate 폴링 인터럽트: " + e.getMessage());
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate 폴링 인터럽트: " + e.getMessage()
            );
        }
    }

    private String extractSingleValidJsonObject(String rawAiResponse, String predictionIdForLog) {
        if (rawAiResponse == null || rawAiResponse.trim().isEmpty()) {
            System.err.println("Extract(ID: " + predictionIdForLog + ") - 응답이 비어있습니다.");
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답이 비어있습니다.");
        }
        System.out.println("Extract(ID: " + predictionIdForLog + ") - 원본 응답:\n" + rawAiResponse);

        String[] lines = rawAiResponse.split("\\r?\\n");
        StringBuilder jsonBuilder = new StringBuilder();
        boolean inJson = false;
        int braceCount = 0;

        for (String line : lines) {
            String trimmed = line.trim();

            if (!inJson && trimmed.startsWith("{")) {
                inJson = true;
            }

            if (inJson) {
                jsonBuilder.append(line).append("\n");
                for (char c : line.toCharArray()) {
                    if (c == '{') braceCount++;
                    else if (c == '}') braceCount--;
                }
                if (braceCount == 0) {
                    String candidate = jsonBuilder.toString().trim();
                    if (candidate.contains("\"title\"") && isValidJson(candidate)) {
                        System.out.println("Extract(ID: " + predictionIdForLog + ") - 중괄호 균형으로 JSON 추출됨.");
                        return candidate;
                    }
                    inJson = false;
                    braceCount = 0;
                    jsonBuilder.setLength(0);
                }
            }
        }

        System.err.println("Extract(ID: " + predictionIdForLog + ") - 유효한 JSON 객체를 찾지 못했습니다.");
        String sample = rawAiResponse.length() > 200 ? rawAiResponse.substring(0, 200) + "..." : rawAiResponse;
        throw new CustomException(
                ErrorCode.AI_RECIPE_GENERATION_FAILED,
                "유효한 레시피 JSON을 추출하지 못했습니다 (ID: " + predictionIdForLog + "). 응답 예시: " + sample
        );
    }

    /**
     * Jackson을 이용해 JSON 유효성을 검증합니다.
     */
    private boolean isValidJson(String jsonString) {
        if (jsonString == null) return false;
        String trimmed = jsonString.trim();
        if (!trimmed.startsWith("{") || !trimmed.endsWith("}")) return false;
        try {
            objectMapper.readTree(jsonString);
            return true;
        } catch (JsonProcessingException e) {
            return false;
        }
    }
}
