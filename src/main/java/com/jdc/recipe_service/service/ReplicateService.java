package com.jdc.recipe_service.service; // 사용자 환경에 맞게 패키지명 수정

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.exception.CustomException; // 사용자 정의 예외
import com.jdc.recipe_service.exception.ErrorCode;       // 사용자 정의 에러 코드
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList; // 추가
import java.util.List;      // 추가
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
public class ReplicateService {

    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper(); // JSON 유효성 검사용

    @Value("${REPLICATE_TOKEN}")
    private String apiToken;

    public String generateRecipeJson(String prompt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(apiToken);

        String url = "https://api.replicate.com/v1/deployments/haemeok/qwen3-recipe-deploy/predictions";

        // Temperature 값을 0.0, do_sample을 false로 설정하여 AI 응답의 일관성 극대화 시도
        Map<String, Object> body = Map.of(
                "input", Map.of(
                        "prompt", prompt,
                        "max_new_tokens", 1200,
                        "temperature", 0.0,   // 결정론적 출력을 위해 0.0으로 설정
                        // "top_p", 0.9,      // temperature가 0이면 top_p, top_k는 보통 무시됨
                        "do_sample", false    // temperature가 0일 때 false로 설정
                )
        );

        try {
            System.out.println("📤 Replicate POST 요청 시작");
            // System.out.println("Prompt: " + prompt); // 필요시 프롬프트 로깅

            ResponseEntity<Map> response = restTemplate.postForEntity(
                    url,
                    new HttpEntity<>(body, headers),
                    Map.class
            );

            Map<String, Object> responseBody = response.getBody();
            if (responseBody == null || !responseBody.containsKey("id")) {
                System.err.println("Replicate 응답 비정상 (id 없음): " + response.getStatusCode() + " | Body: " + responseBody);
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 응답이 비정상적입니다 (id 없음): " + response.getStatusCode()
                );
            }

            String predictionId = (String) responseBody.get("id");
            String getUrl = (String) ((Map<?, ?>) responseBody.get("urls")).get("get");
            System.out.println("🆔 생성된 prediction id = " + predictionId + ", GET URL: " + getUrl);

            String status;
            String output = null;
            int pollCount = 0;
            int maxPolls = 600;

            Map<String, Object> pollBody = null;
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
                    System.err.println("Replicate 폴링 응답 비어있음. ID: " + predictionId);
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Replicate 폴링 응답이 비어있습니다 (ID: " + predictionId + ")"
                    );
                }
                status = (String) pollBody.get("status");

                Object outObj = pollBody.get("output");
                if (outObj instanceof List<?> list && !list.isEmpty()) {
                    StringBuilder fullOutput = new StringBuilder();
                    for (Object segment : list) {
                        fullOutput.append(String.valueOf(segment));
                    }
                    output = fullOutput.toString();
                } else if (outObj instanceof String str) {
                    output = str;
                }
            } while ("starting".equals(status) || "processing".equals(status));

            if (!"succeeded".equals(status) || output == null) {
                Object errorDetails = (pollBody != null) ? pollBody.getOrDefault("error", "Unknown error from Replicate") : "Poll body was null";
                System.err.println("Replicate 실행 실패. ID: " + predictionId + ", Status: " + status + ", Error: " + errorDetails + ", Output: " + output);
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Replicate 실행 실패 (ID: " + predictionId + "): " + status + " - " + errorDetails
                );
            }

            System.out.println("ReplicateService - AI 원본 응답 전체 (추출 전) (ID: " + predictionId + "): [\n" + output + "\n]");
            return extractSingleValidJsonObject(output, predictionId);

        } catch (RestClientException e) {
            System.err.println("Replicate API 호출 실패: " + e.getMessage());
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate API 호출 실패: " + e.getMessage()
            );
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println("Replicate 폴링 중 인터럽트 발생: " + e.getMessage());
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Replicate 폴링 중 인터럽트 발생: " + e.getMessage()
            );
        }
    }

    /**
     * AI 응답에서 유효한 단일 JSON 객체 (특히 "title" 필드 포함)를 추출합니다.
     * 여러 JSON 객체가 섞여있거나 코드 블록이 포함된 경우에도 처리하려고 시도합니다.
     */
    private String extractSingleValidJsonObject(String rawAiResponse, String predictionIdForLog) {
        if (rawAiResponse == null || rawAiResponse.trim().isEmpty()) {
            System.err.println("Extract (ID: " + predictionIdForLog + ") - Raw AI response is null or empty.");
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답이 비어있습니다.");
        }
        System.out.println("Extract (ID: " + predictionIdForLog + ") - Attempting to extract JSON from: [\n" + rawAiResponse + "\n]");

        List<String> potentialJsonStrings = new ArrayList<>();

        // 1순위: ```json ... ``` 코드 블록 내부 검색
        Pattern codeBlockPattern = Pattern.compile("```json\\s*(\\{.*?\\})\\s*```", Pattern.DOTALL);
        Matcher codeBlockMatcher = codeBlockPattern.matcher(rawAiResponse);
        while (codeBlockMatcher.find()) {
            potentialJsonStrings.add(codeBlockMatcher.group(1).trim());
        }

        // 2순위: 코드 블록 외의 텍스트에서도 일반 { ... } 블록 찾기
        // 코드 블록 마커를 제거한 텍스트에서 추가적인 JSON 객체를 찾음
        String textWithoutCodeFence = rawAiResponse;
        // 이미 ```json ... ``` 패턴으로 추출된 부분은 다시 찾지 않도록 간단히 마커만 제거할 수 있으나,
        // 중복 후보가 생기더라도 이후 필터링되므로, 전체 텍스트에서 찾는 것을 유지하거나,
        // 아래 로직을 코드 블록이 없을 때만 실행하도록 할 수도 있음.
        // 여기서는 모든 가능성을 열어두고 후보를 수집.
        if (potentialJsonStrings.isEmpty()) { // 코드블록에서 아무것도 못찾은 경우에만 전체 텍스트에서 재시도
            textWithoutCodeFence = rawAiResponse.replaceAll("```json", "").replaceAll("```", "").trim();
        }


        int searchStartIndex = 0;
        while(searchStartIndex < textWithoutCodeFence.length()) {
            int braceStartIndex = textWithoutCodeFence.indexOf('{', searchStartIndex);
            if (braceStartIndex == -1) break;

            int braceCount = 0;
            int braceEndIndex = -1;
            boolean inString = false;

            for (int i = braceStartIndex; i < textWithoutCodeFence.length(); i++) {
                char c = textWithoutCodeFence.charAt(i);
                if (c == '\\') {
                    i++;
                    continue;
                }
                if (c == '"') {
                    inString = !inString;
                }
                if (!inString) {
                    if (c == '{') {
                        braceCount++;
                    } else if (c == '}') {
                        braceCount--;
                        if (braceCount == 0 && i >= braceStartIndex) {
                            braceEndIndex = i;
                            String foundJsonBlock = textWithoutCodeFence.substring(braceStartIndex, braceEndIndex + 1).trim();
                            // 이미 코드 블록에서 찾은 것과 동일한 내용이면 중복 추가 방지 (선택적 최적화)
                            if (!potentialJsonStrings.contains(foundJsonBlock)) {
                                potentialJsonStrings.add(foundJsonBlock);
                            }
                            searchStartIndex = braceEndIndex + 1;
                            break;
                        }
                    }
                }
            }
            if (braceEndIndex == -1) {
                searchStartIndex = braceStartIndex + 1; // 못찾았으면 다음 '{'부터 다시 시작 (무한루프 방지)
            }
        }

        if (potentialJsonStrings.isEmpty() && rawAiResponse.trim().startsWith("{") && rawAiResponse.trim().endsWith("}")) {
            // 다른 방법으로 후보를 못찾았지만, 전체 응답 자체가 하나의 JSON 객체 형태일 수 있음
            potentialJsonStrings.add(rawAiResponse.trim());
        }


        // 찾은 모든 잠재적 JSON 문자열 중에서 "title"을 포함하고 유효한 첫 번째 것을 선택
        for (String jsonCandidate : potentialJsonStrings) {
            System.out.println("Extract (ID: " + predictionIdForLog + ") - Checking candidate: [" + (jsonCandidate.length() > 200 ? jsonCandidate.substring(0, 200) + "..." : jsonCandidate) + "]");
            if (jsonCandidate.contains("\"title\"") && isValidJson(jsonCandidate, predictionIdForLog)) {
                System.out.println("Extract (ID: " + predictionIdForLog + ") - Valid JSON with 'title' found. Returning this.");
                return jsonCandidate;
            } else {
                System.out.println("Extract (ID: " + predictionIdForLog + ") - Candidate invalid or missing 'title'.");
            }
        }

        System.err.println("Extract (ID: " + predictionIdForLog + ") - Failed to extract a valid JSON object with 'title' from any candidates in AI response.");
        String sampleResponse = rawAiResponse.length() > 500 ? rawAiResponse.substring(0, 500) + "..." : rawAiResponse;
        throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED,
                "AI 응답에서 유효한 레시피 JSON 객체를 추출하지 못했습니다 (ID: " + predictionIdForLog + "). 응답 샘플: " + sampleResponse);
    }

    /**
     * 주어진 문자열이 유효한 JSON 형식인지 검사합니다.
     */
    private boolean isValidJson(String jsonString, String predictionIdForLog) {
        if (jsonString == null || jsonString.trim().isEmpty() || !jsonString.trim().startsWith("{") || !jsonString.trim().endsWith("}")) {
            return false;
        }
        try {
            objectMapper.readTree(jsonString);
            return true;
        } catch (JsonProcessingException e) {
            // System.out.println("Extract (ID: " + predictionIdForLog + ") - Invalid JSON detected: " + e.getMessage() + " | JSON: [" + jsonString +"]"); // 상세 로깅
            return false;
        }
    }
}