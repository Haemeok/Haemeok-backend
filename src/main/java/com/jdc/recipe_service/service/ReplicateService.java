package com.jdc.recipe_service.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
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

        Map<String, Object> body = Map.of(
                "version", "haemeok/qwen3-recipe:latest",
                "input", Map.of("prompt", prompt)
        );

        HttpEntity<?> entity = new HttpEntity<>(body, headers);
        ResponseEntity<Map> response = restTemplate.postForEntity(
                "https://api.replicate.com/v1/predictions",
                entity,
                Map.class
        );

        String id = (String) response.getBody().get("id");
        String status;
        List<String> output;

        do {
            Thread.sleep(2000);
            ResponseEntity<Map> poll = restTemplate.exchange(
                    "https://api.replicate.com/v1/predictions/" + id,
                    HttpMethod.GET,
                    new HttpEntity<>(headers),
                    Map.class
            );
            status = (String) poll.getBody().get("status");
            output = (List<String>) poll.getBody().get("output");
        } while ("starting".equals(status) || "processing".equals(status));

        if (!"succeeded".equals(status)) {
            throw new RuntimeException("Replicate 실행 실패: " + status);
        }
        return output.get(0);
    }
}
