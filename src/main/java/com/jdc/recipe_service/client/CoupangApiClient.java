package com.jdc.recipe_service.client;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.util.HmacGenerator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@Component
@RequiredArgsConstructor
@Slf4j
public class CoupangApiClient {

    private final RestTemplate restTemplate;
    private final HmacGenerator hmacGenerator;
    private final ObjectMapper objectMapper;

    @Value("${coupang.api.access-key}")
    private String accessKey;

    @Value("${coupang.api.secret-key}")
    private String secretKey;

    @Value("${coupang.api.sub-id}")
    private String subId;

    private static final String DOMAIN = "https://api-gateway.coupang.com";
    private static final String SEARCH_PATH = "/v2/providers/affiliate_open_api/apis/openapi/products/search";
    private static final String DEEP_LINK_PATH = "/v2/providers/affiliate_open_api/apis/openapi/deeplink";

    public String searchLandingUrl(String keyword) {
        JsonNode root = callSearchApi(keyword);
        if (root == null) return null;

        return root.path("data").path("landingUrl").asText(null);
    }

    public String searchProductUrl(String keyword) {
        JsonNode root = callSearchApi(keyword);
        if (root == null) return null;

        JsonNode productData = root.path("data").path("productData");

        if (productData.isArray() && productData.size() > 0) {
            return productData.get(0).path("productUrl").asText(null);
        }

        return root.path("data").path("landingUrl").asText(null);
    }

    public String createDeepLink(String rawUrl) {
        try {
            String uriForHmac = DEEP_LINK_PATH;
            String fullUrlStr = DOMAIN + uriForHmac;

            URI finalUri = URI.create(fullUrlStr);

            Map<String, Object> body = new HashMap<>();
            body.put("coupangUrls", Collections.singletonList(rawUrl));
            body.put("subId", subId);
            String jsonBody = objectMapper.writeValueAsString(body);

            String authorization = hmacGenerator.generate("POST", uriForHmac, secretKey, accessKey);

            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", authorization);
            headers.set("Content-Type", "application/json");

            ResponseEntity<String> response = restTemplate.exchange(
                    finalUri, HttpMethod.POST, new HttpEntity<>(jsonBody, headers), String.class
            );

            JsonNode root = objectMapper.readTree(response.getBody());

            if (!isSuccess(root)) {
                logError(root, rawUrl);
                return null;
            }

            JsonNode data = root.path("data");
            if (data.isArray() && data.size() > 0) {
                return data.get(0).path("shortenUrl").asText(null);
            }
            return null;

        } catch (Exception e) {
            log.error("딥링크 API 에러: {}", rawUrl, e);
            return null;
        }
    }

    private JsonNode callSearchApi(String keyword) {
        try {
            String encodedKeyword = URLEncoder.encode(keyword, StandardCharsets.UTF_8);

            String encodedSubId = URLEncoder.encode(subId, StandardCharsets.UTF_8);

            String queryParams = String.format("keyword=%s&limit=1&subId=%s", encodedKeyword, encodedSubId);

            String uriForHmac = SEARCH_PATH + "?" + queryParams;
            String fullUrlStr = DOMAIN + uriForHmac;

            URI finalUri = URI.create(fullUrlStr);

            String authorization = hmacGenerator.generate("GET", uriForHmac, secretKey, accessKey);

            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", authorization);
            headers.set("Content-Type", "application/json");

            ResponseEntity<String> response = restTemplate.exchange(
                    finalUri, HttpMethod.GET, new HttpEntity<>(headers), String.class
            );

            JsonNode root = objectMapper.readTree(response.getBody());

            if (!isSuccess(root)) {
                logError(root, keyword);
                return null;
            }
            return root;

        } catch (Exception e) {
            log.error("검색 API 에러 keyword={}", keyword, e);
            return null;
        }
    }

    private boolean isSuccess(JsonNode root) {
        String rCode = root.path("rCode").asText("");
        return "0".equals(rCode);
    }

    private void logError(JsonNode root, String key) {
        String rCode = root.path("rCode").asText("");
        String code = root.path("code").asText("");

        String msg = root.has("rMessage") ? root.path("rMessage").asText() : root.path("message").asText();
        String finalCode = rCode.isEmpty() ? code : rCode;

        log.warn("API 응답 실패 [Target: {}] code={}, msg={}", key, finalCode, msg);
    }
}