package com.jdc.recipe_service.service.product;

import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.entity.user.UserSubscription;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.repository.user.UserSubscriptionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class SubscriptionService {

    private final UserSubscriptionRepository userSubscriptionRepository;
    private final RestTemplate restTemplate;
    private final CreditProductRepository creditProductRepository;


    @Value("${lemonsqueezy.api-key}")
    private String lemonApiKey;

    public void upgradeSubscription(Long userId, String newVariantUuid) {

        UserSubscription sub = userSubscriptionRepository.findByUserId(userId)
                .orElseThrow(() -> new IllegalArgumentException("구독 정보가 없습니다."));

        CreditProduct targetProduct = creditProductRepository.findByLemonSqueezyVariantUuid(newVariantUuid)
                .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 상품입니다."));

        Long targetNumericId = targetProduct.getLemonSqueezyVariantId();

        if (Objects.equals(String.valueOf(targetNumericId), sub.getLemonSqueezyVariantId())) {
            throw new IllegalArgumentException("이미 해당 플랜을 이용 중입니다.");
        }

        String url = "https://api.lemonsqueezy.com/v1/subscriptions/" + sub.getLemonSqueezySubscriptionId();

        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", "Bearer " + lemonApiKey);
        headers.set("Content-Type", "application/vnd.api+json");
        headers.set("Accept", "application/vnd.api+json");

        Map<String, Object> attributes = new HashMap<>();
        attributes.put("variant_id", targetNumericId);
        attributes.put("invoice_immediately", true);
        attributes.put("disable_prorations", false);

        Map<String, Object> data = new HashMap<>();
        data.put("type", "subscriptions");
        data.put("id", String.valueOf(sub.getLemonSqueezySubscriptionId()));
        data.put("attributes", attributes);

        Map<String, Object> body = new HashMap<>();
        body.put("data", data);

        try {
            log.info("구독 업그레이드 요청: User={}, NewVariant={}", userId, targetNumericId);
            restTemplate.exchange(url, HttpMethod.PATCH, new HttpEntity<>(body, headers), String.class);
        } catch (Exception e) {
            log.error("업그레이드 API 호출 실패", e);
            throw new RuntimeException("업그레이드 실패: " + e.getMessage());
        }
    }
}