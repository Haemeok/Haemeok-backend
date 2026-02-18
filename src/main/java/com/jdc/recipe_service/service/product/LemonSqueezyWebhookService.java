package com.jdc.recipe_service.service.product;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.user.UserSubscription;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.repository.user.UserCreditRepository;
import com.jdc.recipe_service.domain.repository.user.UserSubscriptionRepository;
import com.jdc.recipe_service.domain.type.product.SubscriptionStatus;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.user.UserCreditService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hashids.Hashids;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Formatter;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class LemonSqueezyWebhookService {

    private final UserCreditService userCreditService;
    private final UserRepository userRepository;
    private final CreditProductRepository creditProductRepository;
    private final UserCreditRepository userCreditRepository;
    private final UserSubscriptionRepository userSubscriptionRepository;
    private final ObjectMapper objectMapper;
    private final Hashids hashids;

    @Value("${lemonsqueezy.webhook-secret:}")
    private String WEBHOOK_SECRET;

    public void validateWebhookSignature(String signature, String payload) {
        if (signature == null || signature.isBlank()) {
            throw new CustomException(ErrorCode.WEBHOOK_VERIFICATION_FAILED, "Signature missing");
        }
        try {
            Mac hmacSha256 = Mac.getInstance("HmacSHA256");
            hmacSha256.init(new SecretKeySpec(WEBHOOK_SECRET.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            String calculated = toHexString(hmacSha256.doFinal(payload.getBytes(StandardCharsets.UTF_8)));

            if (!Objects.equals(signature, calculated)) {
                throw new CustomException(ErrorCode.WEBHOOK_VERIFICATION_FAILED, "Invalid Signature");
            }
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            throw new CustomException(ErrorCode.WEBHOOK_HANDLING_FAILED, "Signature verification error");
        }
    }

    private String toHexString(byte[] bytes) {
        Formatter formatter = new Formatter();
        for (byte b : bytes) formatter.format("%02x", b);
        return formatter.toString();
    }

    @Transactional
    public void handleWebhookEvent(String eventName, String payload) {
        try {
            JsonNode root = objectMapper.readTree(payload);
            JsonNode data = root.path("data");
            JsonNode attributes = data.path("attributes");

            JsonNode customData = root.path("meta").path("custom_data");
            String encodedUserId = customData.path("user_id").asText();
            if (encodedUserId == null || encodedUserId.isBlank() || "0".equals(encodedUserId)) {
                log.warn("⚠️ Webhook Skipped: user_id not found in custom_data");
                return;
            }

            long[] decoded = hashids.decode(encodedUserId);
            if (decoded.length == 0) {
                log.error("❌ 잘못된 HashID가 전달됨: {}", encodedUserId);
                throw new CustomException(ErrorCode.USER_NOT_FOUND);
            }
            Long userId = decoded[0];

            User user = userRepository.findById(userId)
                    .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

            long variantId = attributes.path("variant_id").asLong();
            CreditProduct product = creditProductRepository.findByLemonSqueezyVariantId(variantId)
                    .orElseThrow(() -> {
                        log.error("❌ 등록되지 않은 상품 결제됨! VariantID: {}", variantId);
                        return new CustomException(ErrorCode.INVALID_PAYMENT_PRODUCT);
                    });

            String orderId = data.path("id").asText();
            String status = attributes.path("status").asText();

            if (userCreditRepository.findByTransactionId(orderId).isPresent()) {
                log.info("♻️ 이미 처리된 웹훅입니다. (OrderId={})", orderId);
                return;
            }

            if (("subscription_created".equals(eventName) || "subscription_payment_success".equals(eventName))
                    && product.getType() == CreditType.SUBSCRIPTION) {

                String portalUrl = attributes.path("urls").path("customer_portal").asText();
                String renewsAtStr = attributes.path("renews_at").asText();
                long subId = attributes.path("subscription_id").asLong();

                LocalDateTime nextBillingDate;
                if (renewsAtStr != null && !renewsAtStr.isBlank()) {
                    nextBillingDate = LocalDateTime.ofInstant(Instant.parse(renewsAtStr), ZoneId.systemDefault());
                } else {
                    nextBillingDate = LocalDateTime.now().plusDays(product.getValidDays());
                }

                UserSubscription subscription = userSubscriptionRepository.findByUserId(user.getId())
                        .orElse(UserSubscription.builder()
                                .user(user)
                                .status(SubscriptionStatus.NONE)
                                .build());

                subscription.activate(subId, String.valueOf(variantId), portalUrl, nextBillingDate);

                userSubscriptionRepository.save(subscription);

                LocalDateTime realExpiresAt = nextBillingDate.plusDays(5);
                int totalAmount = product.getCreditAmount() + product.getBonusAmount();

                userCreditService.grantCredit(user, CreditType.SUBSCRIPTION, totalAmount, realExpiresAt, orderId);

                log.info("✅ 구독 갱신 완료: User={}, 만료일(여유포함)={}", userId, realExpiresAt);
            }

            else if ("order_created".equals(eventName) && "paid".equals(status)
                    && product.getType() == CreditType.PAID) {

                int totalAmount = product.getCreditAmount() + product.getBonusAmount();
                LocalDateTime expireDate = LocalDateTime.now().plusYears(5);

                userCreditService.grantCredit(user, product.getType(), totalAmount, expireDate, orderId);

                log.info("✅ 충전 완료: User={}, 양={}", userId, totalAmount);
            }

        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("Webhook Error", e);
            throw new CustomException(ErrorCode.WEBHOOK_HANDLING_FAILED);
        }
    }
}