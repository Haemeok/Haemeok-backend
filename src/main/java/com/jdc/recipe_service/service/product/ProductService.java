package com.jdc.recipe_service.service.product;

import com.jdc.recipe_service.domain.dto.product.ProductResponseDto;
import com.jdc.recipe_service.domain.entity.user.UserSubscription;
import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.repository.user.UserSubscriptionRepository;
import com.jdc.recipe_service.domain.type.product.SubscriptionStatus;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ProductService {

    private final CreditProductRepository creditProductRepository;
    private final UserSubscriptionRepository userSubscriptionRepository;
    private final Hashids hashids;

    private static final String STORE_URL = "https://recipio.lemonsqueezy.com";
    private static final String DEFAULT_PORTAL_URL = "https://recipio.lemonsqueezy.com/billing";

    public List<ProductResponseDto> getActiveProducts(Long userId) {
        UserSubscription subscription = userSubscriptionRepository.findByUserId(userId).orElse(null);

        boolean isSubscribed = subscription != null &&
                (subscription.getStatus() == SubscriptionStatus.ACTIVE ||
                        subscription.getStatus() == SubscriptionStatus.CANCELED);

        String currentVariantId = (subscription != null) ? subscription.getLemonSqueezyVariantId() : null;

        String portalUrl = (isSubscribed && subscription.getCustomerPortalUrl() != null)
                ? subscription.getCustomerPortalUrl()
                : DEFAULT_PORTAL_URL;

        String encodedUserId = hashids.encode(userId);

        List<CreditProduct> products = creditProductRepository.findAll().stream()
                .filter(CreditProduct::isActive)
                .filter(p -> p.getType() == CreditType.SUBSCRIPTION || p.getType() == CreditType.PAID)
                .toList();

        LocalDateTime nextBillingDate = (subscription != null) ? subscription.getNextBillingDate() : null;

        return products.stream()
                .map(product -> {
                    boolean isCurrentPlan = isSubscribed &&
                            String.valueOf(product.getLemonSqueezyVariantId()).equals(currentVariantId);

                    String checkoutUrl = STORE_URL + "/buy/" + product.getLemonSqueezyVariantUuid()
                            + "?checkout[custom][user_id]=" + encodedUserId
                            + "&_=" + System.currentTimeMillis();

                    String dynamicUrl;

                    if (product.getType() == CreditType.PAID) {
                        dynamicUrl = checkoutUrl;
                    } else {
                        if (isCurrentPlan) {
                            dynamicUrl = null;
                        } else if (isSubscribed) {
                            dynamicUrl = portalUrl;
                        } else {
                            dynamicUrl = checkoutUrl;
                        }
                    }
                    LocalDateTime displayBillingDate = isCurrentPlan ? nextBillingDate : null;

                    return ProductResponseDto.from(product, dynamicUrl, portalUrl,
                            isCurrentPlan,
                            isCurrentPlan,
                            displayBillingDate
                    );
                })
                .toList();
    }
}