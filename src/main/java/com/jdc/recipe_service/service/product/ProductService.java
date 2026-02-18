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

        boolean currentIsUnlimited = false;
        int currentCreditAmount = 0;

        List<CreditProduct> products = creditProductRepository.findAllByTypeAndIsActiveTrueOrderByCreditAmountAsc(CreditType.SUBSCRIPTION);

        if (isSubscribed && currentVariantId != null) {
            CreditProduct currentProduct = products.stream()
                    .filter(p -> String.valueOf(p.getLemonSqueezyVariantId()).equals(currentVariantId))
                    .findFirst()
                    .orElse(null);

            if (currentProduct != null) {
                currentIsUnlimited = currentProduct.isUnlimited();
                currentCreditAmount = currentProduct.getCreditAmount();
            }
        }

        String portalUrl = (isSubscribed)
                ? ((subscription.getCustomerPortalUrl() != null) ? subscription.getCustomerPortalUrl() : DEFAULT_PORTAL_URL)
                : null;
        String encodedUserId = hashids.encode(userId);

        final boolean myUnlimited = currentIsUnlimited;
        final int myCredit = currentCreditAmount;

        return products.stream()
                .map(product -> {
                    boolean isCurrentPlan = isSubscribed &&
                            String.valueOf(product.getLemonSqueezyVariantId()).equals(currentVariantId);
                    boolean isUpgrade = false;

                    if (isSubscribed && !isCurrentPlan) {
                        if (!myUnlimited && product.isUnlimited()) {
                            isUpgrade = true;
                        } else if (!myUnlimited && !product.isUnlimited() && product.getCreditAmount() > myCredit) {
                            isUpgrade = true;
                        }
                    }

                    String dynamicUrl;

                    if (isCurrentPlan) {
                        dynamicUrl = null;
                    } else if (isSubscribed) {
                        if (isUpgrade) {
                            dynamicUrl = "API_UPGRADE";
                        } else {
                            dynamicUrl = portalUrl;
                        }
                    } else {
                        String nonce = String.valueOf(System.currentTimeMillis());
                        dynamicUrl = STORE_URL + "/buy/" + product.getLemonSqueezyVariantUuid()
                                + "?checkout[custom][user_id]=" + encodedUserId
                                + "&_=" + nonce;
                    }

                    return ProductResponseDto.from(product, dynamicUrl, portalUrl, isSubscribed, isCurrentPlan);
                })
                .toList();
    }
}