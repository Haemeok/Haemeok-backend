package com.jdc.recipe_service.domain.dto.product;

import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class ProductResponseDto {
    private Long id;
    private String name;
    private int creditAmount;
    private int bonusAmount;
    private String type;
    private String checkoutUrl;
    private String customerPortalUrl;
    private boolean isSubscribed;
    private boolean isCurrentPlan;

    public static ProductResponseDto from(CreditProduct product, String fullCheckoutUrl, String customerPortalUrl, boolean isSubscribed, boolean isCurrentPlan) {
        return ProductResponseDto.builder()
                .id(product.getId())
                .name(product.getName())
                .creditAmount(product.getCreditAmount())
                .bonusAmount(product.getBonusAmount())
                .type(product.getType().name())
                .checkoutUrl(fullCheckoutUrl)
                .customerPortalUrl(customerPortalUrl)
                .isSubscribed(isSubscribed)
                .isCurrentPlan(isCurrentPlan)
                .build();
    }
}