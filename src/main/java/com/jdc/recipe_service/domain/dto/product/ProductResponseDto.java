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

    public static ProductResponseDto from(CreditProduct product, String fullCheckoutUrl) {
        return ProductResponseDto.builder()
                .id(product.getId())
                .name(product.getName())
                .creditAmount(product.getCreditAmount())
                .bonusAmount(product.getBonusAmount())
                .type(product.getType().name())
                .checkoutUrl(fullCheckoutUrl)
                .build();
    }
}