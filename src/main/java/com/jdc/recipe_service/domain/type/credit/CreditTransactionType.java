package com.jdc.recipe_service.domain.type.credit;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum CreditTransactionType {
    CHARGE("충전"),
    SUBSCRIPTION("정기구독"),
    BONUS("보너스"),
    USE("사용"),
    REFUND("환불/취소"),
    EXPIRED("만료");

    private final String description;
}