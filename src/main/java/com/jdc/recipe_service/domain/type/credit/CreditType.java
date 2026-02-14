package com.jdc.recipe_service.domain.type.credit;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum CreditType {

    // 소진 순서: 구독(사라짐) -> 보너스(기간제) -> 베이직(무료) -> 충전(내돈)
    SUBSCRIPTION("구독 정기권", 1), // 1순위: 기간 지나면 100% 사라짐
    BONUS("이벤트/초대", 2),       // 2순위: 보통 유효기간 30~90일
    BASIC("기본 무료제공", 3),     // 3순위: 가입 축하금 (유효기간 길게 or 무제한)
    PAID("유료 충전", 4);          // 4순위: 환불 대상, 최대한 아껴둠

    private final String description;
    private final int priority;
}