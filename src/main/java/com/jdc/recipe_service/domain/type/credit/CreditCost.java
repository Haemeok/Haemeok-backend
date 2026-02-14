package com.jdc.recipe_service.domain.type.credit;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum CreditCost {
    AI_RECIPE_TEXT("AI 레시피 생성 (텍스트)"),
    YOUTUBE_SUMMARY_TEXT("유튜브 추출 (텍스트)"),

    AI_RECIPE_IMAGE("AI 레시피 생성 (이미지 포함)"),
    YOUTUBE_SUMMARY_IMAGE("유튜브 추출 (이미지 포함)");

    private final String description;
}