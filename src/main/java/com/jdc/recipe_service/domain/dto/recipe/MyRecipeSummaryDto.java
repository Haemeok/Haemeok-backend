package com.jdc.recipe_service.domain.dto.recipe;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;


/**
 * 내가 작성한 레시피 목록 조회용
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MyRecipeSummaryDto {
    private Long id;
    private String title;
    private String imageUrl;
    private String dishType;
    private LocalDateTime createdAt;
    private boolean isAiGenerated;
}
