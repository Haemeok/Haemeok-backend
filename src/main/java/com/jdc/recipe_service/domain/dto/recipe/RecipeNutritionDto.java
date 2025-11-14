package com.jdc.recipe_service.domain.dto.recipe;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import java.math.BigDecimal;

@Getter @Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RecipeNutritionDto {

    @Schema(description = "단백질 (g)")
    private BigDecimal proteinG;

    @Schema(description = "탄수화물 (g)")
    private BigDecimal carbohydrateG;

    @Schema(description = "지방 (g)")
    private BigDecimal fatG;

    @Schema(description = "당류 (g)")
    private BigDecimal sugarG;

    @Schema(description = "나트륨 (mg)")
    private Integer sodiumMg;
}