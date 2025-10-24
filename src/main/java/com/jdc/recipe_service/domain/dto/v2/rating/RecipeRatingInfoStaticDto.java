package com.jdc.recipe_service.domain.dto.v2.rating;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RecipeRatingInfoStaticDto {
    private BigDecimal avgRating;
    private Long ratingCount;
}
