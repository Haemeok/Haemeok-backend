package com.jdc.recipe_service.domain.dto.v2.recipe;

import lombok.*;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class RecipeSimpleStatusDto {
    private boolean likedByCurrentUser;
}