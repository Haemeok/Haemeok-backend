package com.jdc.recipe_service.domain.dto.user;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ReactionRequestDto {
    private int likeCount;
    private int ratingCount;
}
