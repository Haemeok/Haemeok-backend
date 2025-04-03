package com.jdc.recipe_service.domain.dto.recipe;


import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor

/**
 * 레시피 단순 조히용
 */
public class RecipeSimpleDto {
    private Long id;
    private String title;
    private String imageUrl;
    private String authorName;
    private LocalDateTime createdAt;

    private long likeCount;
    private boolean likedByCurrentUser;

    public void setLikedByCurrentUser(boolean b) {
        likedByCurrentUser = b;
    }

}
