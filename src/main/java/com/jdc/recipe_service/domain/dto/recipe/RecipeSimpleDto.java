package com.jdc.recipe_service.domain.dto.recipe;


import com.querydsl.core.annotations.QueryProjection;
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

    @QueryProjection
    public RecipeSimpleDto(Long id, String title, String imageUrl, String authorName,
                           LocalDateTime createdAt, int likeCount, boolean likedByCurrentUser) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorName = authorName;
        this.createdAt = createdAt;
        this.likeCount = likeCount;
        this.likedByCurrentUser = likedByCurrentUser;
    }
}
