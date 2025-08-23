package com.jdc.recipe_service.domain.dto.recipe.v2;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import lombok.*;

import java.util.List;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStatusDto {
    private long likeCount;
    private boolean likedByCurrentUser;
    private boolean favoriteByCurrentUser;
    private Double avgRating;
    private Integer myRating;
    private long ratingCount;
    private long commentCount;
    private List<CommentDto> comments;
}