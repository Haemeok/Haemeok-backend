package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.jdc.recipe_service.domain.dto.v2.comment.CommentStatusDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Schema(description = "레시피 상세 사용자 특정 동적 정보 DTO")
public class RecipeDetailStatusDto {

    @Schema(description = "좋아요 수")
    private int likeCount;

    @Schema(description = "현재 로그인한 사용자가 좋아요를 눌렀는지 여부")
    private boolean likedByCurrentUser;

    @Schema(description = "현재 로그인한 사용자가 즐겨찾기를 눌렀는지 여부")
    private boolean favoriteByCurrentUser;

    @Schema(description = "현재 로그인한 사용자의 평점")
    private Integer myRating;

    @Schema(description = "댓글들의 사용자 특정 상태 정보 (ID 및 나의 좋아요 여부)")
    private List<CommentStatusDto> comments;
}