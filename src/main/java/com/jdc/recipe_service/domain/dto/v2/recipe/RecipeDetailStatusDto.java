package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
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
    private Long likeCount;

    @Schema(description = "현재 로그인한 사용자가 좋아요를 눌렀는지 여부")
    private boolean likedByCurrentUser;

    @Schema(description = "즐겨찾기 수")
    private Long favoriteCount;

    @Schema(description = "현재 로그인한 사용자가 즐겨찾기를 눌렀는지 여부")
    private boolean favoriteByCurrentUser;

    @Schema(description = "현재 로그인한 사용자의 평점")
    private Double myRating;

    @Schema(description = "댓글들의 사용자 특정 상태 정보 (ID 및 나의 좋아요 여부)")
    private List<CommentStatusDto> comments;

    @Schema(description = "이 레시피 재료 중 현재 로그인한 사용자의 냉장고에 보유 중인 재료 ID 목록. 비로그인 또는 목록(배치) 조회에서는 비어 있을 수 있다.")
    @JsonSerialize(contentUsing = HashIdSerializer.class)
    private List<Long> ingredientIdsInFridge;

    @Schema(description = "현재 요청자가 이미 이 원본을 리믹스했는지. 비로그인 또는 목록(배치) 조회에서는 false.",
            example = "false")
    private boolean clonedByMe;

    @Schema(description = "이 레시피를 복제하여 공개된 레시피 수. 비로그인은 무관(누구에게나 같은 값). 목록(배치) 조회에서는 0.",
            example = "12")
    private long remixCount;
}