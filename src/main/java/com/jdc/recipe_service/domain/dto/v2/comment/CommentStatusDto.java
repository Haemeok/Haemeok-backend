package com.jdc.recipe_service.domain.dto.v2.comment;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Schema(description = "댓글 사용자 특정 동적 정보 DTO")
public class CommentStatusDto {

    @Schema(description = "댓글 ID")
    private Long id;

    @Schema(description = "현재 로그인한 사용자가 이 댓글에 좋아요를 눌렀는지 여부")
    private boolean likedByCurrentUser;

    @Schema(description = "댓글 좋아요 수")
    private int likeCount;
}