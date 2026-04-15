package com.jdc.recipe_service.domain.dto.v2.recipe;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Schema(description = "레시피 목록용 사용자 특정 동적 정보 DTO")
public class RecipeSimpleStatusDto {

    @Schema(description = "현재 로그인한 사용자가 좋아요를 눌렀는지 여부")
    private boolean likedByCurrentUser;

    @Schema(description = "현재 로그인한 사용자가 즐겨찾기를 눌렀는지 여부")
    private boolean favoriteByCurrentUser;
}
