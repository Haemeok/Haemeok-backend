package com.jdc.recipe_service.dev.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Dev V3 레시피북 상세 응답.
 *
 * 운영 {@link com.jdc.recipe_service.domain.dto.recipebook.RecipeBookDetailResponse}와 동일 shape이지만
 * recipes 필드의 타입을 {@link DevRecipeBookItemResponse} (4 enum 노출)로 교체.
 *
 * recipeCount는 dev 정책 통과 레시피만 — 다른 사람 RESTRICTED는 카운트에서도 제외 (운영보다 엄격).
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Dev V3 레시피북 상세 응답 (recipes는 4 enum 노출)")
public class DevRecipeBookDetailResponse {

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "레시피북 ID")
    private Long id;

    @Schema(description = "레시피북 이름")
    private String name;

    @Schema(description = "기본 레시피북 여부")
    private boolean isDefault;

    @Schema(description = "현재 사용자가 dev 정책으로 볼 수 있는 레시피 수 " +
            "(본인 ACTIVE PRIVATE/RESTRICTED + 다른 사람 PUBLIC+LISTED+ACTIVE). " +
            "타인의 RESTRICTED/PRIVATE/non-ACTIVE는 카운트 제외 — 운영보다 엄격.")
    private int recipeCount;

    @Schema(description = "레시피 목록 (default sort: item.createdAt DESC = 폴더 추가 시각 기준 최신순)")
    private List<DevRecipeBookItemResponse> recipes;

    @Schema(description = "다음 페이지 존재 여부")
    private boolean hasNext;
}
