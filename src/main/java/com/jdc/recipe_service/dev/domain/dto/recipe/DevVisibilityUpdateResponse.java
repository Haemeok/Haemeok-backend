package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;

/** Dev V3 가시성 변경 응답. RESTRICTED는 신규 입력에서 거부되므로 정상 흐름에 등장하지 않음. */
public record DevVisibilityUpdateResponse(
        @Schema(description = "갱신된 visibility. 신규 응답에는 PUBLIC | PRIVATE만 등장.",
                allowableValues = {"PUBLIC", "PRIVATE"})
        RecipeVisibility visibility,

        @Schema(description = "legacy isPrivate. PRIVATE=true, PUBLIC=false.")
        Boolean isPrivate
) {}
