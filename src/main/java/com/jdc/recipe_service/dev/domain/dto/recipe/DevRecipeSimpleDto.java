package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * Dev V3 favorites/사용자 동적 응답 DTO.
 *
 * 운영 {@link RecipeSimpleDto} 모든 필드를 {@code @SuperBuilder}로 상속하고 dev V3 4-enum
 * (visibility/listingStatus/lifecycleStatus/source) 추가 노출. 운영 DTO zero touch.
 */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "Dev V3 레시피 간략 DTO (V1 base + 4 enum)")
public class DevRecipeSimpleDto extends RecipeSimpleDto {

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "listing 상태", example = "LISTED")
    private String listingStatus;

    @Schema(description = "lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "source", example = "USER")
    private String source;
}
