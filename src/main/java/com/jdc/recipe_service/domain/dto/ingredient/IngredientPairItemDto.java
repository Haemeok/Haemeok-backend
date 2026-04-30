package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Structured ingredient pair item parsed from goodPairs/badPairs.")
public class IngredientPairItemDto {

    @Schema(description = "Ingredient ID when the pair name exists in the ingredient master. Null for free-text pairs.",
            example = "xJvY7aBp", nullable = true)
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;

    @Schema(description = "Pair ingredient name", example = "마늘")
    private String name;

    @Schema(description = "Ingredient image URL when the pair is linked to a master ingredient. Null for free-text pairs.",
            example = "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/마늘.webp",
            nullable = true)
    private String imageUrl;
}
