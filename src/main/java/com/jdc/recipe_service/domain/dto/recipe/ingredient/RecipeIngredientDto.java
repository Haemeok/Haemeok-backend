package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;


/**
 *
 *  재료 응답용
 */


@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientDto {
    private Long id;
    private String name;
    private String quantity;
    private String unit;
    private Integer price;

    public static RecipeIngredientDto from(RecipeIngredient ri) {
        Ingredient ingredient = ri.getIngredient();
        boolean isCustom = (ingredient == null);

        return RecipeIngredientDto.builder()
                .id(isCustom ? null : ingredient.getId())
                .name(isCustom ? ri.getCustomName() : ingredient.getName())
                .quantity(ri.getQuantity())
                .unit(isCustom ? ri.getCustomUnit() : ri.getUnit())
                .price(isCustom
                        ? (ri.getCustomPrice() != null ? ri.getCustomPrice().intValue() : null)
                        : (ingredient.getPrice() != null ? ingredient.getPrice() : 0))
                .build();
    }

}
