package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

public class RecipeIngredientMapper {

    public static RecipeIngredient toEntity(
            RecipeIngredientRequestDto dto,
            Recipe recipe,
            Ingredient masterIngredient,
            int calculatedPriceForItem,
            String unitForRecipeItemField,
            RecipeSourceType sourceType) {

        RecipeIngredient.RecipeIngredientBuilder builder = RecipeIngredient.builder()
                .recipe(recipe)
                .quantity(dto.getQuantity())
                .price(calculatedPriceForItem)
                .unit(unitForRecipeItemField);

        if (masterIngredient != null) {
            builder.ingredient(masterIngredient)
                    .customName(null)
                    .customPrice(0)
                    .customUnit(null)
                    .customCalorie(BigDecimal.ZERO)
                    .customCarbohydrate(BigDecimal.ZERO)
                    .customProtein(BigDecimal.ZERO)
                    .customFat(BigDecimal.ZERO)
                    .customSugar(BigDecimal.ZERO)
                    .customSodium(BigDecimal.ZERO);
        } else {
            int priceValue = (dto.getCustomPrice() != null)
                    ? dto.getCustomPrice().intValue()
                    : 0;



            builder.ingredient(null)
                    .customName(dto.getName())
                    .customUnit(unitForRecipeItemField)
                    .customPrice(priceValue)
                    .customCalorie(
                            Optional.ofNullable(dto.getCustomCalories()).orElse(BigDecimal.ZERO)
                    )
                    .customCarbohydrate(
                            Optional.ofNullable(dto.getCustomCarbohydrate()).orElse(BigDecimal.ZERO)
                    )
                    .customProtein(
                            Optional.ofNullable(dto.getCustomProtein()).orElse(BigDecimal.ZERO)
                    )
                    .customFat(
                            Optional.ofNullable(dto.getCustomFat()).orElse(BigDecimal.ZERO)
                    )
                    .customSugar(
                            Optional.ofNullable(dto.getCustomSugar()).orElse(BigDecimal.ZERO)
                    )
                    .customSodium(
                            Optional.ofNullable(dto.getCustomSodium()).orElse(BigDecimal.ZERO));
        }
        return builder.build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        if (entity == null) return null;

        Ingredient ingredient = entity.getIngredient();

        boolean isCustom = (ingredient == null);

        if (isCustom && entity.getCustomName() == null) {
            return null;
        }

        int totalPrice = Optional.ofNullable(entity.getPrice()).orElse(0);

        Double quantityValue = null;
        try {
            quantityValue = parseQuantity(entity.getQuantity());
        } catch (Exception e) {
        }

        Double totalCalories = null;

        if (isCustom) {
            if (entity.getCustomCalorie() != null) {
                totalCalories = entity.getCustomCalorie().doubleValue();
            }
        } else {
            if (ingredient.getCalorie() != null && quantityValue != null) {
                totalCalories = ingredient.getCalorie().doubleValue() * quantityValue;
            }
        }

        return RecipeIngredientDto.builder()
                .id(ingredient != null ? ingredient.getId() : null)
                .name(ingredient != null ? ingredient.getName() : entity.getCustomName())
                .quantity(formatQuantityForDisplay(entity.getQuantity()))
                .unit(ingredient != null ? entity.getUnit() : entity.getCustomUnit())
                .price(totalPrice)
                .calories(totalCalories)
                .coupangLink(ingredient != null ? ingredient.getCoupangLink() : null)
                .build();
    }

    public static List<RecipeIngredientDto> toDtoList(List<RecipeIngredient> entities) {
        if (entities == null) {
            return List.of();
        }
        return entities.stream()
                .map(RecipeIngredientMapper::toDto)
                .filter(dto -> dto != null)
                .toList();
    }

    private static double parseQuantity(String quantityStr) {
        if (quantityStr == null) return 0.0;
        quantityStr = quantityStr.trim();
        quantityStr = quantityStr.replaceAll("[^0-9./]", "");
        if (quantityStr.contains("/")) {
            String[] parts = quantityStr.split("/");
            if (parts.length == 2) {
                double numerator = Double.parseDouble(parts[0]);
                double denominator = Double.parseDouble(parts[1]);
                if (denominator == 0) throw new NumberFormatException("0으로 나눌 수 없습니다");
                return numerator / denominator;
            } else {
                throw new NumberFormatException("잘못된 분수: " + quantityStr);
            }
        }
        return Double.parseDouble(quantityStr);
    }

    private static String formatQuantityForDisplay(String quantityStr) {
        try {
            double value = parseQuantity(quantityStr);
            if (value == (int) value) {
                return Integer.toString((int) value);
            } else {
                return quantityStr;
            }
        } catch (Exception e) {
            return quantityStr;
        }
    }
}
