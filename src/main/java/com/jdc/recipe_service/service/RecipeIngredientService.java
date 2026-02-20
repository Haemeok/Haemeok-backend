package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private static final Set<String> SPECIAL_QUANTITY_WORDS = Set.of("약간");


    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos, RecipeSourceType sourceType) {
        int totalCost = 0;

        Map<String, Ingredient> ingredientMap = ingredientRepository.findAll().stream()
                .collect(Collectors.toMap(i -> i.getName().toLowerCase().trim(), Function.identity()));

        for (RecipeIngredientRequestDto dto : dtos) {
            if (dto.getName() == null || dto.getName().isBlank()) {
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "재료 이름이 비어있습니다.");
            }
            if (sourceType == RecipeSourceType.AI && (dto.getCustomUnit() == null || dto.getCustomUnit().isBlank())) {
                log.warn("경고: AI가 생성한 재료 '" + dto.getName() + "'의 단위 정보(customUnit)가 누락되었습니다.");
            }

            String nameKey = dto.getName().toLowerCase().trim();
            double quantity;
            try {
                quantity = parseQuantity(dto.getQuantity());
            } catch (NumberFormatException e) {
                log.warn("수량 파싱 실패: name={}, quantity='{}'. → '약간'으로 보정", dto.getName(), dto.getQuantity());
                dto.setQuantity("약간");
                quantity = 0.0;
            }

            Ingredient masterIngredient = ingredientMap.get(nameKey);

            int calculatedPrice = 0;
            String unitForRecipeItem;

            if (masterIngredient != null) {
                BigDecimal pricePerUnit = (masterIngredient.getPrice() != null)
                        ? BigDecimal.valueOf(masterIngredient.getPrice())
                        : BigDecimal.ZERO;

                calculatedPrice = pricePerUnit.multiply(BigDecimal.valueOf(quantity)).intValue();

                unitForRecipeItem = (dto.getCustomUnit() != null && !dto.getCustomUnit().isBlank())
                        ? dto.getCustomUnit()
                        : masterIngredient.getUnit();
            } else {
                unitForRecipeItem = dto.getCustomUnit();

                if (unitForRecipeItem == null || unitForRecipeItem.isBlank()) {
                    throw new CustomException(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING, dto.getName());
                }

                if (dto.getCustomPrice() == null) {
                    log.warn("신규 재료 '{}' customPrice 누락 → 0으로 처리", dto.getName());
                }

                calculatedPrice = (dto.getCustomPrice() != null)
                        ? dto.getCustomPrice().intValue()
                        : 0;
            }

            totalCost += calculatedPrice;

            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, masterIngredient, calculatedPrice, unitForRecipeItem, sourceType);
            recipeIngredientRepository.save(entity);
        }
        return totalCost;
    }

    @Transactional
    public int updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos, RecipeSourceType sourceType) {
        List<RecipeIngredient> existingIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        for (RecipeIngredient ri : existingIngredients) {
            recipeStepIngredientRepository.deleteByRecipeIngredientId(ri.getId());
        }
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush();
        return saveAll(recipe, ingredientDtos, sourceType);
    }

    @Transactional
    public int updateIngredientsFromUser(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        return updateIngredients(recipe, dtos, RecipeSourceType.USER);
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeIngredient> ingredientsToDelete = recipeIngredientRepository.findByRecipeId(recipeId);
        for (RecipeIngredient ri : ingredientsToDelete) {
            recipeStepIngredientRepository.deleteByRecipeIngredientId(ri.getId());
        }
        recipeIngredientRepository.deleteByRecipeId(recipeId);
    }

    private double parseQuantity(String quantityStr) {
        if (quantityStr == null || quantityStr.trim().isEmpty()) {
            throw new NumberFormatException("수량 문자열이 비어있습니다.");
        }
        quantityStr = quantityStr.trim();

        if (SPECIAL_QUANTITY_WORDS.contains(quantityStr)) {
            return 0.0;
        }

        if (quantityStr.contains("/")) {
            String[] parts = quantityStr.split("/");
            if (parts.length == 2) {
                try {
                    double numerator = Double.parseDouble(parts[0].trim());
                    double denominator = Double.parseDouble(parts[1].trim());
                    if (denominator == 0) {
                        throw new NumberFormatException("분모는 0이 될 수 없습니다: " + quantityStr);
                    }
                    return numerator / denominator;
                } catch (NumberFormatException e) {
                    throw new NumberFormatException("잘못된 분수 형식입니다: " + quantityStr + " (" + e.getMessage() + ")");
                }
            } else {
                throw new NumberFormatException("잘못된 분수 형식입니다: " + quantityStr);
            }
        }
        try {
            return Double.parseDouble(quantityStr);
        } catch (NumberFormatException e) {
            throw new NumberFormatException("숫자로 변환할 수 없는 수량입니다: " + quantityStr + " (" + e.getMessage() + ")");
        }
    }
}