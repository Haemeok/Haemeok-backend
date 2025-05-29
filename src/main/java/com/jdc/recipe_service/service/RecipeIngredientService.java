package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;

    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos, RecipeSourceType sourceType) {
        int totalCost = 0;

        Map<String, Ingredient> ingredientMap = ingredientRepository.findAll().stream()
                .collect(Collectors.toMap(i -> i.getName().toLowerCase().trim(), Function.identity()));

        for (RecipeIngredientRequestDto dto : dtos) {
            if (dto.getName() == null || dto.getName().isBlank()) {
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "재료 이름이 비어있습니다.");
            }
            // AI 생성 시 dto.getCustomUnit()에 AI의 "unit"이 매핑되어 있어야 함
            if (sourceType == RecipeSourceType.AI && (dto.getCustomUnit() == null || dto.getCustomUnit().isBlank())) {
                System.err.println("경고: AI가 생성한 재료 '" + dto.getName() + "'의 단위 정보(customUnit)가 누락되었습니다.");
                // 필요시 예외 처리 또는 기본 단위 사용 로직 추가
            }

            String nameKey = dto.getName().toLowerCase().trim();
            double quantity;
            try {
                quantity = parseQuantity(dto.getQuantity());
            } catch (NumberFormatException e) {
                throw new CustomException(ErrorCode.INVALID_INGREDIENT_QUANTITY, "재료 '"+dto.getName()+"'의 수량 '"+dto.getQuantity()+"'이(가) 올바르지 않습니다: " + e.getMessage());
            }

            Ingredient masterIngredient = ingredientMap.get(nameKey);
            BigDecimal unitPrice = BigDecimal.ZERO;
            String unitForRecipeItem;

            if (masterIngredient != null) {
                unitPrice = (masterIngredient.getPrice() != null) ? BigDecimal.valueOf(masterIngredient.getPrice()) : BigDecimal.ZERO;
                unitForRecipeItem = (dto.getCustomUnit() != null && !dto.getCustomUnit().isBlank()) ? dto.getCustomUnit() : masterIngredient.getUnit();
            } else {
                unitForRecipeItem = dto.getCustomUnit(); // 새로운 재료의 단위는 DTO의 customUnit 사용 (AI의 unit 또는 사용자의 customUnit)
                if (sourceType == RecipeSourceType.AI) {
                    unitPrice = BigDecimal.ZERO;
                    if (unitForRecipeItem == null || unitForRecipeItem.isBlank()) {
                        System.err.println("재확인 경고: AI 신규 재료 '" + dto.getName() + "'의 단위(customUnit)가 없습니다.");
                    }
                } else {
                    if (dto.getCustomPrice() == null || unitForRecipeItem == null || unitForRecipeItem.isBlank()) {
                        throw new CustomException(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING, dto.getName());
                    }
                    unitPrice = dto.getCustomPrice();
                }
            }

            int calculatedPrice = unitPrice.multiply(BigDecimal.valueOf(quantity)).intValue();
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