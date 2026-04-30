package com.jdc.recipe_service.dev.repository.fridge;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.RecipeType;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;

import java.util.List;

/**
 * Dev V3 fridge 추천용 repository.
 *
 * 운영 {@link com.jdc.recipe_service.domain.repository.RecipeQueryRepositoryImpl#searchRecipesByFridgeIngredients}
 * 의 dev 버전. {@code recipe.isPrivate.isFalse()} 자리에 4-enum 정책(PUBLIC+LISTED+ACTIVE) 적용.
 *
 * 응답은 entity Slice — 변환은 service 레이어에서 (운영 FridgeRecipeService 패턴 그대로).
 */
public interface DevFridgeRecipeQueryRepository {

    Slice<Recipe> searchRecipesByFridgeIngredientsDev(
            List<Long> userIngredientIds,
            List<RecipeType> types,
            Pageable pageable);
}
