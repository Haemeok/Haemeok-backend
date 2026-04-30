package com.jdc.recipe_service.dev.service.ingredient;

import com.jdc.recipe_service.dev.repository.recipe.DevTopRecipeQueryRepository;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.service.IngredientService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * Dev V3 ingredient detail service.
 *
 * 운영 {@link IngredientService#findDetailById}를 그대로 호출해서 보관 정보/페어링/조리법은 동일하게 받고,
 * top recipe 부분만 dev 정책({@link DevTopRecipeQueryRepository})으로 교체한다.
 *
 * 비효율: 운영 service가 이미 한 번 top recipe query를 도므로 같은 ingredient에 대해 두 번 조회 발생.
 * dev MVP에서 감수 (호출 빈도 낮고, swap 후에는 운영 path가 dev 정책을 흡수하므로 자연 해소).
 */
@Service
@RequiredArgsConstructor
public class DevIngredientService {

    private final IngredientService ingredientService;
    private final DevTopRecipeQueryRepository devTopRecipeQueryRepository;

    private static final int TOP_LIMIT = 10;

    @Transactional(readOnly = true)
    public IngredientDetailDto findDetailByIdDev(Long ingredientId) {
        IngredientDetailDto base = ingredientService.findDetailById(ingredientId);
        List<RecipeSimpleDto> devRecipes = devTopRecipeQueryRepository.findTopByIngredientIdDev(ingredientId, TOP_LIMIT);

        return base.toBuilder()
                .recipes(devRecipes)
                .build();
    }
}
