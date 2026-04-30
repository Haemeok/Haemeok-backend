package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.type.Role;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class FridgeRecipeQueryRepositoryImplTest {

    private static final Pageable PAGE_10_BY_FAVORITE_DESC =
            PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, "favoriteCount"));

    @Autowired
    EntityManager em;

    @Autowired
    RecipeRepository recipeRepository;

    private User owner;
    private Ingredient potato;

    @BeforeEach
    void setUp() {
        owner = User.builder()
                .provider("test")
                .oauthId("fridge-owner")
                .nickname("owner")
                .role(Role.USER)
                .build();
        em.persist(owner);

        potato = Ingredient.builder()
                .name("potato")
                .category("vegetable")
                .isPantry(false)
                .build();
        em.persist(potato);
        em.flush();
    }

    @Test
    @DisplayName("searchRecipesByFridgeIngredients: favoriteCount >= 2 recipes only enter fridge match calculation")
    void searchRecipesByFridgeIngredients_requiresFavoriteCountAtLeastTwo() {
        Recipe savedOnce = persistRecipe("saved-once", 1L);
        Recipe threshold = persistRecipe("threshold", 2L);
        Recipe popular = persistRecipe("popular", 3L);
        linkIngredient(savedOnce, potato);
        linkIngredient(threshold, potato);
        linkIngredient(popular, potato);
        em.flush();
        em.clear();

        Slice<Recipe> result = recipeRepository.searchRecipesByFridgeIngredients(
                List.of(potato.getId()), List.of(RecipeType.USER), PAGE_10_BY_FAVORITE_DESC);

        assertThat(result.getContent())
                .extracting(Recipe::getTitle)
                .containsExactly("popular", "threshold");
    }

    private Recipe persistRecipe(String title, Long favoriteCount) {
        Recipe recipe = Recipe.builder()
                .user(owner)
                .title(title)
                .dishType(DishType.FRYING)
                .favoriteCount(favoriteCount)
                .totalIngredientCount(1)
                .isAiGenerated(false)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private void linkIngredient(Recipe recipe, Ingredient ingredient) {
        RecipeIngredient ri = RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .quantity("1")
                .unit("piece")
                .build();
        em.persist(ri);
    }
}
