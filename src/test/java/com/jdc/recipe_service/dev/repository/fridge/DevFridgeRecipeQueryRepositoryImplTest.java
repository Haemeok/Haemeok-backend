package com.jdc.recipe_service.dev.repository.fridge;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
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
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevFridgeRecipeQueryRepositoryImpl 검증 (H2).
 *
 * 핵심:
 *  1. PUBLIC+LISTED+ACTIVE 정책 (RESTRICTED/PRIVATE/HIDDEN 차단)
 *  2. imageReady 조건 (운영 V2가 빠뜨린 것 dev에서 추가한 것 검증)
 *  3. typeFilter (USER vs YOUTUBE)
 *  4. matchRate 정렬 (사용자 ingredient 매칭 비율 DESC)
 *  5. pantry ingredient 제외 (isPantry=true)
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevFridgeRecipeQueryRepositoryImpl.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevFridgeRecipeQueryRepositoryImplTest {

    @Autowired EntityManager em;
    @Autowired DevFridgeRecipeQueryRepository repo;

    private User owner;
    private Ingredient ingA;
    private Ingredient ingB;
    private Ingredient ingC;

    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        ingA = persistIngredient("재료A", "채소", false);
        ingB = persistIngredient("재료B", "채소", false);
        ingC = persistIngredient("재료C", "채소", false);
        em.flush();
    }

    @Test
    @DisplayName("PUBLIC+LISTED+ACTIVE만 SELECT — RESTRICTED/PRIVATE/HIDDEN 누수 차단")
    void searchRecipesByFridgeIngredientsDev_excludesNonPolicyMatching() {
        Recipe ok = persistRecipe(owner, "ok", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 1, false);
        Recipe priv = persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,
                RecipeListingStatus.UNLISTED, RecipeImageStatus.READY, 1, false);
        Recipe restricted = persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED,
                RecipeListingStatus.UNLISTED, RecipeImageStatus.READY, 1, false);
        Recipe hidden = persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 1, false);
        for (Recipe r : List.of(ok, priv, restricted, hidden)) {
            linkIngredient(r, ingA);
        }
        em.flush();
        em.clear();

        Slice<Recipe> result = repo.searchRecipesByFridgeIngredientsDev(
                List.of(ingA.getId()), List.of(RecipeType.USER, RecipeType.YOUTUBE), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("ok");
    }

    @Test
    @DisplayName("imageStatus=PENDING 제외 (READY 또는 null만 — 운영 V2 누락 필터를 dev에서 추가)")
    void searchRecipesByFridgeIngredientsDev_imageReadyOrNullOnly() {
        Recipe ready = persistRecipe(owner, "ready", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 1, false);
        Recipe nullImage = persistRecipe(owner, "nullImage", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, null, 1, false);
        Recipe pending = persistRecipe(owner, "pending", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.PENDING, 1, false);
        for (Recipe r : List.of(ready, nullImage, pending)) {
            linkIngredient(r, ingA);
        }
        em.flush();
        em.clear();

        Slice<Recipe> result = repo.searchRecipesByFridgeIngredientsDev(
                List.of(ingA.getId()), List.of(RecipeType.USER, RecipeType.YOUTUBE), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactlyInAnyOrder("ready", "nullImage");
    }

    @Test
    @DisplayName("typeFilter=USER: AI/YOUTUBE recipe 제외")
    void searchRecipesByFridgeIngredientsDev_typeFilterUser_excludesAiAndYoutube() {
        Recipe userRecipe = persistRecipeWithType(owner, "user", false, null);
        Recipe aiRecipe = persistRecipeWithType(owner, "ai", true, null);
        Recipe youtubeRecipe = persistRecipeWithType(owner, "youtube", false, "https://youtube.com/watch?v=abc");
        for (Recipe r : List.of(userRecipe, aiRecipe, youtubeRecipe)) {
            linkIngredient(r, ingA);
        }
        em.flush();
        em.clear();

        Slice<Recipe> result = repo.searchRecipesByFridgeIngredientsDev(
                List.of(ingA.getId()), List.of(RecipeType.USER), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("user");
    }

    @Test
    @DisplayName("matchRate 정렬: 사용자 재료 매칭 비율(매칭/총재료) 높은 순")
    void searchRecipesByFridgeIngredientsDev_sortedByMatchRateDesc() {
        // user fridge: ingA, ingB
        // recipeHigh: ingA, ingB (2/2 = 1.0)
        // recipeLow: ingA, ingC (1/2 = 0.5)
        Recipe recipeHigh = persistRecipe(owner, "high", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 2, false);
        Recipe recipeLow = persistRecipe(owner, "low", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 2, false);
        linkIngredient(recipeHigh, ingA);
        linkIngredient(recipeHigh, ingB);
        linkIngredient(recipeLow, ingA);
        linkIngredient(recipeLow, ingC);
        em.flush();
        em.clear();

        Slice<Recipe> result = repo.searchRecipesByFridgeIngredientsDev(
                List.of(ingA.getId(), ingB.getId()), List.of(RecipeType.USER, RecipeType.YOUTUBE), PAGE_10);

        // matchRate: high=1.0, low=0.5 → high가 먼저
        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("high", "low");
    }

    @Test
    @DisplayName("pantry ingredient는 매칭에서 제외 (사용자 fridge에 있어도 없는 것처럼 처리)")
    void searchRecipesByFridgeIngredientsDev_excludesPantryIngredients() {
        Ingredient pantry = persistIngredient("소금", "조미료", true); // pantry
        em.flush();

        // user fridge에 pantry id를 넣어도 매칭에서 빠짐 → recipe에 pantry만 있으면 매칭 0
        Recipe pantryOnly = persistRecipe(owner, "pantryOnly", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 1, false);
        linkIngredient(pantryOnly, pantry);

        Recipe withReal = persistRecipe(owner, "withReal", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 1, false);
        linkIngredient(withReal, ingA);

        em.flush();
        em.clear();

        // user fridge: pantry + ingA
        Slice<Recipe> result = repo.searchRecipesByFridgeIngredientsDev(
                List.of(pantry.getId(), ingA.getId()), List.of(RecipeType.USER, RecipeType.YOUTUBE), PAGE_10);

        // pantryOnly는 having count >= 1 (non-pantry 매칭) 통과 못 함
        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("withReal");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder().provider("test").oauthId(oauthId).nickname(nickname).role(Role.USER).build();
        em.persist(user);
        return user;
    }

    private Ingredient persistIngredient(String name, String category, boolean isPantry) {
        Ingredient ing = Ingredient.builder().name(name).category(category).isPantry(isPantry).build();
        em.persist(ing);
        return ing;
    }

    private Recipe persistRecipe(User user, String title,
                                  RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                  RecipeListingStatus listing, RecipeImageStatus imageStatus,
                                  int totalIngredientCount, boolean ai) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .totalIngredientCount(totalIngredientCount)
                .isAiGenerated(ai)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private Recipe persistRecipeWithType(User user, String title, boolean ai, String youtubeUrl) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)
                .imageStatus(RecipeImageStatus.READY)
                .totalIngredientCount(1)
                .isAiGenerated(ai)
                .youtubeUrl(youtubeUrl)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private void linkIngredient(Recipe recipe, Ingredient ingredient) {
        RecipeIngredient ri = RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .quantity("1")
                .unit("개")
                .build();
        em.persist(ri);
    }
}
