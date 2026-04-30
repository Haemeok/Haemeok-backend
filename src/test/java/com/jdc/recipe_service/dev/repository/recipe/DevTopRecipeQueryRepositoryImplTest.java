package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
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
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevTopRecipeQueryRepositoryImpl 검증 (H2).
 *
 * 핵심:
 *  1. ingredient 매칭 + PUBLIC+LISTED+ACTIVE 정책 적용 (RESTRICTED/PRIVATE/HIDDEN 차단)
 *  2. imageReady 조건 (READY 또는 null만)
 *  3. popularityScore DESC, id DESC 정렬
 *  4. limit 적용
 *  5. 다른 ingredient를 가진 recipe는 제외
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevTopRecipeQueryRepositoryImpl.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevTopRecipeQueryRepositoryImplTest {

    @Autowired EntityManager em;
    @Autowired DevTopRecipeQueryRepository repo;

    private User owner;
    private Ingredient target;
    private Ingredient other;

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        target = persistIngredient("대파", "채소");
        other = persistIngredient("양파", "채소");
        em.flush();
    }

    @Test
    @DisplayName("findTopByIngredientIdDev: PUBLIC+LISTED+ACTIVE만 SELECT + popularityScore 정렬")
    void findTopByIngredientIdDev_publicListedActiveOnly_sortedByPopularity() {
        Recipe high = persistRecipe(owner, "high", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        Recipe low = persistRecipe(owner, "low", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 10L);
        linkIngredient(high, target);
        linkIngredient(low, target);
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientIdDev(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("high", "low");
    }

    @Test
    @DisplayName("findTopByIngredientIdDev: RESTRICTED/PRIVATE/HIDDEN/BANNED 누수 차단")
    void findTopByIngredientIdDev_excludesNonPolicyMatching() {
        Recipe ok = persistRecipe(owner, "ok", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        Recipe priv = persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,
                RecipeListingStatus.UNLISTED, RecipeImageStatus.READY, 100L);
        Recipe restricted = persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED,
                RecipeListingStatus.UNLISTED, RecipeImageStatus.READY, 100L);
        Recipe hidden = persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        Recipe banned = persistRecipe(owner, "banned", RecipeLifecycleStatus.BANNED, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        for (Recipe r : List.of(ok, priv, restricted, hidden, banned)) {
            linkIngredient(r, target);
        }
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientIdDev(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("ok");
    }

    @Test
    @DisplayName("findTopByIngredientIdDev: imageStatus=READY 또는 null만 (PENDING 제외)")
    void findTopByIngredientIdDev_imageReadyOrNullOnly() {
        Recipe ready = persistRecipe(owner, "ready", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        Recipe nullImage = persistRecipe(owner, "nullImage", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, null, 80L);
        Recipe pending = persistRecipe(owner, "pending", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.PENDING, 100L);
        for (Recipe r : List.of(ready, nullImage, pending)) {
            linkIngredient(r, target);
        }
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientIdDev(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactlyInAnyOrder("ready", "nullImage");
    }

    @Test
    @DisplayName("findTopByIngredientIdDev: limit 적용")
    void findTopByIngredientIdDev_respectsLimit() {
        for (int i = 1; i <= 5; i++) {
            Recipe r = persistRecipe(owner, "r" + i, RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                    RecipeListingStatus.LISTED, RecipeImageStatus.READY, (long) (100 - i * 10));
            linkIngredient(r, target);
        }
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientIdDev(target.getId(), 3);

        assertThat(result).hasSize(3);
        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("r1", "r2", "r3");
    }

    @Test
    @DisplayName("findTopByIngredientIdDev: 다른 ingredient만 가진 recipe는 제외")
    void findTopByIngredientIdDev_excludesRecipesWithoutTargetIngredient() {
        Recipe withTarget = persistRecipe(owner, "withTarget", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 50L);
        Recipe withOther = persistRecipe(owner, "withOther", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, 100L);
        linkIngredient(withTarget, target);
        linkIngredient(withOther, other);
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientIdDev(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("withTarget");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder().provider("test").oauthId(oauthId).nickname(nickname).role(Role.USER).build();
        em.persist(user);
        return user;
    }

    private Ingredient persistIngredient(String name, String category) {
        Ingredient ing = Ingredient.builder().name(name).category(category).build();
        em.persist(ing);
        return ing;
    }

    private Recipe persistRecipe(User user, String title,
                                  RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                  RecipeListingStatus listing, RecipeImageStatus imageStatus,
                                  Long popularityScore) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .popularityScore(popularityScore)
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
