package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevUserRecipesQueryRepositoryImpl 검증 (H2).
 *
 * 핵심:
 *  1. anonymous/non-owner: ACTIVE+PUBLIC+LISTED만 — RESTRICTED/PRIVATE 누수 차단
 *  2. owner: 자신의 ACTIVE PRIVATE/RESTRICTED 추가 노출
 *  3. non-ACTIVE는 owner도 차단 (admin 우회 방지)
 *  4. AI imageKey null 차단 (운영 "completed recipes" 정책)
 *  5. imageReady (READY 또는 null만) — A2/A3 일관
 *  6. source filter (sourceTypes IN)
 *  7. count/content 일관성 (totalElements 정확)
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevUserRecipesQueryRepositoryImpl.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevUserRecipesQueryRepositoryImplTest {

    @Autowired EntityManager em;
    @Autowired DevUserRecipesQueryRepository repo;

    private User owner;
    private User other;

    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        other = persistUser("other-oauth", "other");
        em.flush();
    }

    @Test
    @DisplayName("anonymous (viewer=null): owner의 ACTIVE+PUBLIC+LISTED만 SELECT — RESTRICTED/PRIVATE 누수 차단 + total 정확")
    void anonymous_publicListedActiveOnly_totalAccurate() {
        persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), null, null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("pub");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("non-owner (viewer != owner): RESTRICTED/PRIVATE 누수 차단 + 정확한 total")
    void nonOwner_excludesRestrictedAndPrivate() {
        persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), other.getId(), null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("pub");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("owner (viewer == target): 자신의 ACTIVE PRIVATE/RESTRICTED 모두 노출")
    void owner_includesOwnPrivateAndRestricted() {
        persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), owner.getId(), null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle)
                .containsExactlyInAnyOrder("pub", "priv", "restricted");
        assertThat(result.getTotalElements()).isEqualTo(3);
    }

    @Test
    @DisplayName("non-ACTIVE는 owner도 차단 (HIDDEN/BANNED/DELETED — admin 우회 방지)")
    void nonActive_blocksEvenOwner() {
        persistRecipe(owner, "active", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "banned", RecipeLifecycleStatus.BANNED, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), owner.getId(), null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("active");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("AI 레시피 + imageKey=null 차단 (운영 'completed recipes' 정책 동일)")
    void aiNoImageKey_excluded() {
        // AI + imageKey null → 차단
        persistRecipeWithFlags(owner, "ai-noimage", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, true, null);
        // AI + imageKey 있음 → 통과
        persistRecipeWithFlags(owner, "ai-with-image", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, true, "key.webp");
        // 사람 + imageKey null → 통과 (AI 조건만 차단)
        persistRecipeWithFlags(owner, "user-noimage", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, false, null);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), owner.getId(), null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle)
                .containsExactlyInAnyOrder("ai-with-image", "user-noimage");
    }

    @Test
    @DisplayName("imageReady 조건: PENDING/FAILED 차단 (READY 또는 null만 노출 — A2/A3 정합)")
    void imageReadyCondition_excludesPendingAndFailed() {
        persistRecipeWithFlags(owner, "ready", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY, false, null);
        persistRecipeWithFlags(owner, "nullImage", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, null, false, null);
        persistRecipeWithFlags(owner, "pending", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.PENDING, false, null);
        persistRecipeWithFlags(owner, "failed", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.FAILED, false, null);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findUserRecipesAccessible(owner.getId(), owner.getId(), null, PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle)
                .containsExactlyInAnyOrder("ready", "nullImage");
        assertThat(result.getTotalElements()).isEqualTo(2);
    }

    @Test
    @DisplayName("source filter: sourceTypes 명시 시 IN 절로 추가 필터")
    void sourceFilter_appliesInClause() {
        persistWithSource(owner, "user-recipe", RecipeSourceType.USER);
        persistWithSource(owner, "ai-recipe", RecipeSourceType.AI);
        persistWithSource(owner, "youtube-recipe", RecipeSourceType.YOUTUBE);
        em.flush(); em.clear();

        // USER만
        Page<Recipe> result = repo.findUserRecipesAccessible(
                owner.getId(), owner.getId(), List.of(RecipeSourceType.USER), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("user-recipe");

        // USER + YOUTUBE
        Page<Recipe> result2 = repo.findUserRecipesAccessible(
                owner.getId(), owner.getId(), List.of(RecipeSourceType.USER, RecipeSourceType.YOUTUBE), PAGE_10);

        assertThat(result2.getContent()).extracting(Recipe::getTitle)
                .containsExactlyInAnyOrder("user-recipe", "youtube-recipe");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder().provider("test").oauthId(oauthId).nickname(nickname).role(Role.USER).build();
        em.persist(user);
        return user;
    }

    private Recipe persistRecipe(User user, String title,
                                  RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                  RecipeListingStatus listing) {
        return persistRecipeWithFlags(user, title, lifecycle, visibility, listing,
                RecipeImageStatus.READY, false, null);
    }

    private Recipe persistRecipeWithFlags(User user, String title,
                                           RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                           RecipeListingStatus listing, RecipeImageStatus imageStatus,
                                           boolean ai, String imageKey) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .isAiGenerated(ai)
                .imageKey(imageKey)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private Recipe persistWithSource(User user, String title, RecipeSourceType source) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(source)
                .imageStatus(RecipeImageStatus.READY)
                .build();
        em.persist(recipe);
        return recipe;
    }
}
