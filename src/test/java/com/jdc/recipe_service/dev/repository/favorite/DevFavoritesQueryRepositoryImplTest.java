package com.jdc.recipe_service.dev.repository.favorite;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
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
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevFavoritesQueryRepositoryImpl 검증 (H2).
 *
 * 핵심:
 *  1. viewer가 자기 RESTRICTED 즐겨찾기 → 노출 (owner 분기)
 *  2. **다른 사람이 만든 RESTRICTED/PRIVATE 즐겨찾기 → 차단** (운영보다 더 엄격 — 핵심 invariant)
 *  3. non-ACTIVE는 모두 차단 (HIDDEN/BANNED/DELETED — 즐겨찾기 추가했어도)
 *  4. imageReady (PENDING/FAILED 차단 — 운영의 PENDING 노출과 다름)
 *  5. 다른 사용자의 favorite은 안 보임 (user.id 필터)
 *  6. count/content 일관성
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevFavoritesQueryRepositoryImpl.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevFavoritesQueryRepositoryImplTest {

    @Autowired EntityManager em;
    @Autowired DevFavoritesQueryRepository repo;

    private User favoriter;
    private User otherOwner;

    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        favoriter = persistUser("favoriter-oauth", "favoriter");
        otherOwner = persistUser("other-oauth", "other");
        em.flush();
    }

    @Test
    @DisplayName("viewer가 자기 RESTRICTED 즐겨찾기 → 노출 (owner 분기)")
    void ownRestricted_visible() {
        Recipe ownRestricted = persistRecipe(favoriter, "own-restricted",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, RecipeImageStatus.READY);
        persistFavorite(favoriter, ownRestricted);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("own-restricted");
    }

    @Test
    @DisplayName("**다른 사람이 만든 RESTRICTED/PRIVATE 즐겨찾기 → 차단** (운영보다 엄격한 dev V3 핵심 invariant)")
    void othersRestrictedAndPrivate_blocked() {
        // favoriter가 다른 사람의 PUBLIC/RESTRICTED/PRIVATE 모두 즐겨찾기 추가
        Recipe othersPub = persistRecipe(otherOwner, "others-pub",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        Recipe othersRestricted = persistRecipe(otherOwner, "others-restricted",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, RecipeImageStatus.READY);
        Recipe othersPrivate = persistRecipe(otherOwner, "others-private",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED, RecipeImageStatus.READY);
        persistFavorite(favoriter, othersPub);
        persistFavorite(favoriter, othersRestricted);
        persistFavorite(favoriter, othersPrivate);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        // 다른 사람 거 PUBLIC만 노출. RESTRICTED/PRIVATE은 즐겨찾기 추가했어도 차단.
        // (운영은 isPrivate=false 단일 필터라 RESTRICTED도 노출됨 — dev가 운영보다 엄격한 정책)
        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("others-pub");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("non-ACTIVE 즐겨찾기 차단 (HIDDEN/BANNED — admin 조치 후에도 즐겨찾기에 남았어도)")
    void nonActive_blocked() {
        Recipe hidden = persistRecipe(otherOwner, "hidden",
                RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        Recipe banned = persistRecipe(favoriter, "own-banned", // 본인 거여도 BANNED는 차단
                RecipeLifecycleStatus.BANNED, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        Recipe ok = persistRecipe(otherOwner, "ok",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        persistFavorite(favoriter, hidden);
        persistFavorite(favoriter, banned);
        persistFavorite(favoriter, ok);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle).containsExactly("ok");
    }

    @Test
    @DisplayName("imageReady 통일: PENDING/FAILED 차단 (운영의 PENDING 노출과 다름 — A2/A3 정합)")
    void imageNotReady_blocked() {
        Recipe ready = persistRecipe(otherOwner, "ready",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        Recipe nullImage = persistRecipe(otherOwner, "nullImage",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, null);
        Recipe pending = persistRecipe(otherOwner, "pending",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.PENDING);
        Recipe failed = persistRecipe(favoriter, "own-failed", // 본인 거여도 imageReady 통일
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.FAILED);
        persistFavorite(favoriter, ready);
        persistFavorite(favoriter, nullImage);
        persistFavorite(favoriter, pending);
        persistFavorite(favoriter, failed);
        em.flush(); em.clear();

        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        assertThat(result.getContent()).extracting(Recipe::getTitle)
                .containsExactlyInAnyOrder("ready", "nullImage");
    }

    @Test
    @DisplayName("default 정렬: favorite.createdAt DESC (운영 Page<RecipeFavorite> 시맨틱 — recipe.createdAt 아님)")
    void defaultSort_byFavoriteCreatedAtDesc_notRecipeCreatedAt() {
        // recipe 생성 순서와 favorite 추가 순서를 정확히 반대로 둠
        Recipe oldRecipe = persistRecipe(otherOwner, "old-recipe",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        Recipe newRecipe = persistRecipe(otherOwner, "new-recipe",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        RecipeFavorite earlierFavorite = persistFavoriteAndReturn(favoriter, newRecipe); // newRecipe 먼저 즐겨찾기
        RecipeFavorite laterFavorite = persistFavoriteAndReturn(favoriter, oldRecipe);   // oldRecipe 나중에 즐겨찾기
        em.flush();

        // JPA Auditing이 @PrePersist에 createdAt을 자동 set하므로, 명시 시각으로 강제하려면 JPQL UPDATE 필요.
        // (ReflectionTestUtils.setField만으로는 dirty checking이 안 잡혀 update SQL이 안 나감.)
        forceCreatedAt("Recipe", oldRecipe.getId(), LocalDateTime.of(2026, 1, 1, 0, 0));
        forceCreatedAt("Recipe", newRecipe.getId(), LocalDateTime.of(2026, 5, 1, 0, 0));
        forceCreatedAt("RecipeFavorite", earlierFavorite.getId(), LocalDateTime.of(2026, 6, 1, 0, 0));
        forceCreatedAt("RecipeFavorite", laterFavorite.getId(), LocalDateTime.of(2026, 6, 2, 0, 0));
        em.clear();

        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        // favorite.createdAt DESC: 나중에 즐겨찾기한 oldRecipe가 먼저.
        // (recipe.createdAt 기준이면 newRecipe가 먼저였을 것 — 회귀 시 순서 반대로 깨짐)
        assertThat(result.getContent()).extracting(Recipe::getTitle)
                .as("default sort=createdAt → favorite.createdAt DESC, NOT recipe.createdAt")
                .containsExactly("old-recipe", "new-recipe");
    }

    /** JPA Auditing이 set한 createdAt을 강제로 명시 시각으로 update — 정렬 의도 검증용. */
    private void forceCreatedAt(String entityName, Long id, LocalDateTime ts) {
        em.createQuery("UPDATE " + entityName + " e SET e.createdAt = :ts WHERE e.id = :id")
                .setParameter("ts", ts)
                .setParameter("id", id)
                .executeUpdate();
    }

    @Test
    @DisplayName("다른 사용자의 favorite은 안 보임 (user.id 필터)")
    void otherUsersFavorites_excluded() {
        Recipe ok = persistRecipe(otherOwner, "ok",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        // favoriter가 즐겨찾기 추가
        persistFavorite(favoriter, ok);
        // 다른 사용자도 같은 레시피 즐겨찾기
        persistFavorite(otherOwner, ok);
        em.flush(); em.clear();

        // favoriter 쿼리 — 다른 사용자 favorite 안 셈 (totalElements=1)
        Page<Recipe> result = repo.findFavoritesAccessible(favoriter.getId(), PAGE_10);

        assertThat(result.getContent()).hasSize(1);
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder().provider("test").oauthId(oauthId).nickname(nickname).role(Role.USER).build();
        em.persist(user);
        return user;
    }

    private Recipe persistRecipe(User owner, String title,
                                  RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                  RecipeListingStatus listing, RecipeImageStatus imageStatus) {
        Recipe recipe = Recipe.builder()
                .user(owner)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private void persistFavorite(User user, Recipe recipe) {
        persistFavoriteAndReturn(user, recipe);
    }

    private RecipeFavorite persistFavoriteAndReturn(User user, Recipe recipe) {
        RecipeFavorite favorite = RecipeFavorite.builder()
                .user(user)
                .recipe(recipe)
                .build();
        em.persist(favorite);
        return favorite;
    }
}
