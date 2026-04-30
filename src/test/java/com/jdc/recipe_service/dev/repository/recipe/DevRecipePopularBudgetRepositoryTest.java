package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
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
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipePopularBudgetRepository의 JPQL 4개 동작 검증 (H2).
 *
 * 핵심:
 *  1. JPQL constructor expression이 dev DTO에 정확히 매칭 (BigDecimal avgRating + long primitive 시그니처)
 *  2. PUBLIC+LISTED+ACTIVE + isAiGenerated=false 정책 — RESTRICTED/PRIVATE/HIDDEN/BANNED/AI 누수 차단
 *  3. weekly column path와 realtime count path 모두 같은 필터 적용
 *  4. budget cost 범위 + excludedIds + 4 enum 매핑
 *
 * @DataJpaTest + H2. dev repository 자동 wiring.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevRecipePopularBudgetRepositoryTest {

    @Autowired EntityManager em;
    @Autowired DevRecipePopularBudgetRepository repo;

    private User owner;

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        em.flush();
    }

    // ---------- findPopularDevWeekly ----------

    @Test
    @DisplayName("findPopularDevWeekly: PUBLIC+LISTED+ACTIVE만 SELECT + 4 enum 응답에 매핑 + weekly 정렬")
    void findPopularDevWeekly_publicListedActiveOnly_with4EnumMapped_weeklySort() {
        persistPopular(owner, "low", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 5L, 5L);
        persistPopular(owner, "mid", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 50L, 30L);
        persistPopular(owner, "high", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.findPopularDevWeekly(PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle)
                .containsExactly("high", "mid", "low"); // weeklyLikeCount + weeklyFavoriteCount DESC

        // 4 enum 응답에 매핑
        DevRecipeSimpleStaticDto first = result.getContent().get(0);
        assertThat(first.getVisibility()).isEqualTo("PUBLIC");
        assertThat(first.getListingStatus()).isEqualTo("LISTED");
        assertThat(first.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(first.getSource()).isEqualTo("USER");
    }

    @Test
    @DisplayName("findPopularDevWeekly: RESTRICTED/PRIVATE/HIDDEN/BANNED/AI 누수 차단")
    void findPopularDevWeekly_excludesAllNonPolicyMatchingRows() {
        persistPopular(owner, "ok", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        persistPopular(owner, "private", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED, 100L, 50L);
        persistPopular(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, 100L, 50L);
        persistPopular(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        persistPopular(owner, "banned", RecipeLifecycleStatus.BANNED, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        persistAiPopular(owner, "ai", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.findPopularDevWeekly(PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("ok");
    }

    // ---------- findPopularDevByRealtimeCount ----------

    @Test
    @DisplayName("findPopularDevByRealtimeCount: 같은 정책 필터 적용 (RESTRICTED/AI 누수 차단)")
    void findPopularDevByRealtimeCount_appliesSamePolicyFilter() {
        persistPopular(owner, "ok", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 0L, 0L);
        persistPopular(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, 0L, 0L);
        persistAiPopular(owner, "ai", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 0L, 0L);
        em.flush();
        em.clear();

        // RecipeLike fixture 없이도 query 자체는 실행됨 (LEFT JOIN + COUNT=0)
        Page<DevRecipeSimpleStaticDto> result = repo.findPopularDevByRealtimeCount(
                LocalDateTime.of(2000, 1, 1, 0, 0), PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("ok");
    }

    // ---------- findTop10PopularDevIds ----------

    @Test
    @DisplayName("findTop10PopularDevIds: PUBLIC+LISTED+ACTIVE id만 반환 + weekly 정렬")
    void findTop10PopularDevIds_returnsOnlyPolicyMatchingIds() {
        Recipe high = persistPopular(owner, "high", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 100L, 50L);
        Recipe low = persistPopular(owner, "low", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, 5L, 5L);
        Recipe restricted = persistPopular(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, 200L, 200L);
        em.flush();
        em.clear();

        List<Long> ids = repo.findTop10PopularDevIds(PageRequest.of(0, 10));

        assertThat(ids).containsExactly(high.getId(), low.getId());
        assertThat(ids).doesNotContain(restricted.getId());
    }

    // ---------- findBudgetDev ----------

    @Test
    @DisplayName("findBudgetDev: cost 범위 [1000, maxCost] + excludedIds 제외 + 4 enum + cost/marketPrice 매핑")
    void findBudgetDev_costRangeAndExcludedIds_with4EnumMapped() {
        Recipe excluded = persistBudget(owner, "excluded", 5000, 8000); // 인기 top10이라 가정 → 제외
        Recipe inRange = persistBudget(owner, "inRange", 5000, 8000);
        Recipe tooCheap = persistBudget(owner, "tooCheap", 500, 1000); // 1000 미만 제외
        Recipe tooExpensive = persistBudget(owner, "tooExpensive", 20000, 25000); // maxCost=10000 초과 제외
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDtoV2> result = repo.findBudgetDev(
                10000, List.of(excluded.getId()), PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDtoV2::getTitle).containsExactly("inRange");
        DevRecipeSimpleStaticDtoV2 dto = result.getContent().get(0);
        assertThat(dto.getIngredientCost()).isEqualTo(5000);
        assertThat(dto.getMarketPrice()).isEqualTo(8000);
        // 4 enum
        assertThat(dto.getVisibility()).isEqualTo("PUBLIC");
        assertThat(dto.getListingStatus()).isEqualTo("LISTED");
        assertThat(dto.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(dto.getSource()).isEqualTo("USER");
    }

    @Test
    @DisplayName("findBudgetDev: RESTRICTED/PRIVATE/HIDDEN/AI 누수 차단")
    void findBudgetDev_excludesNonPolicyMatchingRows() {
        Recipe ok = persistBudget(owner, "ok", 5000, 8000);
        // 정책 위반 row들도 cost 범위는 만족하지만 정책 필터에서 차단되어야
        persistRecipeWithVisibility(owner, "private", 5000, 8000, RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED, false);
        persistRecipeWithVisibility(owner, "restricted", 5000, 8000, RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED, false);
        persistRecipeWithVisibility(owner, "hidden", 5000, 8000, RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, false);
        persistRecipeWithVisibility(owner, "ai", 5000, 8000, RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, true);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDtoV2> result = repo.findBudgetDev(
                10000, List.of(-1L), PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDtoV2::getTitle).containsExactly("ok");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder()
                .provider("test")
                .oauthId(oauthId)
                .nickname(nickname)
                .role(Role.USER)
                .build();
        em.persist(user);
        return user;
    }

    private Recipe persistPopular(User user, String title,
                                   RecipeLifecycleStatus lifecycle,
                                   RecipeVisibility visibility,
                                   RecipeListingStatus listing,
                                   long weeklyLikeCount, long weeklyFavoriteCount) {
        return persistRecipeFull(user, title, 5000, 8000, lifecycle, visibility, listing, false,
                weeklyLikeCount, weeklyFavoriteCount, 0L);
    }

    private Recipe persistAiPopular(User user, String title,
                                     RecipeLifecycleStatus lifecycle,
                                     RecipeVisibility visibility,
                                     RecipeListingStatus listing,
                                     long weeklyLikeCount, long weeklyFavoriteCount) {
        return persistRecipeFull(user, title, 5000, 8000, lifecycle, visibility, listing, true,
                weeklyLikeCount, weeklyFavoriteCount, 0L);
    }

    private Recipe persistBudget(User user, String title, int ingredientCost, int marketPrice) {
        return persistRecipeFull(user, title, ingredientCost, marketPrice,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, false,
                0L, 10L, 5L);
    }

    private Recipe persistRecipeWithVisibility(User user, String title, int ingredientCost, int marketPrice,
                                                RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                                RecipeListingStatus listing, boolean ai) {
        return persistRecipeFull(user, title, ingredientCost, marketPrice, lifecycle, visibility, listing, ai,
                0L, 10L, 5L);
    }

    private Recipe persistRecipeFull(User user, String title, int ingredientCost, int marketPrice,
                                      RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                      RecipeListingStatus listing, boolean ai,
                                      long weeklyLikeCount, long weeklyFavoriteCount, long favoriteCount) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .totalIngredientCost(ingredientCost)
                .marketPrice(marketPrice)
                .weeklyLikeCount(weeklyLikeCount)
                .weeklyFavoriteCount(weeklyFavoriteCount)
                .favoriteCount(favoriteCount)
                .isAiGenerated(ai)
                .build();
        em.persist(recipe);
        return recipe;
    }
}
