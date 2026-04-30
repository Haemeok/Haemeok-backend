package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Dev V3 popular / budget JPA query repository.
 *
 * 운영 {@link com.jdc.recipe_service.domain.repository.RecipeRepository}의 동일 이름 query들을 미러링하되
 * {@code r.isPrivate = false} 자리에 dev V3 4-enum 정책 적용:
 *   {@code r.lifecycleStatus = ACTIVE && r.visibility = PUBLIC && r.listingStatus = LISTED}
 *
 * 정책 결정:
 *  - popular/budget은 anonymous-only (모든 사용자에게 동일 set) → owner 분기 없음
 *  - {@code r.isAiGenerated = false}는 운영 정책 그대로 유지 (popular/budget은 사람이 만든 레시피만)
 *  - dev에서는 cache 미적용 (검증 환경에서 stale 응답 사고 방지) — service 레이어에 @Cacheable 안 붙임
 *
 * 운영 RecipeRepository는 zero touch — Spring Data JPA가 별도 Bean으로 자동 등록.
 */
public interface DevRecipePopularBudgetRepository extends JpaRepository<Recipe, Long> {

    /** weekly column 기반 fast path. 운영 V2의 findPopularRecipesStaticV2 미러. */
    @Query("""
        SELECT new com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto(
            r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
            COALESCE(r.likeCount, 0L),
            COALESCE(r.favoriteCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount,
            r.youtubeUrl, r.isAiGenerated,
            r.visibility, r.listingStatus, r.lifecycleStatus, r.source
        )
        FROM Recipe r
        WHERE r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
        ORDER BY (COALESCE(r.weeklyLikeCount, 0) + COALESCE(r.weeklyFavoriteCount, 0)) DESC, r.createdAt DESC
    """)
    Page<DevRecipeSimpleStaticDto> findPopularDevWeekly(Pageable pageable);

    /** 실시간 COUNT 기반 monthly/all path. 운영 V2의 findPopularRecipesByRealtimeCount 미러. */
    @Query("""
        SELECT new com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto(
            r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
            COALESCE(r.likeCount, 0L),
            COALESCE(r.favoriteCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount,
            r.youtubeUrl, r.isAiGenerated,
            r.visibility, r.listingStatus, r.lifecycleStatus, r.source
        )
        FROM Recipe r
        JOIN r.user u
        LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
        WHERE r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
        GROUP BY
           r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime,
           r.likeCount, r.favoriteCount, r.avgRating, r.ratingCount,
           r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl,
           r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount, r.youtubeUrl, r.isAiGenerated,
           r.visibility, r.listingStatus, r.lifecycleStatus, r.source
        ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
    """)
    Page<DevRecipeSimpleStaticDto> findPopularDevByRealtimeCount(
            @Param("startDate") LocalDateTime startDate,
            Pageable pageable);

    /** budget 계산 시 제외할 top 10 인기 dev recipe ids. */
    @Query("""
        SELECT r.id
        FROM Recipe r
        WHERE r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
        ORDER BY (COALESCE(r.weeklyLikeCount, 0) + COALESCE(r.weeklyFavoriteCount, 0)) DESC, r.createdAt DESC
    """)
    List<Long> findTop10PopularDevIds(Pageable pageable);

    /** budget query — 운영 V2의 findBudgetRecipesStaticV2 미러. */
    @Query("""
        SELECT new com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2(
            r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
            COALESCE(r.likeCount, 0L),
            COALESCE(r.favoriteCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.totalIngredientCost,
            r.marketPrice,
            r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount,
            r.youtubeUrl, r.isAiGenerated,
            r.visibility, r.listingStatus, r.lifecycleStatus, r.source
        )
        FROM Recipe r
        JOIN r.user u
        WHERE r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
          AND r.totalIngredientCost <= :maxCost
          AND r.totalIngredientCost >= 1000
          AND r.id NOT IN :excludedIds
        ORDER BY ((COALESCE(r.favoriteCount, 0) * 0.3) + (COALESCE(r.weeklyFavoriteCount, 0) * 0.7)) DESC, r.createdAt DESC
    """)
    Page<DevRecipeSimpleStaticDtoV2> findBudgetDev(
            @Param("maxCost") Integer maxCost,
            @Param("excludedIds") List<Long> excludedIds,
            Pageable pageable);
}
