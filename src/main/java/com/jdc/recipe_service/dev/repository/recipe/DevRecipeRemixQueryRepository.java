package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Dev V3 remix 목록 전용 repository.
 *
 * 운영 {@code RecipeRepository.findRemixesByOriginRecipeId}는 4-enum (PUBLIC + LISTED + ACTIVE + isPrivate=false)은
 * 적용하지만 {@code imageStatus} 조건이 없어 PENDING/FAILED 카드가 응답에 섞일 수 있다. dev V3는 다른 listing/추천 경로와
 * 정합을 위해 {@code imageReady} 필터 (READY OR NULL)도 SQL 단계에서 적용.
 *
 * content + count 쿼리의 WHERE 절은 항상 동일해야 한다 (count != list.size 불일치 방지).
 */
@Repository
public interface DevRecipeRemixQueryRepository extends JpaRepository<Recipe, Long> {

    @Query(value = """
            SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.favoriteCount,
                r.likeCount, FALSE, r.cookingTime, r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle,
                r.youtubeThumbnailUrl, r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount,
                COALESCE(ROUND(r.avgRating, 2), 0.0d), r.ratingCount, r.youtubeUrl, r.isAiGenerated)
            FROM Recipe r
            WHERE r.originRecipe.id = :originRecipeId
              AND r.isPrivate = false
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
              AND (r.imageStatus = com.jdc.recipe_service.domain.type.RecipeImageStatus.READY
                   OR r.imageStatus IS NULL)
            """,
            countQuery = """
                    SELECT COUNT(r) FROM Recipe r
                    WHERE r.originRecipe.id = :originRecipeId
                      AND r.isPrivate = false
                      AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
                      AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
                      AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
                      AND (r.imageStatus = com.jdc.recipe_service.domain.type.RecipeImageStatus.READY
                           OR r.imageStatus IS NULL)
                    """)
    Page<RecipeSimpleDto> findStrictRemixesByOriginRecipeId(
            @Param("originRecipeId") Long originRecipeId, Pageable pageable);
}
