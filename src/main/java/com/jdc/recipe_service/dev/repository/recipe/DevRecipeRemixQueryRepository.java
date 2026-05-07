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
 * <p>V1.x 정책: 리믹스 글은 항상 {@code PUBLIC + UNLISTED + isPrivate=false}로 생성되므로 (RecipeService.create에서 강제),
 * 원본의 리믹스 목록은 <b>listingStatus 조건을 적용하면 모든 리믹스가 빠진다</b>. 따라서 listingStatus 필터는 제거하고
 * {@code ACTIVE + visibility=PUBLIC} 만 적용 — PRIVATE 전환된 리믹스만 제외, link-only 리믹스는 모두 노출.
 *
 * <p>운영 {@code RecipeRepository.findRemixesByOriginRecipeId}도 PR 3에서 같은 정책으로 정리됨 — listingStatus 조건 제거 완료.
 *
 * <p>또 운영 query에는 {@code imageStatus} 조건이 없어 PENDING/FAILED 카드가 응답에 섞일 수 있다. dev V3는 다른
 * listing/추천 경로와 정합을 위해 {@code imageReady} 필터 (READY OR NULL)도 SQL 단계에서 적용.
 *
 * <p>content + count 쿼리의 WHERE 절은 항상 동일해야 한다 (count != list.size 불일치 방지).
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
              AND (r.imageStatus = com.jdc.recipe_service.domain.type.RecipeImageStatus.READY
                   OR r.imageStatus IS NULL)
            """,
            countQuery = """
                    SELECT COUNT(r) FROM Recipe r
                    WHERE r.originRecipe.id = :originRecipeId
                      AND r.isPrivate = false
                      AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
                      AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
                      AND (r.imageStatus = com.jdc.recipe_service.domain.type.RecipeImageStatus.READY
                           OR r.imageStatus IS NULL)
                    """)
    Page<RecipeSimpleDto> findStrictRemixesByOriginRecipeId(
            @Param("originRecipeId") Long originRecipeId, Pageable pageable);
}
