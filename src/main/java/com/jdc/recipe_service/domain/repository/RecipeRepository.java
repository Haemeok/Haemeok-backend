package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import jakarta.persistence.QueryHint;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository
public interface RecipeRepository extends JpaRepository<Recipe, Long>, RecipeQueryRepository {


    @Query("""
                SELECT r FROM Recipe r
                JOIN FETCH r.user
                WHERE r.id = :recipeId
            """)
    Optional<Recipe> findWithUserById(@Param("recipeId") Long recipeId);

    @EntityGraph(attributePaths = {
            "user",
            "fineDiningDetails",
            "ingredients",
            "ingredients.ingredient",
            "tags.tag"
    })
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.id = :recipeId
        """)
    Optional<Recipe> findDetailWithFineDiningById(@Param("recipeId") Long recipeId);

    @Query("""
                SELECT r FROM Recipe r
                LEFT JOIN FETCH r.steps
                WHERE r.id = :recipeId
            """)
    Optional<Recipe> findWithStepsById(@Param("recipeId") Long recipeId);

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    Page<Recipe> findByUserId(Long userId, Pageable pageable);

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    Page<Recipe> findByUserIdAndIsPrivateFalse(Long userId, Pageable pageable);

    Optional<Recipe> findById(Long id);

    @EntityGraph(attributePaths = {
            "user",
            "tags.tag"
    })
    Optional<Recipe> findWithAllRelationsById(Long id);


    @Query("SELECT r.id, r.likeCount FROM Recipe r WHERE r.id IN :ids")
    List<Object[]> findLikeCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findLikeCountsMapByIds(List<Long> ids) {
        return findLikeCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Query("""
            SELECT r.id, COUNT(rc.id)
            FROM Recipe r
            LEFT JOIN RecipeComment rc ON rc.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findCommentCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findCommentCountsMapByIds(List<Long> ids) {
        return findCommentCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Query("""
            SELECT r.id, COALESCE(AVG(rr.rating), 0.0)
            FROM Recipe r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findAvgRatingsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Double> findAvgRatingsMapByIds(List<Long> ids) {
        return findAvgRatingsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Double) a[1] : 0.0
                ));
    }

    @Query("""
            SELECT r.id, COUNT(rr.id)
            FROM Recipe r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findRatingCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findRatingCountsMapByIds(List<Long> ids) {
        return findRatingCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Modifying(clearAutomatically = true)
    @Query("""
        UPDATE Recipe r 
        SET r.weeklyLikeCount = (
            SELECT COUNT(rl)
            FROM RecipeLike rl
            WHERE rl.recipe.id = r.id
              AND rl.createdAt >= :startDate
        )
    """)
    void updateAllWeeklyLikeCounts(@Param("startDate") java.time.LocalDateTime startDate);

    @SuppressWarnings("JpaQlInspection")
    @Query("""
        SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
            r.id, 
            r.title, 
            r.imageKey, 
            r.user.id, 
            r.user.nickname, 
            r.user.profileImage, 
            r.createdAt, 
            r.cookingTime,
            r.weeklyLikeCount,       
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.youtubeChannelName,
            r.youtubeChannelId,
            r.youtubeVideoTitle,
            r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl,
            r.youtubeSubscriberCount,
            r.youtubeUrl,
            r.isAiGenerated
        )
        FROM Recipe r
        WHERE r.isPrivate = false
          AND r.isAiGenerated = false
        ORDER BY r.weeklyLikeCount DESC, r.createdAt DESC
    """)
    Page<RecipeSimpleStaticDto> findPopularRecipesStaticV2(Pageable pageable);

    @SuppressWarnings("JpaQlInspection")
    @QueryHints({
            @QueryHint(name = "jakarta.persistence.cache.retrieveMode", value = "BYPASS")
    })
    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                    r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                    COUNT(DISTINCT rl.id),
                    COALESCE(r.avgRating, 0.0),
                    COALESCE(r.ratingCount, 0L),
                    r.youtubeChannelName,
                    r.youtubeChannelId,
                    r.youtubeVideoTitle,
                    r.youtubeThumbnailUrl,
                    r.youtubeChannelProfileUrl,
                    r.youtubeSubscriberCount,
                    r.youtubeUrl,
                    r.isAiGenerated
                )
                FROM Recipe r
                JOIN r.user u
                LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
                WHERE r.isPrivate = false
                  AND r.isAiGenerated = false
                GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime, r.avgRating, r.ratingCount
                ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleStaticDto> findPopularRecipesByRealtimeCount(
            @Param("startDate") LocalDateTime startDate,
            Pageable pageable);

    @SuppressWarnings("JpaQlInspection")
    @Query("""
        SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2(
            r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
            COALESCE(r.likeCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.totalIngredientCost,
            r.marketPrice,
            r.youtubeChannelName,
            r.youtubeChannelId,
            r.youtubeVideoTitle,
            r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl,
            r.youtubeSubscriberCount,
            r.youtubeUrl,
            r.isAiGenerated
        )
        FROM Recipe r
        JOIN r.user u
        WHERE r.isPrivate = false
          AND r.isAiGenerated = false
          AND r.totalIngredientCost <= :maxCost
        ORDER BY r.totalIngredientCost ASC, r.createdAt DESC
        """)
    Page<RecipeSimpleStaticDtoV2> findBudgetRecipesStaticV2(
            @Param("maxCost") Integer maxCost,
            Pageable pageable);


    @SuppressWarnings("JpaQlInspection")
    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                COALESCE(r.likeCount, 0L),
                COALESCE(r.avgRating, 0.0),
                COALESCE(r.ratingCount, 0L),
                r.youtubeChannelName,
                r.youtubeChannelId,
                r.youtubeVideoTitle,
                r.youtubeThumbnailUrl,
                r.youtubeChannelProfileUrl,
                r.youtubeSubscriberCount,
                r.youtubeUrl,
                r.isAiGenerated
            )
            FROM Recipe r
            JOIN r.user u
            WHERE r.id IN :ids
            """)
    List<RecipeSimpleStaticDto> findAllSimpleStaticByIds(@Param("ids") List<Long> ids);

    @Query("""
            SELECT r
            FROM Recipe r
            JOIN FETCH r.user
            WHERE r.id IN :ids
              AND r.isPrivate = false
            """)
    List<Recipe> findAllByIdInAndIsPrivateFalseFetchUser(@Param("ids") List<Long> ids);

    @Query("SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(" +
            "r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, " +
            "r.likeCount, FALSE, r.cookingTime, r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl, r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, " +
            "COALESCE(ROUND(r.avgRating, 2), 0.0d), r.ratingCount, r.youtubeUrl, r.isAiGenerated)" +
            "FROM Recipe r " +
            "WHERE r.id IN :ids")
    List<RecipeSimpleDto> findAllSimpleDtoWithCountsByIdIn(@Param("ids") List<Long> ids);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.cookingTips = :tips, r.marketPrice = :price, r.aiAnalysisStatus = :status WHERE r.id = :id")
    void updateAiAnalysisResult(
            @Param("id") Long id,
            @Param("tips") String tips,
            @Param("price") Integer price,
            @Param("status") String status
    );

    @Query("""
            SELECT r.id
            FROM Recipe r
            WHERE r.isPrivate = false
              AND r.isAiGenerated = false
            ORDER BY r.avgRating DESC, r.createdAt DESC
            """)
    Page<Long> findCandidateIdsForRecommendation(Pageable pageable);

    @EntityGraph(attributePaths = {
            "user",
            "tags",
            "tags.tag",
            "ingredients",
            "ingredients.ingredient",
            "fineDiningDetails"
    })
    @Query("""
            SELECT r
            FROM Recipe r
            WHERE r.id IN :ids
            """)
    List<Recipe> findCandidatesForRecommendationByIds(@Param("ids") List<Long> ids);

    @EntityGraph(attributePaths = {
            "user",
            "tags",
            "tags.tag",
            "ingredients",
            "ingredients.ingredient",
            "fineDiningDetails"
    })
    @Query("""
            SELECT r
            FROM Recipe r
            WHERE r.id = :recipeId
            """)
    Optional<Recipe> findForRecommendationById(@Param("recipeId") Long recipeId);

    @Query("""
            SELECT r.id
            FROM Recipe r
            WHERE r.isPrivate = false
              AND r.isAiGenerated = false
            """)
    List<Long> findAllPublicRecipeIds();

    @Query("""
            SELECT r.id
            FROM Recipe r
            WHERE r.dishType IN :dishTypes
              AND r.isPrivate = false
              AND r.isAiGenerated = false
            """)
    List<Long> findIdsByDishTypeIn(@Param("dishTypes") List<DishType> dishTypes);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM Recipe r WHERE r.id = :id")
    void deleteByIdDirectly(@Param("id") Long id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.imageKey = :imageKey, r.imageStatus = :status, r.isPrivate = :isPrivate WHERE r.id = :id")
    void updateImageInfo(@Param("id") Long id, @Param("imageKey") String imageKey,
                         @Param("status") RecipeImageStatus status, @Param("isPrivate") Boolean isPrivate);

    Optional<Recipe> findByYoutubeUrl(String youtubeUrl);
}