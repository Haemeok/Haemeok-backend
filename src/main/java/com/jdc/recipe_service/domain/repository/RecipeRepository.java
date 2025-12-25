package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
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

    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                    r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt,
                    COUNT(DISTINCT rl.id), false,
                    r.cookingTime,
                    COALESCE(ROUND(AVG(rr.rating), 2), 0.0d),
                    COUNT(DISTINCT rr.id)
                )
                FROM Recipe r
                JOIN r.user u
                LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
                LEFT JOIN RecipeRating rr ON rr.recipe = r
                WHERE r.isPrivate = false
                GROUP BY r
                ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleDto> findPopularRecipesSince(
            @Param("startDate") LocalDateTime startDate,
            Pageable pageable);


    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt,
                r.likeCount, false,
                r.cookingTime,
                COALESCE(ROUND(AVG(rr.rating), 2), 0.0d),
                COUNT(DISTINCT rr.id)
            )
            FROM Recipe r
            JOIN r.user u
            LEFT JOIN RecipeLike rl ON rl.recipe = r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.isPrivate = false
            AND r.totalIngredientCost <= :maxCost
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime, r.likeCount
            ORDER BY r.totalIngredientCost ASC, COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleDto> findBudgetRecipes(
            @Param("maxCost") Integer maxCost,
            Pageable pageable);

    @SuppressWarnings("JpaQlInspection")
    @QueryHints({
            @QueryHint(name = "jakarta.persistence.cache.retrieveMode", value = "BYPASS")
    })
    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                    r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                    COUNT(DISTINCT rl.id),      
                    COALESCE(r.avgRating, 0.0), 
                    COALESCE(r.ratingCount, 0L) 
                )
                FROM Recipe r
                JOIN r.user u
                LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
                WHERE r.isPrivate = false
                  AND r.isAiGenerated = false
                GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime, r.avgRating, r.ratingCount
                ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleStaticDto> findPopularRecipesStaticV2(
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
            r.marketPrice
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
                COALESCE(r.ratingCount, 0L)
            )
            FROM Recipe r
            JOIN r.user u
            WHERE r.id IN :ids
            """)
    List<RecipeSimpleStaticDto> findAllSimpleStaticByIds(@Param("ids") List<Long> ids);

    Page<Recipe> findByFridgeFallback(
            List<Long> fridgeIds,
            AiRecipeFilter aiFilter,
            Pageable pageable
    );

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
            "r.likeCount, FALSE, r.cookingTime, COALESCE(ROUND(r.avgRating, 2), 0.0d), r.ratingCount) " +
            "FROM Recipe r " +
            "LEFT JOIN RecipeLike l ON l.recipe.id = r.id " +
            "WHERE r.id IN :ids " +
            "GROUP BY r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime, r.avgRating, r.ratingCount, r.likeCount")
    List<RecipeSimpleDto> findAllSimpleDtoWithCountsByIdIn(List<Long> ids);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.cookingTips = :tips, r.marketPrice = :price, r.aiAnalysisStatus = :status WHERE r.id = :id")
    void updateAiAnalysisResult(
            @Param("id") Long id,
            @Param("tips") String tips,
            @Param("price") Integer price,
            @Param("status") String status
    );

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.isPrivate = false
            ORDER BY r.avgRating DESC
            """)
    List<Recipe> findCandidatesForRecommendation(Pageable pageable);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM Recipe r WHERE r.id = :id")
    void deleteByIdDirectly(@Param("id") Long id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.imageKey = :imageKey, r.imageStatus = :status, r.isPrivate = :isPrivate WHERE r.id = :id")
    void updateImageInfo(@Param("id") Long id, @Param("imageKey") String imageKey,
                         @Param("status") RecipeImageStatus status, @Param("isPrivate") Boolean isPrivate);
}