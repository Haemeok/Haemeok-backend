package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
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

    @Query("""
                SELECT r FROM Recipe r
                LEFT JOIN FETCH r.steps
                WHERE r.id = :recipeId
            """)
    Optional<Recipe> findWithStepsById(@Param("recipeId") Long recipeId);

    Page<Recipe> findByUserId(Long userId, Pageable pageable);


    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt,
                COUNT(DISTINCT rl.id), false,
                r.cookingTime,
                COALESCE(AVG(rr.rating), 0.0d),
                COUNT(rr.id)
            )
            FROM Recipe r
            JOIN r.user u
            JOIN r.tags rt
            LEFT JOIN RecipeLike rl ON rl.recipe = r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE rt.tag = :tag
            AND r.isPrivate = false
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime
            """)
    Page<RecipeSimpleDto> findByTagWithLikeCount(@Param("tag") TagType tag, Pageable pageable);


    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt,
                COUNT(DISTINCT rl.id), false,
                r.cookingTime,
                COALESCE(AVG(rr.rating), 0.0d),
                COUNT(rr.id)
            )
            FROM Recipe r
            JOIN r.user u
            LEFT JOIN RecipeLike rl ON rl.recipe = r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.dishType = :dishType
            AND r.isPrivate = false
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime
            """)
    Page<RecipeSimpleDto> findByDishTypeWithLikeCount(@Param("dishType") DishType dishType, Pageable pageable);


    Optional<Recipe> findById(Long id);

    @EntityGraph(attributePaths = {
            "user",
            "tags.tag"
    })
    Optional<Recipe> findWithAllRelationsById(Long id);


    @Query("SELECT r.id, COUNT(rl.id) FROM Recipe r LEFT JOIN r.likes rl WHERE r.id IN :ids GROUP BY r.id")
    List<Object[]> findLikeCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findLikeCountsMapByIds(List<Long> ids) {
        return findLikeCountsByIdsRaw(ids).stream().collect(Collectors.toMap(a -> (Long) a[0], a -> (Long) a[1]));
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
        return findCommentCountsByIdsRaw(ids).stream().collect(Collectors.toMap(a -> (Long) a[0], a -> (Long) a[1]));
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
        return findAvgRatingsByIdsRaw(ids).stream().collect(Collectors.toMap(a -> (Long) a[0], a -> (Double) a[1]));
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
        return findRatingCountsByIdsRaw(ids).stream().collect(Collectors.toMap(a -> (Long) a[0], a -> (Long) a[1]));
    }

    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
                    r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt,
                    COUNT(DISTINCT rl.id), false,
                    r.cookingTime,
                    COALESCE(AVG(rr.rating), 0.0d),
                    COUNT(rr.id)
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
                COUNT(DISTINCT rl.id), false,
                r.cookingTime,
                COALESCE(AVG(rr.rating), 0.0d),
                COUNT(rr.id)
            )
            FROM Recipe r
            JOIN r.user u
            LEFT JOIN RecipeLike rl ON rl.recipe = r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.isPrivate = false
            AND r.totalIngredientCost <= :maxCost
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime
            ORDER BY r.totalIngredientCost ASC, COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleDto> findBudgetRecipes(
            @Param("maxCost") Integer maxCost,
            Pageable pageable);

    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                    r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                    COUNT(DISTINCT rl.id),
                    COALESCE(AVG(rr.rating), 0.0d),
                    COUNT(rr.id)
                )
                FROM Recipe r
                JOIN r.user u
                LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
                LEFT JOIN RecipeRating rr ON rr.recipe = r
                WHERE r.isPrivate = false
                GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime
                ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleStaticDto> findPopularRecipesStatic(
            @Param("startDate") LocalDateTime startDate,
            Pageable pageable);


    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                COUNT(DISTINCT rl.id),
                COALESCE(AVG(rr.rating), 0.0d),
                COUNT(rr.id)
            )
            FROM Recipe r
            JOIN r.user u
            LEFT JOIN RecipeLike rl ON rl.recipe = r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.isPrivate = false
            AND r.totalIngredientCost <= :maxCost
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime
            ORDER BY r.totalIngredientCost ASC, r.createdAt DESC
            """)
    Page<RecipeSimpleStaticDto> findBudgetRecipesStatic(
            @Param("maxCost") Integer maxCost,
            Pageable pageable);


    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                COUNT(DISTINCT rl.id),
                COALESCE(AVG(rr.rating), 0.0d), 
                COUNT(rr.id)                  
            )
            FROM Recipe r
            JOIN r.user u 
            LEFT JOIN RecipeLike rl ON rl.recipe = r 
            LEFT JOIN RecipeRating rr ON rr.recipe = r 
            WHERE r.id IN :ids
            GROUP BY r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime 
            """)
    List<RecipeSimpleStaticDto> findAllSimpleStaticByIds(@Param("ids") List<Long> ids);
}