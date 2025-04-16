package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
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

import java.util.List;
import java.util.Optional;

@Repository
public interface RecipeRepository extends JpaRepository<Recipe, Long>, RecipeQueryRepository {

    //RecipeRepository.java (목록 조회 시 좋아요 수 포함)
    @Query("SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(" +
            " r.id, r.title, r.imageUrl, r.user.nickname, r.createdAt, COUNT(rl), false) " +
            "FROM Recipe r LEFT JOIN RecipeLike rl ON rl.recipe.id = r.id " +
            "GROUP BY r.id ORDER BY r.createdAt DESC")
    List<RecipeSimpleDto> findAllWithLikeCount();

    //상세사항 조회
    @Query("select r from Recipe r " +
            "join fetch r.user " +
            "left join fetch r.ingredients ri " +
            "left join fetch ri.ingredient " +
            "left join fetch r.tags rt " +
            "left join fetch rt.tag " +
            "where r.id = :recipeId")
    Recipe findDetailedRecipeById(@Param("recipeId") Long recipeId);

    @Query("SELECT r FROM Recipe r LEFT JOIN FETCH r.ingredients WHERE r.id = :recipeId")
    Optional<Recipe> findWithIngredientsById(@Param("recipeId") Long recipeId);

    @Query("select distinct r from Recipe r " +
            "left join fetch r.steps s " +
            "left join fetch s.stepIngredients si " +
            "left join fetch si.ingredient i " +
            "where r.id = :recipeId")
    Optional<Recipe> findWithStepsById(Long recipeId);


    //로그인 조회
    @EntityGraph(attributePaths = {"user"})
    @Query("select r from Recipe r where r.id = :recipeId")
    Optional<Recipe> findWithUserById(@Param("recipeId") Long recipeId);

    //사용자가 쓴 레시피 조회
    Page<Recipe> findByUserId(Long userId, Pageable pageable);

    // dishType 기반 카테고리 조회
    List<Recipe> findByDishType(DishType dishType, Pageable pageable);

    // 태그 이름으로 조회 (RecipeTag → Tag)
    @Query("""
    SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
        r.id, r.title, r.imageUrl, u.nickname, r.createdAt, COUNT(rl.id), false
    )
    FROM Recipe r
    JOIN r.user u
    JOIN r.tags rt
    LEFT JOIN RecipeLike rl ON rl.recipe = r
    WHERE rt.tag = :tag
    GROUP BY r.id, u.nickname, r.title, r.imageUrl, r.createdAt
""")
    Page<RecipeSimpleDto> findByTagWithLikeCount(@Param("tag") TagType tag, Pageable pageable);


    @Query("""
    SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
        r.id, r.title, r.imageUrl, u.nickname, r.createdAt, COUNT(rl.id), false
    )
    FROM Recipe r
    JOIN r.user u
    LEFT JOIN RecipeLike rl ON rl.recipe = r
    WHERE r.dishType = :dishType
    GROUP BY r.id, u.nickname, r.title, r.imageUrl, r.createdAt
""")
    Page<RecipeSimpleDto> findByDishTypeWithLikeCount(@Param("dishType") DishType dishType, Pageable pageable);

}
