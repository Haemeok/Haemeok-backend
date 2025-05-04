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

import java.util.Optional;

@Repository
public interface RecipeRepository extends JpaRepository<Recipe, Long>, RecipeQueryRepository {


    //로그인 조회
//    @EntityGraph(attributePaths = {"user"})
//    @Query("select r from Recipe r where r.id = :recipeId")
    @Query("""
    SELECT r FROM Recipe r
    JOIN FETCH r.user
    WHERE r.id = :recipeId
""")
    Optional<Recipe> findWithUserById(@Param("recipeId") Long recipeId);

    //사용자가 쓴 레시피 조회
    Page<Recipe> findByUserId(Long userId, Pageable pageable);


    // 태그 이름으로 조회 (RecipeTag → Tag)
    @Query("""
SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
    r.id, r.title, r.imageKey, u.nickname, u.profileImage, r.createdAt,
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
GROUP BY r.id, r.title, r.imageKey, u.nickname, u.profileImage, r.createdAt, r.cookingTime
""")
    Page<RecipeSimpleDto> findByTagWithLikeCount(@Param("tag") TagType tag, Pageable pageable);


    @Query("""
SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(
    r.id, r.title, r.imageKey, u.nickname, u.profileImage, r.createdAt,
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
GROUP BY r.id, r.title, r.imageKey, u.nickname, u.profileImage, r.createdAt, r.cookingTime
""")
    Page<RecipeSimpleDto> findByDishTypeWithLikeCount(@Param("dishType") DishType dishType, Pageable pageable);

}
