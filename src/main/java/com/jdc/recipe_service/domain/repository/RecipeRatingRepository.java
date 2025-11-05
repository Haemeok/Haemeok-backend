package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public interface RecipeRatingRepository extends JpaRepository<RecipeRating, Long> {
    Optional<RecipeRating> findByUserAndRecipe(User user, Recipe recipe);
    Optional<RecipeRating> findByUserIdAndRecipeId(Long userId, Long recipeId);

    @Query("SELECT COALESCE(AVG(r.rating), 0) FROM RecipeRating r WHERE r.recipe.id = :recipeId")
    double calculateAverageByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT COUNT(r) FROM RecipeRating r WHERE r.recipe.id = :recipeId")
    long countByRecipeId(@Param("recipeId") Long recipeId);

    @Modifying
    @Transactional
    @Query("DELETE FROM RecipeRating r WHERE r.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT rr.recipe.id, rr.rating FROM RecipeRating rr WHERE rr.user.id = :userId AND rr.recipe.id IN :recipeIds")
    List<Object[]> findRatingsByUserIdAndRecipeIdIn(@Param("userId") Long userId, @Param("recipeIds") List<Long> recipeIds);

    default Map<Long, Double> findRatingsMapByUserIdAndRecipeIdIn(Long userId, List<Long> recipeIds) {
        return findRatingsByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .collect(Collectors.toMap(
                        arr -> (Long) arr[0],
                        arr -> (Double) arr[1]
                ));
    }
}
