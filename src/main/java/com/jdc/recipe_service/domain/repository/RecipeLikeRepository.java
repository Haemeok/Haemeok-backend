package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeLike;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RecipeLikeRepository extends JpaRepository<RecipeLike, Long> {

    int countByRecipeId(Long recipeId);
    boolean existsByRecipeIdAndUserId(Long recipeId, Long userId);

    List<RecipeLike> findByUserIdAndRecipeIdIn(Long userId, List<Long> recipeIds);

    Optional<RecipeLike> findByUserIdAndRecipeId(Long userId, Long recipeId);

    void deleteByUserIdAndRecipeId(Long userId, Long recipeId);
    void deleteByRecipeId(Long recipeId);

    @Query("SELECT rl.recipe.id, COUNT(rl) FROM RecipeLike rl WHERE rl.recipe.id IN :recipeIds GROUP BY rl.recipe.id")
    List<Object[]> countLikesRaw(@Param("recipeIds") List<Long> recipeIds);



}