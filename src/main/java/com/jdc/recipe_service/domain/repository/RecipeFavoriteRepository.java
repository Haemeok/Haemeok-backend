package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RecipeFavoriteRepository extends JpaRepository<RecipeFavorite, Long> {
    Optional<RecipeFavorite> findByUserIdAndRecipeId(Long userId, Long recipeId);
    List<RecipeFavorite> findByUserId(Long userId);

    boolean existsByRecipeIdAndUserId(Long recipeId, Long userId);
    long countByRecipeId(Long recipeId);

    void deleteByUserIdAndRecipeId(Long userId, Long recipeId);
    void deleteByRecipeId(Long recipeId);

}