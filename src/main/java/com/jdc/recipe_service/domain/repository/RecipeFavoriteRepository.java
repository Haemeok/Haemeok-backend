package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface RecipeFavoriteRepository extends JpaRepository<RecipeFavorite, Long> {
    Optional<RecipeFavorite> findByUserIdAndRecipeId(Long userId, Long recipeId);

    @EntityGraph(attributePaths = {"recipe", "recipe.user"})
    Page<RecipeFavorite> findByUserId(Long userId, Pageable pageable);

    boolean existsByRecipeIdAndUserId(Long recipeId, Long userId);

    void deleteByRecipeId(Long recipeId);

}