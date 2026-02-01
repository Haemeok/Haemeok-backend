package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeGenerationJob;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;

public interface RecipeGenerationJobRepository extends JpaRepository<RecipeGenerationJob, Long> {
    Optional<RecipeGenerationJob> findTopByUserIdOrderByCreatedAtDesc(Long userId);
    void deleteByResultRecipeId(Long recipeId);

    Optional<RecipeGenerationJob> findByIdempotencyKey(String idempotencyKey);
}