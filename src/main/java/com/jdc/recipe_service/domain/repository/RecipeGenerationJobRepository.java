package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;

public interface RecipeGenerationJobRepository extends JpaRepository<RecipeGenerationJob, Long> {
    Optional<RecipeGenerationJob> findTopByUserIdOrderByCreatedAtDesc(Long userId);
    void deleteByResultRecipeId(Long recipeId);

    Optional<RecipeGenerationJob> findByIdempotencyKey(String idempotencyKey);

    Optional<RecipeGenerationJob> findByIdempotencyKeyAndDisplayModeAndStatusNot(
            String idempotencyKey,
            RecipeDisplayMode displayMode,
            JobStatus status
    );
}

