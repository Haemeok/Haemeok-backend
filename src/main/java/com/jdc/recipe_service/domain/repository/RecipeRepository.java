package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RecipeRepository extends JpaRepository<Recipe, Long> {
}
