package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeTrashLog;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RecipeTrashLogRepository extends JpaRepository<RecipeTrashLog, Long> {
}