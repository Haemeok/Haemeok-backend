package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeActivityLog;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RecipeActivityLogRepository extends JpaRepository<RecipeActivityLog, Long> {
}