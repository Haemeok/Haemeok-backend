package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.ActionLog;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ActionLogRepository extends JpaRepository<ActionLog, Long> {
}