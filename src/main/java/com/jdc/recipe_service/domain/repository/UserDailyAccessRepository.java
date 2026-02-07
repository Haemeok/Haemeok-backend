package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.UserDailyAccess;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserDailyAccessRepository extends JpaRepository<UserDailyAccess, Long> {
}