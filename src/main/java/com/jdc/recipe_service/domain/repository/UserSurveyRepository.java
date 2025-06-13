package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.UserSurvey;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface UserSurveyRepository extends JpaRepository<UserSurvey, Long> {
    @Query("SELECT us FROM UserSurvey us WHERE us.user.id = :userId")
    Optional<UserSurvey> findByUserId(@Param("userId") Long userId);
}
