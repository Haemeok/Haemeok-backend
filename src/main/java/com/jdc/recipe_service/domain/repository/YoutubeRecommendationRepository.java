package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.YoutubeRecommendation;
import org.springframework.data.jpa.repository.JpaRepository;

public interface YoutubeRecommendationRepository extends JpaRepository<YoutubeRecommendation, Long> {

}
