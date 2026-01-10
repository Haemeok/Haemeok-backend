package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.YoutubeTargetChannel;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface YoutubeTargetChannelRepository extends JpaRepository<YoutubeTargetChannel, Long> {
    List<YoutubeTargetChannel> findAllByIsActiveTrue();
}