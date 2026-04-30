package com.jdc.recipe_service.domain.repository.meta;

import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface RecipeYoutubeInfoRepository extends JpaRepository<RecipeYoutubeInfo, Long> {
    boolean existsByRecipeId(Long recipeId);

    /** dev V3 facade에서 video_id 기반 사전 dedup용. unique constraint(uq_youtube_video_id)와 1:1 대응. */
    Optional<RecipeYoutubeInfo> findByVideoId(String videoId);

    /** dev V3 detail API에서 recipe_id 기반 분리 테이블 조회 (legacy Recipe.youtube_* fallback 판정에 사용). */
    Optional<RecipeYoutubeInfo> findByRecipeId(Long recipeId);
}
