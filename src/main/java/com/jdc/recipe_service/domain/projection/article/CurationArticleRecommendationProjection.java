package com.jdc.recipe_service.domain.projection.article;

/**
 * 추천 후보 조회용 projection. 본문 MDX/status/recipeIds 등은 가져오지 않는다 — 추천 카드가 필요한
 * 최소 컬럼만 select해 트래픽과 메모리 비용을 줄인다. id는 dedup/제외 로직에 필요해서 포함한다 (응답에는 미노출).
 */
public interface CurationArticleRecommendationProjection {
    Long getId();
    String getSlug();
    String getTitle();
    String getCoverImageKey();
    String getCategory();
}
