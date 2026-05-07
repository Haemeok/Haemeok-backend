package com.jdc.recipe_service.service.article;

/**
 * service ↔ controller 내부 result. 외부 API 응답은 {@code CurationArticleCreateResponse}가 담당하고,
 * 이 record는 service에서 controller로 raw id + 확정된 slug를 넘기는 용도다 (HashID 인코딩은 controller가).
 */
public record CurationArticleCreateResult(Long id, String slug) {}
