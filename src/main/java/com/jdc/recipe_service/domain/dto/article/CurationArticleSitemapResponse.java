package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * 큐레이션 아티클 sitemap 응답 항목.
 *
 * <p>PUBLISHED 아티클의 sitemap.xml 생성에 필요한 최소 필드만 노출한다.
 * 프론트는 {@code slug}로 article URL을 만들고, {@code updatedAt}을 sitemap의 {@code <lastmod>}에 사용한다.
 *
 * <p>id, contentMdx, status, recipeIds 등은 의도적으로 제외 — sitemap은 검색엔진 크롤러용 외부 노출이라
 * 필요 이상 필드가 있으면 정보 누수면이 넓어진다.
 */
@Getter
@Builder
@Schema(description = "Public 큐레이션 아티클 sitemap 항목")
public class CurationArticleSitemapResponse {

    @Schema(description = "URL slug. 프론트가 article 상세 URL을 구성하는 데 사용",
            example = "summer-diet-cucumber-recipes")
    private String slug;

    @Schema(description = "마지막 수정 시각. sitemap.xml의 <lastmod>에 사용",
            example = "2026-05-05T10:20:00")
    private LocalDateTime updatedAt;
}
