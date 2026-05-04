package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * 어드민 목록 응답. 본문 MDX는 포함하지 않는다 (목록 트래픽 절감).
 *
 * <p>id는 HashID 문자열로 직렬화된다 — wire에 raw Long 노출 금지 정책.
 */
@Getter
@Builder
@Schema(description = "큐레이션 아티클 목록용 요약 응답")
public class CurationArticleSummaryResponse {

    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    @Schema(description = "아티클 ID (HashID 문자열)", example = "xJvY7aBp", type = "string")
    private Long id;

    private String slug;
    private String title;
    private String description;
    @Schema(description = "커버 이미지 S3 imageKey (.webp)", example = "images/articles/xJvY7aBp/uuid.webp")
    private String coverImageKey;
    private String category;
    private ArticleStatus status;
    private boolean humanReviewed;
    private LocalDateTime publishedAt;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static CurationArticleSummaryResponse of(CurationArticle a) {
        return CurationArticleSummaryResponse.builder()
                .id(a.getId())
                .slug(a.getSlug())
                .title(a.getTitle())
                .description(a.getDescription())
                .coverImageKey(a.getCoverImageKey())
                .category(a.getCategory())
                .status(a.getStatus())
                .humanReviewed(a.isHumanReviewed())
                .publishedAt(a.getPublishedAt())
                .createdAt(a.getCreatedAt())
                .updatedAt(a.getUpdatedAt())
                .build();
    }
}
