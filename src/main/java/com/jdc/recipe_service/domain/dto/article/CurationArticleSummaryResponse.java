package com.jdc.recipe_service.domain.dto.article;

import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * 어드민 목록 응답. 본문 MDX는 포함하지 않는다 (목록 트래픽 절감).
 */
@Getter
@Builder
@Schema(description = "큐레이션 아티클 목록용 요약 응답")
public class CurationArticleSummaryResponse {

    private Long id;
    private String slug;
    private String title;
    private String description;
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
