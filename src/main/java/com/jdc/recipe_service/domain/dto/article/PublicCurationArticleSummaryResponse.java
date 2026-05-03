package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * Public 큐레이션 아티클 목록 응답.
 *
 * <p>본문 MDX와 운영 전용 필드(status/generatedBy/humanReviewed)는 의도적으로 제외해 트래픽과 노출 위험을 줄인다.
 */
@Getter
@Builder
@Schema(description = "Public 큐레이션 아티클 목록 응답")
public class PublicCurationArticleSummaryResponse {

    /**
     * Public 응답에서는 HashID 문자열로 직렬화된다 (raw Long 노출 금지).
     * 직렬화는 HashIdConfig의 정적 Hashids 인스턴스를 사용한다.
     */
    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    @Schema(description = "아티클 ID (HashID 문자열)", example = "xJvY7aBp", type = "string")
    private Long id;

    private String slug;
    private String title;
    private String description;
    private String coverImageKey;
    private String category;
    private LocalDateTime publishedAt;

    public static PublicCurationArticleSummaryResponse of(CurationArticle a) {
        return PublicCurationArticleSummaryResponse.builder()
                .id(a.getId())
                .slug(a.getSlug())
                .title(a.getTitle())
                .description(a.getDescription())
                .coverImageKey(a.getCoverImageKey())
                .category(a.getCategory())
                .publishedAt(a.getPublishedAt())
                .build();
    }
}
