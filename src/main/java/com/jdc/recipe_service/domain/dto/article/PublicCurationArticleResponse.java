package com.jdc.recipe_service.domain.dto.article;

import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Public 큐레이션 아티클 상세 응답.
 *
 * <p>운영 전용 필드(status, generatedBy, humanReviewed, updatedAt)는 의도적으로 제외한다.
 * 이 DTO는 PUBLISHED 상태 아티클에만 적용되므로 status를 노출할 의미도 없다.
 * 어드민 UI는 별도의 {@link CurationArticleResponse}를 사용한다.
 */
@Getter
@Builder
@Schema(description = "Public 큐레이션 아티클 상세 응답")
public class PublicCurationArticleResponse {

    @Schema(description = "아티클 ID")
    private Long id;

    @Schema(description = "URL slug")
    private String slug;

    @Schema(description = "제목")
    private String title;

    @Schema(description = "메타 설명")
    private String description;

    @Schema(description = "커버 이미지 S3 imageKey (.webp)")
    private String coverImageKey;

    @Schema(description = "본문 MDX 원본")
    private String contentMdx;

    @Schema(description = "카테고리")
    private String category;

    @Schema(description = "발행 시각")
    private LocalDateTime publishedAt;

    @Schema(description = "참조한 레시피 ID 목록")
    private List<Long> recipeIds;

    public static PublicCurationArticleResponse of(CurationArticle a, List<Long> recipeIds) {
        return PublicCurationArticleResponse.builder()
                .id(a.getId())
                .slug(a.getSlug())
                .title(a.getTitle())
                .description(a.getDescription())
                .coverImageKey(a.getCoverImageKey())
                .contentMdx(a.getContentMdx())
                .category(a.getCategory())
                .publishedAt(a.getPublishedAt())
                .recipeIds(recipeIds)
                .build();
    }
}
