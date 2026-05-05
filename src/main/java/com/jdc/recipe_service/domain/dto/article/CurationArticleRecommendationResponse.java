package com.jdc.recipe_service.domain.dto.article;

import com.jdc.recipe_service.domain.projection.article.CurationArticleRecommendationProjection;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

/**
 * 추천 아티클 카드 응답.
 *
 * <p>아티클 상세 페이지 하단의 "추천 카드"에 필요한 최소 필드만 노출한다 — id/contentMdx/status/recipeIds는
 * 의도적으로 제외 (raw Long 노출 회피 + 페이로드 절감). slug로 카드 클릭 시 상세 페이지로 이동한다.
 */
@Getter
@Builder
@Schema(description = "Public 큐레이션 아티클 추천 카드 응답")
public class CurationArticleRecommendationResponse {

    @Schema(description = "URL slug. 카드 클릭 시 상세 페이지로 이동",
            example = "high-protein-breakfast")
    private String slug;

    @Schema(description = "카드 제목",
            example = "단백질 아침 레시피 모음")
    private String title;

    @Schema(description = "카드 썸네일 이미지의 S3 imageKey (.webp). null 가능",
            example = "images/articles/xJvY7aBp/uuid.webp",
            nullable = true)
    private String coverImageKey;

    @Schema(description = "카테고리 라벨. null 가능",
            example = "diet",
            nullable = true)
    private String category;

    public static CurationArticleRecommendationResponse of(CurationArticleRecommendationProjection p) {
        return CurationArticleRecommendationResponse.builder()
                .slug(p.getSlug())
                .title(p.getTitle())
                .coverImageKey(p.getCoverImageKey())
                .category(p.getCategory())
                .build();
    }
}
