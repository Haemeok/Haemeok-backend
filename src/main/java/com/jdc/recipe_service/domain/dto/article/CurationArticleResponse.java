package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 어드민 큐레이션 아티클 상세 응답.
 *
 * <p>외부에 노출되는 모든 ID 필드는 HashID 문자열로 직렬화한다 — admin이라도 raw Long을 wire에 노출하지 않는다.
 * 내부 필드 타입은 그대로 Long 유지 (DB/service/repository와 정합).
 */
@Getter
@Builder
@Schema(description = "큐레이션 아티클 상세 응답")
public class CurationArticleResponse {

    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    @Schema(description = "아티클 ID (HashID 문자열)", example = "xJvY7aBp", type = "string")
    private Long id;

    @Schema(description = "URL slug")
    private String slug;

    @Schema(description = "제목")
    private String title;

    @Schema(description = "메타 설명")
    private String description;

    @Schema(description = "커버 이미지 S3 imageKey (.webp). path segment는 articleHashId 기반.",
            example = "images/articles/xJvY7aBp/uuid.webp")
    private String coverImageKey;

    @Schema(description = "본문 MDX 원본")
    private String contentMdx;

    @Schema(description = "카테고리")
    private String category;

    @Schema(description = "발행 상태")
    private ArticleStatus status;

    @Schema(description = "생성 AI 모델 식별자")
    private String generatedBy;

    @Schema(description = "사람 검수 완료 여부")
    private boolean humanReviewed;

    @Schema(description = "최초 발행 시각")
    private LocalDateTime publishedAt;

    @Schema(description = "생성 시각")
    private LocalDateTime createdAt;

    @Schema(description = "수정 시각")
    private LocalDateTime updatedAt;

    @JsonSerialize(contentUsing = HashIdConfig.HashIdSerializer.class)
    @ArraySchema(
            schema = @Schema(type = "string", example = "vK9mP2Qa", description = "레시피 ID (HashID 문자열)"),
            arraySchema = @Schema(description = "참조한 레시피 ID 목록 (HashID 문자열 배열, audit/soft link)")
    )
    private List<Long> recipeIds;

    public static CurationArticleResponse of(CurationArticle a, List<Long> recipeIds) {
        return CurationArticleResponse.builder()
                .id(a.getId())
                .slug(a.getSlug())
                .title(a.getTitle())
                .description(a.getDescription())
                .coverImageKey(a.getCoverImageKey())
                .contentMdx(a.getContentMdx())
                .category(a.getCategory())
                .status(a.getStatus())
                .generatedBy(a.getGeneratedBy())
                .humanReviewed(a.isHumanReviewed())
                .publishedAt(a.getPublishedAt())
                .createdAt(a.getCreatedAt())
                .updatedAt(a.getUpdatedAt())
                .recipeIds(recipeIds)
                .build();
    }
}
