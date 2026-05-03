package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "큐레이션 아티클 생성 요청")
public class CurationArticleCreateRequest {

    @NotBlank
    @Pattern(regexp = "^[a-z0-9]+(?:-[a-z0-9]+)*$",
            message = "slug는 소문자/숫자/하이픈만 허용됩니다.")
    @Size(max = 200)
    @Schema(description = "URL slug. 소문자/숫자/하이픈만 허용", example = "summer-diet-cucumber-recipes")
    private String slug;

    @NotBlank
    @Size(max = 255)
    @Schema(description = "제목", example = "여름 다이어트 오이 레시피 모음")
    private String title;

    @Size(max = 500)
    @Schema(description = "메타 설명 (목록/SEO description)", example = "수분 95%의 오이로 만드는 가벼운 여름 한 끼")
    private String description;

    @Size(max = 500)
    @Schema(description = "커버 이미지 S3 key. presigned URL 응답의 imageKey(.webp)를 저장한다 — uploadKey가 아니다.",
            example = "images/articles/123/uuid.webp")
    private String coverImageKey;

    @NotBlank
    @Schema(description = "본문 MDX 원본")
    private String contentMdx;

    @Size(max = 50)
    @Schema(description = "카테고리", example = "diet")
    private String category;

    @Size(max = 100)
    @Schema(description = "생성에 사용된 AI 모델 식별자", example = "claude-opus-4.7")
    private String generatedBy;

    @Schema(description = "참조한 레시피 ID 목록 (audit/soft link). 각 ID는 양의 정수여야 한다.")
    private List<@NotNull(message = "recipeIds 항목에 null이 포함될 수 없습니다.")
                 @Positive(message = "recipeIds 항목은 양의 정수여야 합니다.") Long> recipeIds;
}
