package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.ArraySchema;
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
            example = "images/articles/xJvY7aBp/uuid.webp")
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

    /**
     * 프론트가 보내는 wire 형식은 HashID 문자열 배열이지만, 내부 필드는 Long으로 유지해
     * service/repository 레이어가 raw Long으로 처리하게 한다.
     *
     * <p>{@code @JsonDeserialize(contentUsing = StrictHashIdDeserializer)}가 element 단위로 String → Long 변환.
     * Strict 버전이라 raw 숫자 입력(예: {@code [101, 102]})은 거부되고 HashID 문자열만 허용된다.
     * Bean Validation의 {@code @NotNull} / {@code @Positive}는 디코드 후 element 값에 적용된다.
     * OpenAPI는 wire 형식(string[])을 정확히 표현하기 위해 {@code @ArraySchema(schema=@Schema(type="string"))}.
     */
    @JsonDeserialize(contentUsing = HashIdConfig.StrictHashIdDeserializer.class)
    @ArraySchema(
            schema = @Schema(type = "string", example = "vK9mP2Qa", description = "레시피 ID (HashID 문자열)"),
            arraySchema = @Schema(description = "참조한 레시피 ID 목록 (HashID 문자열 배열, audit/soft link)")
    )
    private List<@NotNull(message = "recipeIds 항목에 null이 포함될 수 없습니다.")
                 @Positive(message = "recipeIds 항목은 양의 정수여야 합니다.") Long> recipeIds;
}
