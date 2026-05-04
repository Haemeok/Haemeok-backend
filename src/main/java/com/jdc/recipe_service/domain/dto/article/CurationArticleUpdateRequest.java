package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 큐레이션 아티클 수정 요청.
 *
 * <p>slug는 SEO URL 안정성을 위해 생성 후 변경할 수 없다. slug를 바꿔야 하면 archive 후 새로 생성한다.
 */
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "큐레이션 아티클 수정 요청 (slug 제외)")
public class CurationArticleUpdateRequest {

    @NotBlank
    @Size(max = 255)
    @Schema(description = "제목")
    private String title;

    @Size(max = 500)
    @Schema(description = "메타 설명")
    private String description;

    @Size(max = 500)
    @Schema(description = "커버 이미지 S3 key. presigned URL 응답의 imageKey(.webp)를 저장한다 — uploadKey가 아니다.",
            example = "images/articles/xJvY7aBp/uuid.webp")
    private String coverImageKey;

    @NotBlank
    @Schema(description = "본문 MDX 원본")
    private String contentMdx;

    @Size(max = 50)
    @Schema(description = "카테고리")
    private String category;

    @Size(max = 100)
    @Schema(description = "생성에 사용된 AI 모델 식별자 (재생성 시 갱신)")
    private String generatedBy;

    /**
     * 프론트는 HashID 문자열 배열로 보내고, 내부 필드는 Long으로 유지된다 (CreateRequest와 동일 정책).
     */
    @JsonDeserialize(contentUsing = HashIdConfig.StrictHashIdDeserializer.class)
    @ArraySchema(
            schema = @Schema(type = "string", example = "vK9mP2Qa", description = "레시피 ID (HashID 문자열)"),
            arraySchema = @Schema(description = "참조한 레시피 ID 목록 (HashID 문자열 배열) — 요청 값으로 전체 교체된다.")
    )
    private List<@NotNull(message = "recipeIds 항목에 null이 포함될 수 없습니다.")
                 @Positive(message = "recipeIds 항목은 양의 정수여야 합니다.") Long> recipeIds;
}
