package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
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
@Schema(description = "아티클 이미지 finalize 요청 — S3에 변환 결과(.webp)가 실제로 존재하는지 확인한다.")
public class ArticleImageFinalizeRequest {

    @NotEmpty(message = "imageKeys는 비어 있을 수 없습니다.")
    @Size(max = 50, message = "한 번에 finalize 가능한 imageKey 개수는 최대 50개입니다.")
    @Schema(description = "확인할 imageKey 목록 (.webp). 모두 images/articles/{articleHashId}/ 형식이어야 한다. 한 번에 최대 50개.",
            example = "[\"images/articles/xJvY7aBp/abc-uuid.webp\", \"images/articles/xJvY7aBp/def-uuid.webp\"]")
    private List<@NotBlank @Size(max = 500) String> imageKeys;
}
