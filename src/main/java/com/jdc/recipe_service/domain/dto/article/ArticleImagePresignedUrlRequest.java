package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "아티클 이미지 presigned URL 발급 요청")
public class ArticleImagePresignedUrlRequest {

    @NotBlank
    @Schema(description = "Content-Type. image/jpeg, image/png, image/webp 만 허용", example = "image/webp")
    private String contentType;

    @NotNull
    @Positive
    @Schema(description = "파일 크기(byte). 10MB 이하만 허용", example = "245678")
    private Long fileSize;
}
