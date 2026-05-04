package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

/**
 * 아티클 이미지 presigned URL 응답 (articleHashId 기반).
 *
 * <p>레시피 이미지와 동일하게 원본 → webp 변환 파이프라인을 사용하며 key는 articleHashId path segment를 포함한다.
 * <ul>
 *   <li>{@code uploadKey} ({@code original/images/articles/{articleHashId}/{uuid}.{ext}}): 프론트가 PUT 업로드할 위치(원본).</li>
 *   <li>{@code imageKey} ({@code images/articles/{articleHashId}/{uuid}.webp}): Lambda 변환 후의 최종 위치. DB(coverImageKey),
 *       MDX의 {@code <ArticleImage imageKey="..." />}에는 항상 이 키만 저장한다.</li>
 *   <li>{@code presignedUrl}: {@code uploadKey}에 대한 S3 PUT URL. 발급 후 10분간 유효.</li>
 * </ul>
 *
 * <p>주의: 프론트는 절대 {@code uploadKey}를 DB나 MDX에 저장하지 않는다 — 변환 전 원본 경로이기 때문이다.
 */
@Getter
@Builder
@Schema(description = "아티클 이미지 presigned URL 발급 응답 (articleHashId 기반)")
public class ArticleImagePresignedUrlResponse {

    @Schema(description = "S3 PUT 업로드 대상 키 (원본 위치, ext는 contentType에 따라 jpg/png/webp). articleHashId segment 포함.",
            example = "original/images/articles/xJvY7aBp/4f5b3a3a-1a2b-4c5d-9e6f-1234567890ab.jpg")
    private String uploadKey;

    @Schema(description = "변환 후 최종 이미지 키 (.webp 고정). coverImageKey 또는 MDX에 저장한다.",
            example = "images/articles/xJvY7aBp/4f5b3a3a-1a2b-4c5d-9e6f-1234567890ab.webp")
    private String imageKey;

    @Schema(description = "uploadKey에 대한 presigned PUT URL. 10분간 유효.")
    private String presignedUrl;
}
