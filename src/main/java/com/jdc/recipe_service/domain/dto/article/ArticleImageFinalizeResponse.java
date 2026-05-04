package com.jdc.recipe_service.domain.dto.article;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

/**
 * Finalize 성공 응답. 모든 imageKey가 S3에 존재할 때만 반환된다 (HTTP 200).
 *
 * <p>일부라도 누락된 경우는 ArticleImagesNotReadyException → GlobalExceptionHandler가
 * HTTP 409 + missingKeys를 포함한 별도 shape으로 응답한다.
 */
@Getter
@Builder
@Schema(description = "아티클 이미지 finalize 성공 응답")
public class ArticleImageFinalizeResponse {

    @Schema(description = "모든 imageKey가 S3에 존재함을 의미. 항상 true (false 케이스는 409로 응답).",
            example = "true")
    private boolean ready;

    @Schema(description = "확인 완료된 imageKey 목록 (요청과 동일).",
            example = "[\"images/articles/xJvY7aBp/abc-uuid.webp\"]")
    private List<String> imageKeys;
}
