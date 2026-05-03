package com.jdc.recipe_service.exception;

import lombok.Getter;

import java.util.List;

/**
 * Finalize 시점에 일부 imageKey가 S3에 아직 없을 때 던진다.
 *
 * <p>ErrorCode {@link ErrorCode#ARTICLE_IMAGES_NOT_READY} (1207, 409)로 매핑되며, GlobalExceptionHandler가
 * 일반 ErrorResponse가 아닌 별도 shape({@code missingKeys}, {@code presentKeys} 포함)으로 응답한다.
 */
@Getter
public class ArticleImagesNotReadyException extends RuntimeException {

    private final List<String> missingKeys;
    private final List<String> presentKeys;

    public ArticleImagesNotReadyException(List<String> missingKeys, List<String> presentKeys) {
        super(ErrorCode.ARTICLE_IMAGES_NOT_READY.getMessage());
        this.missingKeys = List.copyOf(missingKeys);
        this.presentKeys = List.copyOf(presentKeys);
    }
}
