package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.CommentAccessDeniedException;
import com.jdc.recipe_service.exception.RecipeAccessDeniedException;
import io.swagger.v3.oas.annotations.Hidden;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.nio.file.AccessDeniedException;

@RestControllerAdvice
@Hidden
@Slf4j
public class GlobalExceptionHandler {

    @ExceptionHandler({ CommentAccessDeniedException.class, AccessDeniedException.class })
    public ResponseEntity<String> handleCommentAccess(Exception ex) {
        return ResponseEntity
                .status(HttpStatus.FORBIDDEN)
                .body(ex.getMessage());
    }

    @ExceptionHandler(RecipeAccessDeniedException.class)
    public ResponseEntity<String> handleRecipeAccess(RecipeAccessDeniedException ex) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body("레시피 수정 권한이 없습니다.");
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<String> handleBadRequest(IllegalArgumentException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("잘못된 요청입니다: " + ex.getMessage());
    }

    @Value("${spring.profiles.active:default}")
    private String activeProfile;

    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<String> handleUnknownError(RuntimeException ex) {
        log.error("예상치 못한 서버 오류 발생", ex);

        if ("local".equals(activeProfile)) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("[" + ex.getClass().getSimpleName() + "] " + ex.getMessage());
        }

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("예상치 못한 서버 오류가 발생했습니다.");
    }


}
