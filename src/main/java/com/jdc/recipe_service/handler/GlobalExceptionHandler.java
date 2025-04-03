package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.CommentAccessDeniedException;
import com.jdc.recipe_service.exception.RecipeAccessDeniedException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(CommentAccessDeniedException.class)
    public ResponseEntity<String> handleCommentAccess(
            CommentAccessDeniedException ex) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body("본인의 댓글만 삭제할 수 있습니다.");
    }

    @ExceptionHandler(RecipeAccessDeniedException.class)
    public ResponseEntity<String> handleRecipeAccess(RecipeAccessDeniedException ex) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body("레시피 수정 권한이 없습니다.");
    }

    @ExceptionHandler(MissingServletRequestParameterException.class)
    public ResponseEntity<String> handleMissingParam(MissingServletRequestParameterException ex) {
        return ResponseEntity.badRequest()
                .body("요청 파라미터가 누락되었습니다: " + ex.getParameterName());
    }
}
