package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.CommentAccessDeniedException;
import com.jdc.recipe_service.exception.RecipeAccessDeniedException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
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

    // 🔥 추가된 예외 처리
    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> handleGeneralException(Exception ex) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("서버 오류: " + ex.getMessage());
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<String> handleIllegalArgument(IllegalArgumentException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("잘못된 요청입니다: " + ex.getMessage());
    }

    @ExceptionHandler(org.springframework.http.converter.HttpMessageNotReadableException.class)
    public ResponseEntity<String> handleHttpMessageNotReadable(HttpMessageNotReadableException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("요청 JSON 포맷을 확인해주세요.");
    }
}
