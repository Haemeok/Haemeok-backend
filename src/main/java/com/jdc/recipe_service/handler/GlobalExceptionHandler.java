package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.exception.ErrorResponse;
import com.jdc.recipe_service.service.DailyQuotaService;
import io.swagger.v3.oas.annotations.Hidden;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;
import java.util.UUID;

@RestControllerAdvice(annotations = {RestController.class})
@Hidden
@Slf4j
public class GlobalExceptionHandler {

    @Value("${spring.profiles.active:default}")
    private String activeProfile;

    @ExceptionHandler(CustomException.class)
    public ResponseEntity<ErrorResponse> handleCustomException(CustomException ex) {
        ErrorCode errorCode = ex.getErrorCode();
        return ResponseEntity
                .status(errorCode.getStatus())
                .body(ErrorResponse.of(errorCode));
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ErrorResponse> handleDataIntegrityViolationException(DataIntegrityViolationException ex) {
        log.warn("데이터베이스 무결성 제약조건 위반: {}", ex.getMessage());
        return ResponseEntity
                .status(HttpStatus.CONFLICT) // 409 Conflict
                .body(new ErrorResponse(ErrorCode.DATA_INTEGRITY_VIOLATION.getCode(), "데이터베이스 제약조건 위반 (예: 중복된 값)"));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidationException(MethodArgumentNotValidException ex) {
        String errorMessage = ex.getBindingResult()
                .getFieldErrors()
                .stream()
                .findFirst()
                .map(fieldError -> fieldError.getDefaultMessage())
                .orElse("잘못된 요청입니다.");

        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorResponse(ErrorCode.INVALID_INPUT_VALUE.getCode(), errorMessage));
    }

    @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
    public ResponseEntity<ErrorResponse> handleHttpMediaTypeNotSupportedException(HttpMediaTypeNotSupportedException ex) {
        log.warn("지원하지 않는 Content-Type 요청: {}", ex.getMessage());
        return ResponseEntity
                .status(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
                .body(new ErrorResponse(
                        ErrorCode.INVALID_CONTENT_TYPE.getCode(),
                        "지원하지 않는 Content-Type 입니다. application/json 형식을 사용해주세요."
                ));
    }

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public ResponseEntity<ErrorResponse> handleHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException ex) {
        log.warn("허용되지 않은 HTTP 메소드 요청: {} {}", ex.getMethod(), ex.getSupportedMethods());
        return ResponseEntity
                .status(HttpStatus.METHOD_NOT_ALLOWED)
                .body(new ErrorResponse(
                        ErrorCode.METHOD_NOT_ALLOWED.getCode(),
                        "'" + ex.getMethod() + "' 메소드는 지원하지 않습니다."
                ));
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ErrorResponse> handleIllegalArgument(IllegalArgumentException ex) {
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorResponse("901", "잘못된 요청입니다: " + ex.getMessage()));
    }

    @ExceptionHandler(NullPointerException.class)
    public ResponseEntity<ErrorResponse> handleNullPointerException(NullPointerException ex) {
        log.warn("NullPointerException 발생", ex);

        String msg = "필수 데이터가 누락되었습니다.";
        if ("local".equals(activeProfile)) {
            msg = "[NullPointerException] " + ex.getMessage();
        }

        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorResponse(ErrorCode.NULL_POINTER.getCode(), msg));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleException(Exception ex) {
        String errorId = UUID.randomUUID().toString();

        log.error("[ErrorID: {}] 서버 에러 발생", errorId, ex);

        if (ex.getClass().getName().startsWith("org.springdoc")) {
            throw new RuntimeException(ex);
        }

        if ("local".equals(activeProfile)) {
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(new ErrorResponse("903", "[" + ex.getClass().getSimpleName() + "] " + ex.getMessage(), errorId));
        }

        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(new ErrorResponse("903", "서버 내부 오류가 발생했습니다. 문제가 지속되면 관리자에게 문의해주세요.", errorId));
    }

    @ExceptionHandler(DailyQuotaService.DailyQuotaExceededException.class)
    public ResponseEntity<Map<String, Object>> handleDailyQuotaExceeded(
            DailyQuotaService.DailyQuotaExceededException e) {

        HttpHeaders headers = new HttpHeaders();
        headers.add("Retry-After", Long.toString(e.getRetryAfterSeconds()));

        Map<String, Object> body = Map.<String, Object>of(
                "code", "DAILY_QUOTA_EXCEEDED",
                "message", "하루 생성 제한을 초과했습니다.",
                "retryAfter", e.getRetryAfterSeconds()
        );

        return new ResponseEntity<>(body, headers, e.getStatus());
    }


}