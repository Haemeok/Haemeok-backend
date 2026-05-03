package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.ArticleImagesNotReadyException;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.exception.ErrorResponse;
import com.jdc.recipe_service.service.DailyQuotaService;
import io.micrometer.core.instrument.MeterRegistry;
import io.swagger.v3.oas.annotations.Hidden;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.ConcurrencyFailureException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;

@RestControllerAdvice(annotations = {RestController.class})
@Hidden
@Slf4j
@RequiredArgsConstructor
public class GlobalExceptionHandler {

    @Value("${spring.profiles.active:default}")
    private String activeProfile;

    private final MeterRegistry meterRegistry;

    @ExceptionHandler(CustomException.class)
    public ResponseEntity<ErrorResponse> handleCustomException(CustomException ex, HttpServletRequest request) {
        ErrorCode errorCode = ex.getErrorCode();

        String message = ex.getMessage();

        if (message == null || message.isBlank()) {
            message = errorCode.getMessage();
        }

        if (isRefreshRequest(request)) {
            log.warn("[AUTH_REFRESH] result=response status={} code={} message={}",
                    errorCode.getStatus().value(), errorCode.getCode(), message);
        }

        return ResponseEntity
                .status(errorCode.getStatus())
                .body(new ErrorResponse(errorCode.getCode(), message));
    }

    @ExceptionHandler(UsernameNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleUsernameNotFoundException(UsernameNotFoundException ex) {
        log.warn("⚠️ 존재하지 않는 회원의 요청 (탈퇴 회원 등): {}", ex.getMessage());

        return ResponseEntity
                .status(HttpStatus.UNAUTHORIZED)
                .body(new ErrorResponse(ErrorCode.USER_NOT_FOUND.getCode(), "존재하지 않는 회원입니다. 다시 로그인해주세요."));
    }

    /**
     * @PreAuthorize 등 method security가 던지는 AccessDeniedException을 403으로 매핑한다.
     *
     * <p>운영 경로에서는 ExceptionTranslationFilter가 처리하지만, filter chain을 우회한 경로
     * (예: @WebMvcTest의 addFilters=false, 비표준 invocation)에서도 catch-all 500이 아닌 403이
     * 나가도록 보강한다. 메시지/코드는 ADMIN_ACCESS_DENIED를 재사용한다 — 이 프로젝트의 @PreAuthorize는
     * 사실상 admin 게이트 용도로만 쓰인다.
     */
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ErrorResponse> handleAccessDenied(AccessDeniedException ex) {
        log.warn("AccessDenied: {}", ex.getMessage());
        ErrorCode errorCode = ErrorCode.ADMIN_ACCESS_DENIED;
        return ResponseEntity
                .status(errorCode.getStatus())
                .body(new ErrorResponse(errorCode.getCode(), errorCode.getMessage()));
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ErrorResponse> handleDataIntegrityViolationException(DataIntegrityViolationException ex) {
        log.warn("데이터베이스 무결성 제약조건 위반: {}", ex.getMessage());
        return ResponseEntity
                .status(HttpStatus.CONFLICT)
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

    /**
     * 잘못된 JSON 본문(파싱 실패 / 인코딩 깨짐 / 본문 enum mismatch 등)을 400으로 매핑.
     *
     * <p>Spring 기본은 catch-all로 흘러 500 + errorId가 발급되어 운영자가 매번 로그를 추적해야 했다.
     * 클라이언트 입력 오류는 사용자가 즉시 수정 가능한 4xx로 명확히 내려준다 — 운영 노이즈 감소.
     *
     * <p>대표 케이스 (request body 한정):
     * <ul>
     *   <li>UTF-8이 아닌 인코딩으로 한국어 본문이 들어옴 (charset mismatch)</li>
     *   <li>중간이 잘린 JSON ("{\"name\":")</li>
     *   <li>본문 enum 필드에 whitelist 외 문자열 (예: body의 visibility="UNKNOWN")</li>
     *   <li>HashID 디코딩 실패</li>
     * </ul>
     *
     * <p>query/path param의 enum mismatch는 별개 예외(MethodArgumentTypeMismatchException 등)로
     * 들어오므로 이 핸들러 대상이 아니다.
     */
    @ExceptionHandler(org.springframework.http.converter.HttpMessageNotReadableException.class)
    public ResponseEntity<ErrorResponse> handleHttpMessageNotReadable(
            org.springframework.http.converter.HttpMessageNotReadableException ex) {
        log.warn("요청 본문 파싱 실패: {}", ex.getMessage());
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorResponse(
                        ErrorCode.INVALID_INPUT_VALUE.getCode(),
                        "요청 본문 형식이 올바르지 않습니다. JSON 구문/UTF-8 인코딩/enum 값을 확인해주세요."
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

    // refresh 회전은 row-level pessimistic lock을 잡기 때문에 innodb_lock_wait_timeout 만료 시
    // Spring이 ConcurrencyFailureException 계열(PessimisticLockingFailureException,
    // CannotAcquireLockException, DeadlockLoserDataAccessException 등)을 올려준다.
    // 이 상황은 프론트가 한 번 더 refresh를 시도하면 복구되는 성격이라, 500(catch-all)로
    // 빠지지 않도록 여기서 401 계열 "유효하지 않은 토큰"으로 매핑해 force-logout까지
    // 이어지지 않게 한다. 단, refresh 경로가 아닌 다른 도메인(댓글 좋아요 등)에서 같은
    // 예외가 나면 의미가 다르므로 원 예외를 다시 던져 catch-all로 돌려보낸다.
    @ExceptionHandler(ConcurrencyFailureException.class)
    public ResponseEntity<ErrorResponse> handleConcurrencyFailure(ConcurrencyFailureException ex,
                                                                  HttpServletRequest request) {
        if (!isRefreshRequest(request)) {
            throw ex;
        }

        ErrorCode errorCode = ErrorCode.INVALID_REFRESH_TOKEN;
        log.warn("[AUTH_REFRESH] result=fail reason=lock_timeout exceptionType={} message={}",
                ex.getClass().getSimpleName(), ex.getMessage());

        meterRegistry.counter("auth_refresh_total", "result", "lock_timeout").increment();

        return ResponseEntity
                .status(errorCode.getStatus())
                .body(new ErrorResponse(errorCode.getCode(), errorCode.getMessage()));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleException(Exception ex, HttpServletRequest request) {
        String errorId = UUID.randomUUID().toString();

        log.error("[ErrorID: {}] 서버 에러 발생", errorId, ex);

        if (isRefreshRequest(request)) {
            log.error("[AUTH_REFRESH] result=response status=500 exceptionType={} message={} errorId={}",
                    ex.getClass().getSimpleName(), ex.getMessage(), errorId);
        }

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

    /**
     * Finalize 시점에 일부 imageKey가 S3에 없을 때 응답한다.
     *
     * <p>일반 ErrorResponse({code, message})를 확장해 missingKeys/presentKeys까지 포함한다 — 프론트가
     * 정확히 어떤 키가 아직 변환 안 됐는지 알아야 폴링/리트라이 UX를 만들 수 있기 때문.
     * (DailyQuotaExceededException의 retryAfter 패턴과 동일.)
     */
    @ExceptionHandler(ArticleImagesNotReadyException.class)
    public ResponseEntity<Map<String, Object>> handleArticleImagesNotReady(ArticleImagesNotReadyException ex) {
        ErrorCode errorCode = ErrorCode.ARTICLE_IMAGES_NOT_READY;
        // 정상 폴링 과정에서 자주 발생하는 409라 debug로 낮춤. 운영 로그 노이즈 방지.
        log.debug("Article images not ready: missing={}", ex.getMissingKeys());

        Map<String, Object> body = Map.of(
                "code", errorCode.getCode(),
                "message", errorCode.getMessage(),
                "missingKeys", ex.getMissingKeys(),
                "presentKeys", ex.getPresentKeys()
        );

        return ResponseEntity.status(errorCode.getStatus()).body(body);
    }

    @ExceptionHandler(DailyQuotaService.DailyQuotaExceededException.class)
    public ResponseEntity<Map<String, Object>> handleDailyQuotaExceeded(
            DailyQuotaService.DailyQuotaExceededException e) {

        Map<String, Object> body = Map.of(
                "code", ErrorCode.DAILY_QUOTA_EXCEEDED.getCode(),
                "message", ErrorCode.DAILY_QUOTA_EXCEEDED.getMessage(),
                "retryAfter", e.getRetryAfterSeconds()
        );

        return new ResponseEntity<>(body, e.getStatus());
    }

    @ExceptionHandler(RejectedExecutionException.class)
    public ResponseEntity<ErrorResponse> handleRejected(RejectedExecutionException e) {
        return ResponseEntity.status(HttpStatus.TOO_MANY_REQUESTS)
                .body(new ErrorResponse("TOO_MANY_REQUESTS", e.getMessage()));
    }

    private boolean isRefreshRequest(HttpServletRequest request) {
        return request != null && "/api/token/refresh".equals(request.getRequestURI());
    }

}
