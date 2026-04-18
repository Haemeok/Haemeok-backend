package com.jdc.recipe_service.handler;

import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.exception.ErrorResponse;
import jakarta.servlet.http.HttpServletRequest;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.dao.CannotAcquireLockException;
import org.springframework.dao.ConcurrencyFailureException;
import org.springframework.dao.PessimisticLockingFailureException;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * GlobalExceptionHandler#handleConcurrencyFailure의 스코프 검증.
 *
 * 규칙:
 * - refresh 경로(/api/token/refresh)에서 올라온 lock 계열 예외는 401/400 "유효하지 않은 리프레시 토큰"으로
 *   매핑하고 auth_refresh_total{result=lock_timeout} 카운터를 1 증가시킨다.
 * - 다른 경로에서 올라온 같은 예외는 원래대로 catch-all로 내려가도록 다시 throw해야 한다.
 *
 * 왜 이렇게 나눴는가: refresh 경로에서 발생하는 순간적 lock contention은 프론트가 한 번 더 시도하면 복구되는
 * 성격이라 force-logout까지 가면 UX가 나쁘다. 반면 댓글/좋아요 등 다른 도메인에서는 같은 예외의 의미가 다르므로
 * "refresh 경로 한정"으로만 예외 매핑을 바꾼다.
 */
class GlobalExceptionHandlerTest {

    private MeterRegistry meterRegistry;
    private GlobalExceptionHandler handler;

    @BeforeEach
    void setUp() {
        meterRegistry = new SimpleMeterRegistry();
        handler = new GlobalExceptionHandler(meterRegistry);
    }

    private double lockTimeoutCount() {
        return meterRegistry.counter("auth_refresh_total", "result", "lock_timeout").count();
    }

    private static HttpServletRequest requestWithUri(String uri) {
        HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
        Mockito.when(request.getRequestURI()).thenReturn(uri);
        return request;
    }

    @Test
    @DisplayName("refresh 경로의 PessimisticLockingFailure는 INVALID_REFRESH_TOKEN(400)으로 매핑되고 lock_timeout 카운터가 1 증가한다")
    void refreshPath_mapsLockFailureToInvalidToken() {
        HttpServletRequest request = requestWithUri("/api/token/refresh");
        ConcurrencyFailureException ex = new PessimisticLockingFailureException("lock wait timeout");

        ResponseEntity<ErrorResponse> response = handler.handleConcurrencyFailure(ex, request);

        assertThat(response.getStatusCode()).isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN.getStatus());
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().getCode()).isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN.getCode());
        assertThat(lockTimeoutCount()).isEqualTo(1.0);
    }

    @Test
    @DisplayName("refresh 경로의 CannotAcquireLockException도 같은 방식으로 매핑된다")
    void refreshPath_mapsCannotAcquireLockSameWay() {
        // PessimisticLockingFailureException 외에 CannotAcquireLockException/DeadlockLoserDataAccessException도
        // 같은 super type(ConcurrencyFailureException)이라 동일하게 잡혀야 한다.
        HttpServletRequest request = requestWithUri("/api/token/refresh");
        ConcurrencyFailureException ex = new CannotAcquireLockException("could not acquire lock");

        ResponseEntity<ErrorResponse> response = handler.handleConcurrencyFailure(ex, request);

        assertThat(response.getStatusCode()).isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN.getStatus());
        assertThat(lockTimeoutCount()).isEqualTo(1.0);
    }

    @Test
    @DisplayName("refresh가 아닌 경로에서 올라온 ConcurrencyFailureException은 다시 throw되어 catch-all로 내려간다")
    void nonRefreshPath_rethrowsToCatchAll() {
        HttpServletRequest request = requestWithUri("/api/v1/recipes/1/like");
        ConcurrencyFailureException ex = new PessimisticLockingFailureException("deadlock on likes");

        assertThatThrownBy(() -> handler.handleConcurrencyFailure(ex, request))
                .isSameAs(ex);

        // 이 경로에서는 refresh 전용 카운터가 오염되면 안 된다.
        assertThat(lockTimeoutCount()).isEqualTo(0.0);
    }
}
