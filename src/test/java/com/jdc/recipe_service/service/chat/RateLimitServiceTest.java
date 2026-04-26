package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RateLimitServiceTest {

    private RateLimitService service;

    @BeforeEach
    void setUp() {
        service = new RateLimitService();
    }

    @Test
    @DisplayName("동일 user 분당 10건까지 통과")
    void allowsTenRequestsPerMinute() {
        Long userId = 1L;
        for (int i = 1; i <= 10; i++) {
            int idx = i;
            assertThatCode(() -> service.checkUserRate(userId))
                    .as("call %d should pass", idx)
                    .doesNotThrowAnyException();
        }
    }

    @Test
    @DisplayName("11번째 호출 차단 — CHAT_RATE_LIMITED")
    void blocksEleventhCall() {
        Long userId = 2L;
        for (int i = 1; i <= 10; i++) {
            service.checkUserRate(userId);
        }

        assertThatThrownBy(() -> service.checkUserRate(userId))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_RATE_LIMITED);
    }

    @Test
    @DisplayName("user 분리 — user A의 한도 소진이 user B에 영향 없음")
    void usersAreIndependent() {
        Long userA = 100L;
        Long userB = 200L;

        for (int i = 1; i <= 10; i++) {
            service.checkUserRate(userA);
        }

        // userA 차단 확인
        assertThatThrownBy(() -> service.checkUserRate(userA))
                .isInstanceOf(CustomException.class);

        // userB는 새 bucket이라 통과해야 함
        assertThatCode(() -> service.checkUserRate(userB))
                .doesNotThrowAnyException();
    }

    @Test
    @DisplayName("같은 user 같은 분 안에 10번 + 11번째 차단 후 다른 user 정상 동작")
    void mixedUserPattern() {
        Long u1 = 11L;
        Long u2 = 22L;
        Long u3 = 33L;

        for (int i = 0; i < 10; i++) service.checkUserRate(u1);
        assertThatThrownBy(() -> service.checkUserRate(u1))
                .isInstanceOf(CustomException.class);

        for (int i = 0; i < 5; i++) service.checkUserRate(u2);
        assertThatCode(() -> service.checkUserRate(u2)).doesNotThrowAnyException();

        assertThatCode(() -> service.checkUserRate(u3)).doesNotThrowAnyException();
    }

    @Test
    @DisplayName("처음 호출하는 user — 즉시 통과")
    void firstCallPasses() {
        assertThatCode(() -> service.checkUserRate(999L)).doesNotThrowAnyException();
    }
}
