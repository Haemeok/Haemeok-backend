package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatDailyUsage;
import com.jdc.recipe_service.domain.repository.chat.ChatDailyUsageRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class ChatQuotaServiceTest {

    @Mock
    private ChatDailyUsageRepository repository;

    @Mock
    private ChatConfigService chatConfig;

    @InjectMocks
    private ChatQuotaService service;

    @Test
    @DisplayName("쿼터 한도 미만 — ensureRow 후 원자 증가가 성공하면 통과한다")
    void underLimitPasses() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.incrementUsageIfBelowLimit(eq(userId), any(LocalDate.class), eq(20)))
                .willReturn(1);

        assertThatCode(() -> service.checkAndIncrement(userId)).doesNotThrowAnyException();
        verify(repository).ensureDailyUsageRow(eq(userId), any(LocalDate.class));
        verify(repository).incrementUsageIfBelowLimit(eq(userId), any(LocalDate.class), eq(20));
    }

    @Test
    @DisplayName("한도 직전(limit-1) — 원자 증가가 성공하면 통과한다")
    void exactlyOneBelowLimitPasses() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.incrementUsageIfBelowLimit(eq(userId), any(), eq(20))).willReturn(1);

        assertThatCode(() -> service.checkAndIncrement(userId)).doesNotThrowAnyException();
        verify(repository).incrementUsageIfBelowLimit(eq(userId), any(), eq(20));
    }

    @Test
    @DisplayName("한도 도달(=limit) — 원자 증가가 매칭되지 않아 CHAT_QUOTA_EXCEEDED throw")
    void atLimitThrows() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.incrementUsageIfBelowLimit(eq(userId), any(), eq(20))).willReturn(0);

        assertThatThrownBy(() -> service.checkAndIncrement(userId))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUOTA_EXCEEDED);
        verify(repository).ensureDailyUsageRow(eq(userId), any());
    }

    @Test
    @DisplayName("쿼터 설정이 0 이하이면 ensureRow 호출 전에 차단한다")
    void nonPositiveLimitThrows() {
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(0);

        assertThatThrownBy(() -> service.checkAndIncrement(1L))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUOTA_EXCEEDED);
        verify(repository, never()).ensureDailyUsageRow(any(), any());
        verify(repository, never()).incrementUsageIfBelowLimit(any(), any(), anyInt());
    }

    @Test
    @DisplayName("getRemainingQuota — limit 20 - 사용 5 = 15")
    void getRemainingQuotaCorrect() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any()))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(5).build()));

        assertThat(service.getRemainingQuota(userId)).isEqualTo(15);
    }

    @Test
    @DisplayName("getRemainingQuota — race 초과 시 음수 방지 (Math.max 0)")
    void getRemainingQuotaClampsAtZero() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any()))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(25).build()));

        assertThat(service.getRemainingQuota(userId)).isZero();
    }

    @Test
    @DisplayName("getRemainingQuota — row 없으면 limit 그대로")
    void getRemainingQuotaWhenNoRow() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any())).willReturn(Optional.empty());

        assertThat(service.getRemainingQuota(userId)).isEqualTo(20);
    }
}
