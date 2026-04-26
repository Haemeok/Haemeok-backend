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
    @DisplayName("쿼터 한도 미만 — checkAndIncrement 통과 + incrementUsage 호출")
    void underLimitPassesAndIncrements() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any(LocalDate.class)))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(5).build()));

        assertThatCode(() -> service.checkAndIncrement(userId)).doesNotThrowAnyException();
        verify(repository).incrementUsage(eq(userId), any(LocalDate.class));
    }

    @Test
    @DisplayName("최초 호출 — chat_daily_usage row 없으면 count=0으로 간주, 통과")
    void firstCallNoRowYetPasses() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any())).willReturn(Optional.empty());

        assertThatCode(() -> service.checkAndIncrement(userId)).doesNotThrowAnyException();
        verify(repository).incrementUsage(eq(userId), any());
    }

    @Test
    @DisplayName("한도 직전(limit-1) — 통과")
    void exactlyOneBelowLimitPasses() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any()))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(19).build()));

        assertThatCode(() -> service.checkAndIncrement(userId)).doesNotThrowAnyException();
        verify(repository).incrementUsage(any(), any());
    }

    @Test
    @DisplayName("한도 도달(=limit) — CHAT_QUOTA_EXCEEDED throw, increment 안 됨")
    void atLimitThrowsAndDoesNotIncrement() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any()))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(20).build()));

        assertThatThrownBy(() -> service.checkAndIncrement(userId))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUOTA_EXCEEDED);
        verify(repository, never()).incrementUsage(any(), any());
    }

    @Test
    @DisplayName("race로 한도 +1 초과(limit+1) — 여전히 차단")
    void overLimitFromRaceStillBlocked() {
        Long userId = 1L;
        given(chatConfig.getIntValue("daily_quota_per_user")).willReturn(20);
        given(repository.findByUserIdAndUsageDate(eq(userId), any()))
                .willReturn(Optional.of(ChatDailyUsage.builder().userId(userId).callCount(21).build()));

        assertThatThrownBy(() -> service.checkAndIncrement(userId))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUOTA_EXCEEDED);
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
