package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.entity.QuotaPolicy;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.repository.QuotaPolicyRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.QuotaType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.time.ZoneId;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class DailyQuotaServiceTest {

    @Mock DailyQuotaDao dao;
    @Mock QuotaProperties props;
    @Mock QuotaPolicyRepository policyRepository;
    @Mock UserRepository userRepository;

    DailyQuotaService svc;

    @BeforeEach
    void setup() {
        svc = new DailyQuotaService(dao, props, policyRepository, userRepository);
        SecurityContextHolder.clearContext();
    }

    private void setAuth(String role) {
        var auth = new UsernamePasswordAuthenticationToken(
                "u", "pw", List.of(new SimpleGrantedAuthority(role)));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    // --- [1. 소비 로직 테스트] ---

    @Test
    @DisplayName("시나리오 1: 일일 무료 한도가 남아있으면, 토큰을 확인하지 않고 성공한다 (결과: false)")
    void consume_success_with_daily_quota() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 2;

        given(policyRepository.findByQuotaType(type)).willReturn(Optional.empty());
        given(props.getPerDay()).willReturn(limit);
        given(dao.tryConsume(eq(userId), eq(type), eq(limit))).willReturn(true); // 무료 성공

        // When
        boolean usedToken = svc.consumeForUserOrThrow(userId, type);

        // Then
        assertThat(usedToken).isFalse();
        verify(userRepository, never()).findById(any());
    }

    @Test
    @DisplayName("시나리오 2: 일일 한도가 소진되었으나, 보유 토큰이 있으면 성공한다 (결과: true)")
    void consume_success_with_token() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 2;

        given(policyRepository.findByQuotaType(type)).willReturn(Optional.empty());
        given(props.getPerDay()).willReturn(limit);

        // 1. 일일 한도 실패
        given(dao.tryConsume(eq(userId), eq(type), eq(limit))).willReturn(false);

        // 2. 토큰 성공
        User mockUser = mock(User.class);
        given(userRepository.findById(userId)).willReturn(Optional.of(mockUser));
        given(mockUser.tryUseAiToken()).willReturn(true);

        // When
        boolean usedToken = svc.consumeForUserOrThrow(userId, type);

        // Then
        assertThat(usedToken).isTrue();
        verify(mockUser).tryUseAiToken();
    }

    @Test
    @DisplayName("시나리오 3: 일일 한도도 없고, 토큰도 없으면 예외가 발생한다")
    void consume_fail_no_quota_no_token() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;
        int limit = 3;

        given(policyRepository.findByQuotaType(type)).willReturn(Optional.empty());
        given(props.getYoutubePerDay()).willReturn(limit);
        given(props.zoneId()).willReturn(ZoneId.of("Asia/Seoul"));

        // 1. 일일 한도 실패
        given(dao.tryConsume(eq(userId), eq(type), eq(limit))).willReturn(false);

        // 2. 토큰 실패
        User mockUser = mock(User.class);
        given(userRepository.findById(userId)).willReturn(Optional.of(mockUser));
        given(mockUser.tryUseYoutubeToken()).willReturn(false);

        // When & Then
        assertThatThrownBy(() -> svc.consumeForUserOrThrow(userId, type))
                .isInstanceOf(DailyQuotaService.DailyQuotaExceededException.class);
    }

    @Test
    @DisplayName("시나리오 4: 토큰을 사용하려는데 유저가 DB에 없으면 예외 발생 (방어 로직)")
    void consume_fail_user_not_found() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 999L;
        QuotaType type = QuotaType.AI_GENERATION;

        given(policyRepository.findByQuotaType(type)).willReturn(Optional.empty());
        given(props.getPerDay()).willReturn(1);
        given(dao.tryConsume(any(), any(), anyInt())).willReturn(false); // 일일 한도 끝남

        given(userRepository.findById(userId)).willReturn(Optional.empty()); // 유저 없음

        // When & Then
        assertThatThrownBy(() -> svc.consumeForUserOrThrow(userId, type))
                .isInstanceOf(IllegalArgumentException.class);
    }


    // --- [2. 환불 로직 테스트] ---

    @Test
    @DisplayName("환불: usedToken=false면 Redis(Dao)를 복구한다")
    void refund_daily_quota() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;

        // When
        svc.refund(userId, type, false); // 무료 횟수 환불

        // Then
        verify(dao).refundOnce(userId, type);
        verify(userRepository, never()).findById(any());
    }

    @Test
    @DisplayName("환불: usedToken=true이고 유튜브 타입이면 유튜브 토큰을 복구한다")
    void refund_token_youtube() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;

        User mockUser = mock(User.class);
        given(userRepository.findById(userId)).willReturn(Optional.of(mockUser));

        // When
        svc.refund(userId, type, true); // 토큰 환불

        // Then
        verify(dao, never()).refundOnce(any(), any());
        verify(mockUser).addYoutubeToken(1); // Youtube 토큰 증가 확인
    }

    @Test
    @DisplayName("환불: usedToken=true이고 AI 타입이면 AI 토큰을 복구한다 [New]")
    void refund_token_ai() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;

        User mockUser = mock(User.class);
        given(userRepository.findById(userId)).willReturn(Optional.of(mockUser));

        // When
        svc.refund(userId, type, true); // 토큰 환불

        // Then
        verify(dao, never()).refundOnce(any(), any());
        verify(mockUser).addAiToken(1); // AI 토큰 증가 확인
    }

    // --- [3. 기타 테스트] ---

    @Test
    @DisplayName("관리자는 무조건 통과 (false 반환)")
    void admin_bypasses_all_limits() {
        // Given
        setAuth("ROLE_ADMIN");
        Long adminId = 99L;

        // When
        boolean result = svc.consumeForUserOrThrow(adminId, QuotaType.AI_GENERATION);

        // Then
        assertThat(result).isFalse();
        verifyNoInteractions(dao, policyRepository, userRepository);
    }

    @Test
    @DisplayName("DB 정책 우선순위 확인")
    void db_policy_overrides_properties() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 3L;
        QuotaType type = QuotaType.AI_GENERATION;
        int dbLimit = 10;

        QuotaPolicy mockPolicy = QuotaPolicy.builder().limitCount(dbLimit).build();
        given(policyRepository.findByQuotaType(type)).willReturn(Optional.of(mockPolicy));
        given(dao.tryConsume(eq(userId), eq(type), eq(dbLimit))).willReturn(true);

        // When
        svc.consumeForUserOrThrow(userId, type);

        // Then
        verify(dao).tryConsume(eq(userId), eq(type), eq(dbLimit));
    }
}
