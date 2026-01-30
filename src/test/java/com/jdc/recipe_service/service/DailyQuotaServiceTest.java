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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

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
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 2;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getPerDay()).thenReturn(limit);
        when(dao.tryConsume(eq(userId), eq(type), eq(limit))).thenReturn(true); // 무료 성공

        boolean usedToken = svc.consumeForUserOrThrow(userId, type);

        assertThat(usedToken).isFalse();
        verify(userRepository, never()).findById(any());
    }

    @Test
    @DisplayName("시나리오 2: 일일 한도가 소진되었으나, 보유 토큰이 있으면 성공한다 (결과: true)")
    void consume_success_with_token() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 2;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getPerDay()).thenReturn(limit);

        // 1. 일일 한도 실패
        when(dao.tryConsume(eq(userId), eq(type), eq(limit))).thenReturn(false);

        // 2. 토큰 성공
        User mockUser = mock(User.class);
        when(userRepository.findById(userId)).thenReturn(Optional.of(mockUser));
        when(mockUser.tryUseAiToken()).thenReturn(true);

        boolean usedToken = svc.consumeForUserOrThrow(userId, type);

        assertThat(usedToken).isTrue();
        verify(mockUser).tryUseAiToken();
    }

    @Test
    @DisplayName("시나리오 3: 일일 한도도 없고, 토큰도 없으면 예외가 발생한다")
    void consume_fail_no_quota_no_token() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;
        int limit = 3;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getYoutubePerDay()).thenReturn(limit);
        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));

        // 1. 일일 한도 실패
        when(dao.tryConsume(eq(userId), eq(type), eq(limit))).thenReturn(false);

        // 2. 토큰 실패
        User mockUser = mock(User.class);
        when(userRepository.findById(userId)).thenReturn(Optional.of(mockUser));
        when(mockUser.tryUseYoutubeToken()).thenReturn(false);

        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(userId, type)
        );
    }

    @Test
    @DisplayName("시나리오 4: 토큰을 사용하려는데 유저가 DB에 없으면 예외 발생 (방어 로직)")
    void consume_fail_user_not_found() {
        setAuth("ROLE_USER");
        Long userId = 999L;
        QuotaType type = QuotaType.AI_GENERATION;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getPerDay()).thenReturn(1);
        when(dao.tryConsume(any(), any(), anyInt())).thenReturn(false); // 일일 한도 끝남

        when(userRepository.findById(userId)).thenReturn(Optional.empty()); // 유저 없음

        assertThrows(IllegalArgumentException.class,
                () -> svc.consumeForUserOrThrow(userId, type));
    }


    // --- [2. 환불 로직 테스트] ---

    @Test
    @DisplayName("환불: usedToken=false면 Redis(Dao)를 복구한다")
    void refund_daily_quota() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;

        svc.refund(userId, type, false); // 무료 횟수 환불

        verify(dao).refundOnce(userId, type);
        verify(userRepository, never()).findById(any());
    }

    @Test
    @DisplayName("환불: usedToken=true이고 유튜브 타입이면 유튜브 토큰을 복구한다")
    void refund_token_youtube() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;

        User mockUser = mock(User.class);
        when(userRepository.findById(userId)).thenReturn(Optional.of(mockUser));

        svc.refund(userId, type, true); // 토큰 환불

        verify(dao, never()).refundOnce(any(), any());
        verify(mockUser).addYoutubeToken(1); // Youtube 토큰 증가 확인
    }

    @Test
    @DisplayName("환불: usedToken=true이고 AI 타입이면 AI 토큰을 복구한다 [New]")
    void refund_token_ai() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;

        User mockUser = mock(User.class);
        when(userRepository.findById(userId)).thenReturn(Optional.of(mockUser));

        svc.refund(userId, type, true); // 토큰 환불

        verify(dao, never()).refundOnce(any(), any());
        verify(mockUser).addAiToken(1); // AI 토큰 증가 확인
    }

    // --- [3. 기타 테스트] ---

    @Test
    @DisplayName("관리자는 무조건 통과 (false 반환)")
    void admin_bypasses_all_limits() {
        setAuth("ROLE_ADMIN");
        Long adminId = 99L;

        boolean result = svc.consumeForUserOrThrow(adminId, QuotaType.AI_GENERATION);

        assertThat(result).isFalse();
        verifyNoInteractions(dao, policyRepository, userRepository);
    }

    @Test
    @DisplayName("DB 정책 우선순위 확인")
    void db_policy_overrides_properties() {
        setAuth("ROLE_USER");
        Long userId = 3L;
        QuotaType type = QuotaType.AI_GENERATION;
        int dbLimit = 10;

        QuotaPolicy mockPolicy = QuotaPolicy.builder().limitCount(dbLimit).build();
        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.of(mockPolicy));
        when(dao.tryConsume(eq(userId), eq(type), eq(dbLimit))).thenReturn(true);

        svc.consumeForUserOrThrow(userId, type);

        verify(dao).tryConsume(eq(userId), eq(type), eq(dbLimit));
    }
}