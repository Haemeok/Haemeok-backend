package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.entity.QuotaPolicy;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.repository.QuotaPolicyRepository;
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

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DailyQuotaServiceTest {

    @Mock DailyQuotaDao dao;
    @Mock QuotaProperties props;
    @Mock QuotaPolicyRepository policyRepository;

    DailyQuotaService svc;

    @BeforeEach
    void setup() {
        svc = new DailyQuotaService(dao, props, policyRepository);
        SecurityContextHolder.clearContext();
    }

    private void setAuth(String role) {
        var auth = new UsernamePasswordAuthenticationToken(
                "u", "pw", List.of(new SimpleGrantedAuthority(role)));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @Test
    @DisplayName("AI 생성: 설정된 한도(1회)에 따라 1회 성공 후 2회째 차단되는지 확인")
    void ai_generation_limit_enforced() {
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 1;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getPerDay()).thenReturn(limit);

        when(dao.tryConsume(eq(userId), eq(type), eq(limit)))
                .thenReturn(true)
                .thenReturn(false);

        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));

        svc.consumeForUserOrThrow(userId, type);

        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(userId, type)
        );

        verify(dao, times(2)).tryConsume(eq(userId), eq(type), eq(limit));
    }

    @Test
    @DisplayName("유튜브 추출: 설정된 한도(1회)에 따라 2회째 차단 확인")
    void youtube_extraction_limit_enforced() {
        setAuth("ROLE_USER");
        Long userId = 2L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;
        int limit = 1;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getYoutubePerDay()).thenReturn(limit);

        when(dao.tryConsume(eq(userId), eq(type), eq(limit)))
                .thenReturn(true)
                .thenReturn(false);

        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));

        svc.consumeForUserOrThrow(userId, type);

        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(userId, type)
        );

        verify(dao, times(2)).tryConsume(eq(userId), eq(type), eq(limit));
    }

    @Test
    @DisplayName("관리자는 한도 조회나 소비 로직 없이 무조건 통과한다")
    void admin_bypasses_all_limits() {
        setAuth("ROLE_ADMIN");
        Long adminId = 99L;

        svc.consumeForUserOrThrow(adminId, QuotaType.AI_GENERATION);
        svc.consumeForUserOrThrow(adminId, QuotaType.YOUTUBE_EXTRACTION);

        verifyNoInteractions(dao, props, policyRepository);
    }

    @Test
    @DisplayName("DB에 정책이 존재하면 설정 파일 값 대신 DB 한도를 우선 사용한다")
    void db_policy_overrides_properties() {
        setAuth("ROLE_USER");
        Long userId = 3L;
        QuotaType type = QuotaType.AI_GENERATION;

        int dbLimit = 10;
        int propLimit = 1;

        QuotaPolicy mockPolicy = QuotaPolicy.builder().limitCount(dbLimit).build();
        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.of(mockPolicy));

        when(dao.tryConsume(eq(userId), eq(type), eq(dbLimit))).thenReturn(true);

        svc.consumeForUserOrThrow(userId, type);

        verify(dao).tryConsume(eq(userId), eq(type), eq(dbLimit));
        verify(dao, never()).tryConsume(eq(userId), eq(type), eq(propLimit));
    }

    @Test
    @DisplayName("사용자 쿼터 환불 시 DAO가 정상 호출된다 (관리자는 호출 안 됨)")
    void refund_logic_verification() {
        setAuth("ROLE_USER");
        Long userId = 4L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION;

        svc.refundIfPolicyAllows(userId, type);

        verify(dao, times(1)).refundOnce(userId, type);

        setAuth("ROLE_ADMIN");
        svc.refundIfPolicyAllows(99L, type);
        verify(dao, never()).refundOnce(99L, type);
    }

    @Test
    @DisplayName("남은 횟수 조회 시 현재 적용된 한도(DB/yml)를 기반으로 조회한다")
    void remaining_quota_verification() {
        setAuth("ROLE_USER");
        Long userId = 5L;
        QuotaType type = QuotaType.AI_GENERATION;
        int limit = 5;

        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.empty());
        when(props.getPerDay()).thenReturn(limit);

        when(dao.remainingToday(userId, type, limit)).thenReturn(3);

        int remaining = svc.getRemainingQuota(userId, type);

        verify(dao).remainingToday(userId, type, limit);
        assert remaining == 3;
    }

    @Test
    @DisplayName("캐시 제거 검증: DB 한도 값을 변경하면 즉시 반영되어야 한다")
    void cache_removal_verification() {
        setAuth("ROLE_USER");
        Long userId = 10L;
        QuotaType type = QuotaType.AI_GENERATION;

        QuotaPolicy policy10 = QuotaPolicy.builder().limitCount(10).build();
        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.of(policy10));

        int limit1 = svc.getLimitByType(type);
        org.assertj.core.api.Assertions.assertThat(limit1).isEqualTo(10);

        QuotaPolicy policy50 = QuotaPolicy.builder().limitCount(50).build();
        when(policyRepository.findByQuotaType(type)).thenReturn(Optional.of(policy50));

        int limit2 = svc.getLimitByType(type);
        org.assertj.core.api.Assertions.assertThat(limit2).isEqualTo(50);

        verify(policyRepository, times(2)).findByQuotaType(type);
    }
}