package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
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

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DailyQuotaServiceTest {

    @Mock
    DailyQuotaDao dao;
    @Mock
    QuotaProperties props;

    DailyQuotaService svc;

    @BeforeEach
    void setup() {
        svc = new DailyQuotaService(dao, props);
        SecurityContextHolder.clearContext();
    }

    private void setAuth(String role) {
        var auth = new UsernamePasswordAuthenticationToken(
                "u", "pw", List.of(new SimpleGrantedAuthority(role)));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @Test
    @DisplayName("AI 생성: 1회 성공 후 2회째 실패(429) 확인")
    void ai_generation_limit_enforced() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 1L;
        QuotaType type = QuotaType.AI_GENERATION;

        // Mocking: 첫 번째는 true(성공), 두 번째는 false(실패) 반환
        when(dao.tryConsume(eq(userId), eq(type))).thenReturn(true).thenReturn(false);
        // 실패 시 재시도 시간 계산을 위해 ZoneId 필요
        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));

        // When & Then
        // 1. 첫 번째 호출 -> 통과 (Exception 없음)
        svc.consumeForUserOrThrow(userId, type);

        // 2. 두 번째 호출 -> 차단 (ExceededException 발생)
        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(userId, type)
        );

        // 검증: dao가 정확한 타입으로 2번 호출되었는지 확인
        verify(dao, times(2)).tryConsume(eq(userId), eq(type));
    }

    @Test
    @DisplayName("유튜브 추출: 1회 성공 후 2회째 실패(429) 확인 (AI 생성과 별개)")
    void youtube_extraction_limit_enforced() {
        // Given
        setAuth("ROLE_USER");
        Long userId = 2L;
        QuotaType type = QuotaType.YOUTUBE_EXTRACTION; // ✅ 유튜브 타입 지정

        // Mocking
        when(dao.tryConsume(eq(userId), eq(type))).thenReturn(true).thenReturn(false);
        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));

        // When & Then
        // 1. 첫 번째 추출 -> 통과
        svc.consumeForUserOrThrow(userId, type);

        // 2. 두 번째 추출 -> 차단
        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(userId, type)
        );

        verify(dao, times(2)).tryConsume(eq(userId), eq(type));
    }

    @Test
    @DisplayName("관리자는 AI 생성 및 유튜브 추출 제한을 모두 무시한다")
    void admin_bypasses_all_limits() {
        // Given
        setAuth("ROLE_ADMIN");
        Long adminId = 99L;

        // When
        // 관리자는 어떤 타입을 호출하든 DB 접근 없이 통과해야 함
        svc.consumeForUserOrThrow(adminId, QuotaType.AI_GENERATION);
        svc.consumeForUserOrThrow(adminId, QuotaType.YOUTUBE_EXTRACTION);

        // Then
        // dao(Redis)나 props에 전혀 접근하지 않았음을 검증
        verifyNoInteractions(dao, props);
    }
}