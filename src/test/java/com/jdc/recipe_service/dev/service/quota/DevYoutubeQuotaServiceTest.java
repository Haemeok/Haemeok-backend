package com.jdc.recipe_service.dev.service.quota;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.service.DailyQuotaService.DailyQuotaExceededException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevYoutubeQuotaService 단위 테스트.
 *
 * 검증 포인트:
 *  1. tryStartOrThrow: 시작 조건 통과 시 COST_BASIC(2) 차감 + 차감 날짜 반환, 실패 시 DailyQuotaExceededException
 *  2. chargeGeminiUpgrade: limit 체크 없이 무조건 +3 차감 (마지막 기회 보장)
 *     - chargeGeminiUpgrade(userId, usedOn): cross-midnight 정확성 — 차감일 row에 +3
 *     - chargeGeminiUpgrade(userId): deprecated, today 기준 차감 (legacy job 호환)
 *  3. refund: 양수면 위임, 0/음수면 no-op (today 버전 + date-aware 버전)
 *  4. admin: 모든 경로 bypass
 *  5. limit 결정: dev는 props.devYoutubePerDay 단일 출처 (QuotaPolicy DB 오버라이드 영향 없음)
 */
@ExtendWith(MockitoExtension.class)
class DevYoutubeQuotaServiceTest {

    @Mock DailyQuotaDao dao;
    @Mock QuotaProperties props;

    @InjectMocks DevYoutubeQuotaService service;

    private static final Long USER_ID = 1L;
    private static final int LIMIT = 20;

    @BeforeEach
    void setUpAuthAsUser() {
        var auth = new UsernamePasswordAuthenticationToken(
                "u", "pw", List.of(new SimpleGrantedAuthority("ROLE_USER")));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @AfterEach
    void clearAuth() {
        SecurityContextHolder.clearContext();
    }

    private void setAdmin() {
        var auth = new UsernamePasswordAuthenticationToken(
                "admin", "pw", List.of(new SimpleGrantedAuthority("ROLE_ADMIN")));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    private void setLimitViaProps() {
        // dev limit은 props.devYoutubePerDay 단일 출처 (QuotaPolicy 오버라이드 안 받음)
        given(props.getDevYoutubePerDay()).willReturn(LIMIT);
    }

    /** tryStartOrThrow가 LocalDate.now(props.zoneId()) 호출 → admin/non-admin 모두 zoneId stub 필요 */
    private void setZoneId() {
        given(props.zoneId()).willReturn(ZoneId.of("UTC"));
    }

    // --- tryStartOrThrow ---

    @Test
    @DisplayName("시작 차감은 date-aware DAO 사용 + service가 한 번 잡은 today를 그대로 DAO에 전달 (반환값 == DAO 인자)")
    void tryStart_success_usesDateAwareDaoAndReturnsDate() {
        setLimitViaProps();
        setZoneId();
        // 테스트가 자체적으로 LocalDate.now()를 잡지 않음 — UTC 자정 race 차단.
        // any(LocalDate.class)로 stub하고 service가 DAO에 넘긴 날짜를 캡처해 반환값과 동일한지 확인.
        given(dao.tryConsume(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                eq(LIMIT), eq(2), any(LocalDate.class))).willReturn(true);

        LocalDate result = service.tryStartOrThrow(USER_ID);

        ArgumentCaptor<LocalDate> dateCaptor = ArgumentCaptor.forClass(LocalDate.class);
        verify(dao).tryConsume(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                eq(LIMIT), eq(2), dateCaptor.capture());
        // 핵심 invariant: service가 캡처한 today를 (재호출 없이) DAO와 반환값에 동시에 흘림.
        assertThat(dateCaptor.getValue()).isEqualTo(result);
        // deprecated today-based overload는 호출 안 됨
        verify(dao, never()).tryConsume(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION), anyInt(), anyInt());
    }

    @Test
    @DisplayName("시작 조건 미충족(limit 도달)이면 DailyQuotaExceededException 던짐 (date-aware DAO false 반환)")
    void tryStart_overLimit_throws() {
        setLimitViaProps();
        setZoneId();
        // any(LocalDate.class)로 — 테스트의 시간 캡처가 service와 어긋날 가능성 자체를 제거.
        given(dao.tryConsume(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                eq(LIMIT), eq(2), any(LocalDate.class))).willReturn(false);

        assertThatThrownBy(() -> service.tryStartOrThrow(USER_ID))
                .isInstanceOf(DailyQuotaExceededException.class);
    }

    // --- chargeGeminiUpgrade ---

    @Test
    @DisplayName("Gemini upgrade(today): limit 체크 없이 +3 차감 (deprecated 경로, legacy job 호환)")
    @SuppressWarnings("deprecation")
    void chargeGemini_today_addsThreeWithoutLimitCheck() {
        service.chargeGeminiUpgrade(USER_ID);

        // limit=Integer.MAX_VALUE로 호출해서 start 체크를 우회 → 항상 +3
        verify(dao).tryConsume(USER_ID, QuotaType.YOUTUBE_EXTRACTION, Integer.MAX_VALUE, 3);
    }

    @Test
    @DisplayName("Gemini upgrade(usedOn): cross-midnight 정확성 — 명시 날짜 row에 +3 차감 (date-aware DAO)")
    void chargeGemini_withDate_usesDateAwareDao() {
        LocalDate yesterday = LocalDate.of(2026, 4, 25);

        service.chargeGeminiUpgrade(USER_ID, yesterday);

        verify(dao).tryConsume(USER_ID, QuotaType.YOUTUBE_EXTRACTION, Integer.MAX_VALUE, 3, yesterday);
    }

    @Test
    @DisplayName("Gemini upgrade(usedOn=null): legacy job → today 기준 deprecated 경로로 fallback")
    @SuppressWarnings("deprecation")
    void chargeGemini_withNullDate_fallsBackToToday() {
        service.chargeGeminiUpgrade(USER_ID, null);

        // deprecated 2-arg DAO 호출
        verify(dao).tryConsume(USER_ID, QuotaType.YOUTUBE_EXTRACTION, Integer.MAX_VALUE, 3);
        // date-aware overload는 호출 안 됨
        verify(dao, never()).tryConsume(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.anyInt(),
                org.mockito.ArgumentMatchers.any(LocalDate.class));
    }

    @Test
    @DisplayName("admin: chargeGeminiUpgrade(usedOn)도 dao 호출 없이 통과")
    void admin_chargeGemini_withDate_bypass() {
        setAdmin();

        service.chargeGeminiUpgrade(USER_ID, LocalDate.of(2026, 4, 25));

        verifyNoInteractions(dao);
    }

    // --- refund ---

    @Test
    @DisplayName("refund 양수: dao.refund에 그대로 전달")
    void refund_positive_delegates() {
        service.refund(USER_ID, 5);

        verify(dao).refund(USER_ID, QuotaType.YOUTUBE_EXTRACTION, 5);
    }

    @Test
    @DisplayName("refund 0: no-op")
    void refund_zero_noOp() {
        service.refund(USER_ID, 0);

        verify(dao, never()).refund(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION), org.mockito.ArgumentMatchers.anyInt());
    }

    @Test
    @DisplayName("refund 음수: no-op (방어 가드)")
    void refund_negative_noOp() {
        service.refund(USER_ID, -3);

        verify(dao, never()).refund(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION), org.mockito.ArgumentMatchers.anyInt());
    }

    // --- admin bypass ---

    @Test
    @DisplayName("admin: tryStartOrThrow는 dao 호출 없이 today 반환")
    void admin_tryStart_bypass() {
        setAdmin();
        setZoneId();

        // service 호출 시각을 윈도우로 감쌈 — 자정 순간이라도 [before, after] 안에 떨어지면 통과 (UTC race-free).
        LocalDate before = LocalDate.now(ZoneId.of("UTC"));
        LocalDate result = service.tryStartOrThrow(USER_ID);
        LocalDate after = LocalDate.now(ZoneId.of("UTC"));

        assertThat(result).isBetween(before, after);
        verifyNoInteractions(dao);
    }

    @Test
    @DisplayName("admin: chargeGeminiUpgrade(today)는 dao 호출 없이 통과")
    @SuppressWarnings("deprecation")
    void admin_chargeGemini_bypass() {
        setAdmin();

        service.chargeGeminiUpgrade(USER_ID);

        verifyNoInteractions(dao);
    }

    @Test
    @DisplayName("admin: refund는 dao 호출 없이 통과")
    @SuppressWarnings("deprecation")
    void admin_refund_bypass() {
        setAdmin();

        service.refund(USER_ID, 5);

        verifyNoInteractions(dao);
    }

    // ---------- 날짜 기반 refund (cross-midnight 환불 정확성) ----------

    @Test
    @DisplayName("refund(amount, usedOn): 명시 날짜로 dao.refund(date) 위임")
    void refund_withDate_delegates() {
        LocalDate yesterday = LocalDate.of(2026, 4, 25);

        service.refund(USER_ID, 5, yesterday);

        verify(dao).refund(USER_ID, QuotaType.YOUTUBE_EXTRACTION, 5, yesterday);
    }

    @Test
    @DisplayName("refund(amount=0, usedOn): no-op (방어 가드)")
    void refund_withDate_zeroAmount_noOp() {
        service.refund(USER_ID, 0, LocalDate.of(2026, 4, 25));

        verify(dao, never()).refund(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(LocalDate.class));
    }

    @Test
    @DisplayName("refund(amount, usedOn=null): no-op (방어 가드)")
    void refund_withDate_nullDate_noOp() {
        service.refund(USER_ID, 5, null);

        verify(dao, never()).refund(eq(USER_ID), eq(QuotaType.YOUTUBE_EXTRACTION),
                org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(LocalDate.class));
    }

    @Test
    @DisplayName("admin: refund(amount, usedOn)도 dao 호출 없이 통과")
    void admin_refund_withDate_bypass() {
        setAdmin();

        service.refund(USER_ID, 5, LocalDate.of(2026, 4, 25));

        verifyNoInteractions(dao);
    }

    @Test
    @DisplayName("admin: getRemainingQuota는 무한대 반환")
    void admin_remaining_isMaxValue() {
        setAdmin();

        int remaining = service.getRemainingQuota(USER_ID);

        assertThat(remaining).isEqualTo(Integer.MAX_VALUE);
        verifyNoInteractions(dao);
    }
}
