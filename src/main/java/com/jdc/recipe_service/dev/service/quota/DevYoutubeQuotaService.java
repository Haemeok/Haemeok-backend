package com.jdc.recipe_service.dev.service.quota;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.DailyQuotaService.DailyQuotaExceededException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.LocalDate;
import java.time.ZonedDateTime;

/**
 * Dev V3 YouTube extraction 전용 일일 쿼터 서비스.
 *
 * 운영 V1/V2 (DailyQuotaService)와 분리:
 *  - V1/V2: AI 토큰 fallback 포함, 1회 단위 차감
 *  - V3: variable cost (BASIC=2, WITH_GEMINI=5), 토큰 fallback 없음, 음수 허용 (마지막 기회)
 *
 * 두 단계 차감 패턴:
 *  1. tryStartExtraction: job 시작 시 BASIC(2) 차감. 시작 조건은 used_count < limit만 검사.
 *     합이 limit을 초과해도 허용 (예: used=18, +2 → 20). 다음 호출은 used >= limit이라 거부.
 *  2. chargeGeminiUpgrade: 추출 도중 Gemini 분석이 필요해진 경우 추가 3 차감.
 *     (tryStartExtraction에서 시작 조건은 이미 통과했으므로 무조건 차감.)
 *
 * 환불은 차감된 누적 cost (2 or 5)를 그대로 전달.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevYoutubeQuotaService {

    /** 시그널 충분 (자막/설명/댓글에서 재료 추출 가능) → Gemini 미사용 */
    public static final int COST_BASIC = 2;
    /** 시그널 부족 → Gemini 분석 fallback 사용 */
    public static final int COST_WITH_GEMINI = 5;
    /** Gemini fallback 결정 시 추가 차감분 */
    public static final int COST_GEMINI_UPGRADE = COST_WITH_GEMINI - COST_BASIC; // 3

    private final DailyQuotaDao dao;
    private final QuotaProperties props;

    /**
     * Job 시작 시 호출 — 기본 비용(2) 차감.
     * 시작 조건만 검사. limit 초과 가능 (마지막 기회).
     *
     * @return 차감이 발생한 날짜 (facade가 job.quotaUsedOn에 저장 → cross-midnight 환불 정확성 보장).
     *         admin인 경우에도 오늘 날짜 반환 (호출자 코드 단순화).
     * @throws DailyQuotaExceededException limit 도달로 시작 불가 (controller에서 429 매핑)
     */
    public LocalDate tryStartOrThrow(Long userId) {
        // 자정 race 방지: today를 서비스에서 한 번 캡처하고 DAO에도 그대로 넘긴다.
        // (today-based DAO를 쓰면 service가 today 캡처한 직후 자정이 지나면 DAO는 다음 날 row에 차감 →
        //  service 반환값(어제)과 실제 차감 row(오늘)가 어긋나 환불 시 누수 발생.)
        LocalDate today = LocalDate.now(props.zoneId());
        if (isAdmin()) return today;

        int limit = getLimit();
        boolean ok = dao.tryConsume(userId, QuotaType.YOUTUBE_EXTRACTION, limit, COST_BASIC, today);
        if (!ok) {
            throw new DailyQuotaExceededException(secondsUntilMidnight());
        }
        log.info("✅ [DevYoutubeQuota] start consume: userId={}, cost={}, usedOn={}", userId, COST_BASIC, today);
        return today;
    }

    /**
     * 추출 도중 Gemini fallback이 결정된 경우 추가 3 차감 (today 기준).
     * tryStartOrThrow에서 시작 조건은 이미 통과했으므로 limit 체크 없이 무조건 차감.
     * (used_count가 limit을 초과해도 마지막 기회 허용.)
     *
     * @deprecated cross-midnight 환불 정확성을 위해 {@link #chargeGeminiUpgrade(Long, LocalDate)}를 사용할 것.
     *             today 기준으로 차감하면 자정 넘어 BASIC(어제) + UPGRADE(오늘) row가 분리되어
     *             환불 시 시작일 row만 환불되고 오늘 +3은 누수된다.
     */
    @Deprecated
    public void chargeGeminiUpgrade(Long userId) {
        if (isAdmin()) return;

        // limit=MAX_VALUE로 start 체크를 우회. 결과적으로 used_count += 3.
        dao.tryConsume(userId, QuotaType.YOUTUBE_EXTRACTION, Integer.MAX_VALUE, COST_GEMINI_UPGRADE);
        log.info("⬆️ [DevYoutubeQuota] gemini upgrade: userId={}, +{}", userId, COST_GEMINI_UPGRADE);
    }

    /**
     * 명시적 날짜 기준 Gemini upgrade 차감 — cross-midnight 정확성용.
     * BASIC 차감이 발생한 날짜(quotaUsedOn)에 대해 +3 차감 → 같은 row에 누적.
     * 이후 환불도 같은 날짜 기준으로 정확히 처리됨.
     *
     * usedOn이 null이면 today 기준 deprecated 경로로 fallback (legacy job 호환).
     */
    public void chargeGeminiUpgrade(Long userId, LocalDate usedOn) {
        if (isAdmin()) return;
        if (usedOn == null) {
            // legacy job (V20260426_006 마이그레이션 이전): today 기준 차감.
            // 향후 backfill 후 deprecated 경로 제거 가능.
            chargeGeminiUpgrade(userId);
            return;
        }

        dao.tryConsume(userId, QuotaType.YOUTUBE_EXTRACTION, Integer.MAX_VALUE, COST_GEMINI_UPGRADE, usedOn);
        log.info("⬆️ [DevYoutubeQuota] gemini upgrade(date): userId={}, +{}, usedOn={}",
                userId, COST_GEMINI_UPGRADE, usedOn);
    }

    /**
     * 작업 실패 시 환불. 실제 차감된 누적 cost를 그대로 전달.
     *  - Gemini 사용 안 했으면 COST_BASIC(2) 환불
     *  - Gemini 사용했으면 COST_WITH_GEMINI(5) 환불
     *
     * @deprecated cross-midnight 환불 정확성을 위해 {@link #refund(Long, int, LocalDate)}를 사용할 것.
     */
    @Deprecated
    public void refund(Long userId, int amount) {
        if (isAdmin()) return;
        if (amount <= 0) {
            log.warn("[DevYoutubeQuota] refund skipped: non-positive amount={}", amount);
            return;
        }
        dao.refund(userId, QuotaType.YOUTUBE_EXTRACTION, amount);
        log.info("💸 [DevYoutubeQuota] refund: userId={}, amount={}", userId, amount);
    }

    /**
     * 명시적 날짜 기준 환불 — cross-midnight 정확성용.
     * facade가 job.quotaUsedOn (차감 시점 날짜)을 그대로 전달하면, async가 자정 넘어 실패해도
     * 시작일 quota counter에서 정확히 환불됨.
     */
    public void refund(Long userId, int amount, LocalDate usedOn) {
        if (isAdmin()) return;
        if (amount <= 0 || usedOn == null) {
            log.warn("[DevYoutubeQuota] refund(date) skipped: amount={}, usedOn={}", amount, usedOn);
            return;
        }
        dao.refund(userId, QuotaType.YOUTUBE_EXTRACTION, amount, usedOn);
        log.info("💸 [DevYoutubeQuota] refund(date): userId={}, amount={}, usedOn={}", userId, amount, usedOn);
    }

    public int getRemainingQuota(Long userId) {
        if (isAdmin()) return Integer.MAX_VALUE;
        return dao.remainingToday(userId, QuotaType.YOUTUBE_EXTRACTION, getLimit());
    }

    private int getLimit() {
        // dev는 항상 props.devYoutubePerDay (default 20) 사용.
        // 운영 V1/V2의 QuotaPolicy(YOUTUBE_EXTRACTION) DB row가 3 등으로 오버라이드돼 있어도 dev는 영향 안 받음.
        // (운영과 dev의 limit을 격리하기 위함. 추후 dev 전용 정책이 필요해지면 DEV_YOUTUBE_EXTRACTION 같은
        //  별도 QuotaType + 정책을 도입해 정식 분리.)
        return props.getDevYoutubePerDay();
    }

    private boolean isAdmin() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null) return false;
        return auth.getAuthorities().stream()
                .anyMatch(a -> a.getAuthority().equals("ROLE_ADMIN") || a.getAuthority().equals("ADMIN"));
    }

    private long secondsUntilMidnight() {
        var zone = props.zoneId();
        var now = ZonedDateTime.now(zone);
        var midnight = now.toLocalDate().plusDays(1).atStartOfDay(zone);
        return Duration.between(now, midnight).getSeconds();
    }
}
