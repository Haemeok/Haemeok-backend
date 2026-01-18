package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.entity.QuotaPolicy;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.repository.QuotaPolicyRepository;
import com.jdc.recipe_service.domain.type.QuotaType;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.ZonedDateTime;

@Service
@RequiredArgsConstructor
@Slf4j
public class DailyQuotaService {

    private final DailyQuotaDao dao;
    private final QuotaProperties props; // 기본 설정값 (DB 없을 때 백업용)
    private final QuotaPolicyRepository policyRepository; // DB 정책

    /**
     * [핵심] DB에서 한도를 조회합니다. (성능을 위해 10분 정도 캐싱 권장)
     */
    @Cacheable(value = "quotaLimit", key = "#type", unless = "#result == null")
    public int getLimitByType(QuotaType type) {
        return policyRepository.findByQuotaType(type)
                .map(QuotaPolicy::getLimitCount)
                .orElseGet(() -> {
                    if (type == QuotaType.YOUTUBE_EXTRACTION) return props.getYoutubePerDay();
                    return props.getPerDay();
                });
    }

    public void consumeForUserOrThrow(Long userId, QuotaType type) {
        if (isAdmin()) return;

        int currentLimit = getLimitByType(type);

        if (!dao.tryConsume(userId, type, currentLimit)) {
            long retryAfter = secondsUntilMidnight();
            throw new DailyQuotaExceededException(retryAfter);
        }
    }

    public void refundIfPolicyAllows(Long userId, QuotaType type) {
        if (isAdmin()) return;
        dao.refundOnce(userId, type);
    }

    public int getRemainingQuota(Long userId, QuotaType type) {
        if (isAdmin()) {
            return Integer.MAX_VALUE;
        }

        int currentLimit = getLimitByType(type);
        return dao.remainingToday(userId, type, currentLimit);
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

    @Getter
    public static class DailyQuotaExceededException extends RuntimeException {
        private final long retryAfterSeconds;
        private final HttpStatus status = HttpStatus.TOO_MANY_REQUESTS;
        public DailyQuotaExceededException(long s) { super("하루 생성 제한 초과"); this.retryAfterSeconds = s; }
    }
}