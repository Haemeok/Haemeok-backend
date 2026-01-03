package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.type.QuotaType;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.ZonedDateTime;

@Service
@RequiredArgsConstructor
public class DailyQuotaService {
    private final DailyQuotaDao dao;
    private final QuotaProperties props;

    public void consumeForUserOrThrow(Long userId, QuotaType type) {
        if (isAdmin()) return;

        if (!dao.tryConsume(userId, type)) {
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
        return dao.remainingToday(userId, type);
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
