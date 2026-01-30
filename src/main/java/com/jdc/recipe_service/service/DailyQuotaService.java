package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.entity.QuotaPolicy;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import com.jdc.recipe_service.domain.repository.QuotaPolicyRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.ZonedDateTime;

@Service
@RequiredArgsConstructor
@Slf4j
public class DailyQuotaService {

    private final DailyQuotaDao dao;
    private final QuotaProperties props;
    private final QuotaPolicyRepository policyRepository;
    private final UserRepository userRepository;

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

    @Transactional
    public boolean consumeForUserOrThrow(Long userId, QuotaType type) {
        if (isAdmin()) return false;

        int currentLimit = getLimitByType(type);
        if (dao.tryConsume(userId, type, currentLimit)) {
            return false; // 무료 횟수 사용함
        }

        useUserTokenOrThrow(userId, type);
        return true;
    }

    /**
     * [신규] 상황에 맞는 환불 메서드
     * @param usedToken consume 메서드가 반환한 값 (true면 토큰 환불, false면 무료 횟수 복구)
     */
    @Transactional
    public void refund(Long userId, QuotaType type, boolean usedToken) {
        if (isAdmin()) return;

        if (usedToken) {
            refundToken(userId, type);
        } else {
            dao.refundOnce(userId, type);
        }
    }

    private void refundToken(Long userId, QuotaType type) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        if (type == QuotaType.YOUTUBE_EXTRACTION) {
            user.addYoutubeToken(1);
        } else {
            user.addAiToken(1);
        }
    }

    private void useUserTokenOrThrow(Long userId, QuotaType type) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        boolean success;
        if (type == QuotaType.YOUTUBE_EXTRACTION) {
            success = user.tryUseYoutubeToken();
        } else {
            success = user.tryUseAiToken();
        }

        if (!success) {
            long retryAfter = secondsUntilMidnight();
            throw new DailyQuotaExceededException(retryAfter);
        }

        log.info("User id={} used TOKEN for type={}", userId, type);
    }

    @Transactional
    public void rewardReviewTokens(Long userId) {
        User user = userRepository.findById(userId).orElseThrow();
        user.addYoutubeToken(30);
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