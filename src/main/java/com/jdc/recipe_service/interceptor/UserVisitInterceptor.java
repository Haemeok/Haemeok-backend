package com.jdc.recipe_service.interceptor;

import com.jdc.recipe_service.service.token.UserActivityService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserVisitInterceptor implements HandlerInterceptor {

    private final RedisTemplate<String, String> redisTemplate;
    private final UserActivityService userActivityService;

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null ||
                !authentication.isAuthenticated() ||
                "anonymousUser".equals(authentication.getPrincipal())) {
            return true;
        }

        String userId = authentication.getName();

        String osType = request.getHeader("X-Os-Type");

        if (osType == null || osType.isBlank()) {
            osType = "UNKNOWN";
        }

        if (osType.length() > 20) {
            osType = osType.substring(0, 20);
        }

        String today = LocalDate.now(ZoneId.of("Asia/Seoul")).format(DateTimeFormatter.BASIC_ISO_DATE);

        String key = "visit:" + today + ":" + userId;

        Boolean isFirstVisit = redisTemplate.opsForValue()
                .setIfAbsent(key, "1", 1, TimeUnit.DAYS);

        if (Boolean.TRUE.equals(isFirstVisit)) {
            log.info("📈 새로운 DAU 집계: UserID={}", userId);
            userActivityService.saveUserVisit(userId, osType);
        }

        return true;
    }
}