package com.jdc.recipe_service.scheduler;

import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class RefreshTokenCleanupScheduler {

    private final RefreshTokenRepository refreshTokenRepository;

    // 매일 자정에 만료된 토큰 일괄 삭제
    @Scheduled(cron = "0 0 0 * * *")
    @Transactional
    public void cleanExpiredTokens() {
        refreshTokenRepository.deleteAllByExpiredAtBefore(LocalDateTime.now());
    }
}