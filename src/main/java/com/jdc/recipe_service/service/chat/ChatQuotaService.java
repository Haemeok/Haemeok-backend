package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatDailyUsage;
import com.jdc.recipe_service.domain.repository.chat.ChatDailyUsageRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.ZoneId;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class ChatQuotaService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    private final ChatDailyUsageRepository repository;
    private final ChatConfigService chatConfig;

    @Transactional
    public void checkAndIncrement(Long userId) {
        int limit = chatConfig.getIntValue("daily_quota_per_user");
        if (limit <= 0) {
            throw new CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED);
        }

        LocalDate today = LocalDate.now(KST);

        repository.ensureDailyUsageRow(userId, today);
        if (repository.incrementUsageIfBelowLimit(userId, today, limit) == 0) {
            throw new CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED);
        }
    }

    public int getRemainingQuota(Long userId) {
        int limit = chatConfig.getIntValue("daily_quota_per_user");
        LocalDate today = LocalDate.now(KST);

        int currentCount = repository.findByUserIdAndUsageDate(userId, today)
                .map(ChatDailyUsage::getCallCount)
                .orElse(0);

        return Math.max(0, limit - currentCount);
    }
}
