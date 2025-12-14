package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.ActionLog;
import com.jdc.recipe_service.domain.repository.ActionLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class ActionLogService {

    private final ActionLogRepository actionLogRepository;

    @Transactional
    public void saveLog(String action, String uuid, String ip, String userAgent, Long loginUserId) {

        ActionLog.ActionLogBuilder logBuilder = ActionLog.builder()
                .actionType(action)
                .ipAddress(ip)
                .userAgent(userAgent)
                .guestUuid(uuid);

        if (loginUserId != null) {
            logBuilder.userId(loginUserId);
        }

        actionLogRepository.save(logBuilder.build());
    }
}