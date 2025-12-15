package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.ActionLog;
import com.jdc.recipe_service.domain.repository.ActionLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    @Transactional(readOnly = true)
    public Map<String, Object> getDashboardData() {
        Map<String, Object> data = new HashMap<>();

        LocalDateTime startOfDay = LocalDate.now(ZoneId.of("Asia/Seoul"))
                .atStartOfDay(ZoneId.of("Asia/Seoul"))
                .withZoneSameInstant(ZoneId.systemDefault())
                .toLocalDateTime();

        long todayVisitors = actionLogRepository.countTodayUniqueVisitors(startOfDay);
        long todayClicks = actionLogRepository.countTodayTotalClicks(startOfDay);

        List<Object[]> allStats = actionLogRepository.getStatSummary();

        data.put("todayVisitors", todayVisitors);
        data.put("todayClicks", todayClicks);
        data.put("allStats", allStats);

        return data;
    }
}