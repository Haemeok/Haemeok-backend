package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.RecipeActivityLog;
import com.jdc.recipe_service.domain.repository.RecipeActivityLogRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class RecipeActivityService {

    private final RecipeActivityLogRepository repository;

    @Transactional
    public void saveLog(Long userId, String nickname, ActivityLogType type) {
        if (userId == null) return;

        RecipeActivityLog log = RecipeActivityLog.builder()
                .userId(userId)
                .nickname(nickname)
                .activityType(type)
                .build();

        repository.save(log);
    }
}