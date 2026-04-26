package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import com.jdc.recipe_service.domain.repository.chat.ChatLogRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatLogService {

    private final ChatLogRepository repository;

    @Async
    @Transactional
    public void saveAsync(ChatLog chatLog) {
        try {
            repository.save(chatLog);
        } catch (Exception e) {
            log.error("Failed to save chat log: userId={} recipeId={}",
                    chatLog.getUserId(), chatLog.getRecipeId(), e);
        }
    }
}
