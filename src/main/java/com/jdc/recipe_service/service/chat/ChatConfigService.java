package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatConfig;
import com.jdc.recipe_service.domain.repository.chat.ChatConfigRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class ChatConfigService {

    private final ChatConfigRepository repository;

    @Cacheable(value = "chatConfig", key = "#key")
    public String getValue(String key) {
        return repository.findByConfigKey(key)
                .map(ChatConfig::getConfigValue)
                .orElseThrow(() -> new IllegalStateException("Chat config missing: " + key));
    }

    public int getIntValue(String key) {
        return Integer.parseInt(getValue(key));
    }

    public boolean getBoolValue(String key) {
        return Boolean.parseBoolean(getValue(key));
    }

    @Transactional
    @CacheEvict(value = "chatConfig", key = "#key")
    public void updateValue(String key, String newValue, String updatedBy) {
        ChatConfig config = repository.findByConfigKey(key)
                .orElseThrow(() -> new IllegalStateException("Chat config missing: " + key));
        config.updateValue(newValue, updatedBy);
        log.info("Chat config updated: key={} updatedBy={}", key, updatedBy);
    }
}
