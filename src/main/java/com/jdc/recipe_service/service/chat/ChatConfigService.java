package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatConfig;
import com.jdc.recipe_service.domain.repository.chat.ChatConfigRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class ChatConfigService {

    private static final String CHAT_CONFIG_CACHE = "chatConfig";
    private static final Set<String> IMMEDIATE_KEYS = Set.of("chat_enabled");

    private final ChatConfigRepository repository;
    private final CacheManager cacheManager;

    public String getValue(String key) {
        if (IMMEDIATE_KEYS.contains(key)) {
            return loadValue(key);
        }

        Cache cache = cacheManager.getCache(CHAT_CONFIG_CACHE);
        if (cache == null) {
            return loadValue(key);
        }

        return cache.get(key, () -> loadValue(key));
    }

    private String loadValue(String key) {
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
    public void updateValue(String key, String newValue, String updatedBy) {
        ChatConfig config = repository.findByConfigKey(key)
                .orElseThrow(() -> new IllegalStateException("Chat config missing: " + key));
        config.updateValue(newValue, updatedBy);
        evictCachedValue(key);
        log.info("Chat config updated: key={} updatedBy={}", key, updatedBy);
    }

    private void evictCachedValue(String key) {
        Cache cache = cacheManager.getCache(CHAT_CONFIG_CACHE);
        if (cache != null) {
            cache.evict(key);
        }
    }
}
