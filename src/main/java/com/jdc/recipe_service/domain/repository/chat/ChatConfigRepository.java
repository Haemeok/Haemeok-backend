package com.jdc.recipe_service.domain.repository.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatConfig;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ChatConfigRepository extends JpaRepository<ChatConfig, Long> {

    Optional<ChatConfig> findByConfigKey(String configKey);
}
