package com.jdc.recipe_service.service.chat.prompt;

import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 챗봇 프롬프트를 startup 시 1회 classpath에서 로드해 메모리에 보관한다.
 */
@Component
@Slf4j
public class PromptLoader {

    private static final Map<String, String> PROMPT_PATHS = Map.of(
            "classifier", "prompts/classifier-v2.txt",
            "chat",       "prompts/chat-v6.txt",
            "reject",     "prompts/responses/reject.txt",
            "unclear",    "prompts/responses/unclear.txt"
    );

    private Map<String, String> prompts = Map.of();

    @PostConstruct
    void loadAll() {
        Map<String, String> loaded = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : PROMPT_PATHS.entrySet()) {
            String key = entry.getKey();
            String path = entry.getValue();
            loaded.put(key, loadOne(path));
        }
        this.prompts = Map.copyOf(loaded);

        log.info("PromptLoader loaded {} prompts: {}",
                prompts.size(),
                summarizeSizes(prompts));
    }

    public String get(String key) {
        String prompt = prompts.get(key);
        if (prompt == null) {
            throw new IllegalStateException("Prompt not found for key: " + key);
        }
        return prompt;
    }

    private String loadOne(String path) {
        ClassPathResource resource = new ClassPathResource(path);
        if (!resource.exists()) {
            throw new IllegalStateException("Prompt resource missing on classpath: " + path);
        }
        String content;
        try {
            content = StreamUtils.copyToString(resource.getInputStream(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to read prompt resource: " + path, e);
        }
        if (content.isBlank()) {
            throw new IllegalStateException("Prompt resource is blank: " + path);
        }
        return content;
    }

    private String summarizeSizes(Map<String, String> map) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (Map.Entry<String, String> e : map.entrySet()) {
            if (!first) sb.append(", ");
            sb.append(e.getKey()).append('=').append(e.getValue().length()).append("ch");
            first = false;
        }
        return sb.toString();
    }
}
