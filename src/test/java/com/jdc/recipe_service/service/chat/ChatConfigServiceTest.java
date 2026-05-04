package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatConfig;
import com.jdc.recipe_service.domain.repository.chat.ChatConfigRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class ChatConfigServiceTest {

    @Mock
    private ChatConfigRepository repository;

    private ChatConfigService service;

    @BeforeEach
    void setUp() {
        service = new ChatConfigService(repository, new ConcurrentMapCacheManager("chatConfig"));
    }

    @Test
    @DisplayName("chat_enabled는 킬스위치라 캐시를 우회하고 매번 DB에서 읽는다")
    void chatEnabledBypassesCache() {
        // given
        given(repository.findByConfigKey("chat_enabled"))
                .willReturn(Optional.of(config("chat_enabled", "false")))
                .willReturn(Optional.of(config("chat_enabled", "true")));

        // when & then
        assertThat(service.getBoolValue("chat_enabled")).isFalse();
        assertThat(service.getBoolValue("chat_enabled")).isTrue();
        verify(repository, times(2)).findByConfigKey("chat_enabled");
    }

    @Test
    @DisplayName("일반 chat_config는 캐시를 사용해 반복 DB 조회를 줄인다")
    void nonCriticalConfigUsesCache() {
        // given
        given(repository.findByConfigKey("daily_quota_per_user"))
                .willReturn(Optional.of(config("daily_quota_per_user", "20")));

        // when & then
        assertThat(service.getIntValue("daily_quota_per_user")).isEqualTo(20);
        assertThat(service.getIntValue("daily_quota_per_user")).isEqualTo(20);
        verify(repository, times(1)).findByConfigKey("daily_quota_per_user");
    }

    @Test
    @DisplayName("관리 API로 설정을 바꾸면 해당 config 캐시를 무효화한다")
    void updateValueEvictsCache() {
        // given
        ChatConfig quota = config("daily_quota_per_user", "20");
        given(repository.findByConfigKey("daily_quota_per_user"))
                .willReturn(Optional.of(quota));

        // when & then
        assertThat(service.getIntValue("daily_quota_per_user")).isEqualTo(20);
        service.updateValue("daily_quota_per_user", "30", "admin");
        assertThat(service.getIntValue("daily_quota_per_user")).isEqualTo(30);
        verify(repository, times(3)).findByConfigKey("daily_quota_per_user");
    }

    private ChatConfig config(String key, String value) {
        return ChatConfig.builder()
                .configKey(key)
                .configValue(value)
                .build();
    }
}
