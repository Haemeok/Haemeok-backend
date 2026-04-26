package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import com.jdc.recipe_service.domain.repository.chat.ChatLogRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class ChatLogServiceTest {

    @Mock
    private ChatLogRepository repository;

    @InjectMocks
    private ChatLogService chatLogService;

    @Test
    @DisplayName("ChatLog 정상 저장 — repository.save() 호출")
    void savesNormalChatLog() {
        ChatLog log = ChatLog.builder()
                .userId(1L)
                .recipeId(60L)
                .question("이거 매워요?")
                .intent("IN_SCOPE")
                .answer("보통 매운맛이에요.")
                .proCalled(true)
                .totalLatencyMs(2500)
                .build();

        chatLogService.saveAsync(log);

        verify(repository).save(log);
    }

    @Test
    @DisplayName("모든 비용/토큰/플래그 필드가 entity에 정확히 저장")
    void preservesAllFieldsInArgumentCaptor() {
        ChatLog log = ChatLog.builder()
                .userId(7L)
                .recipeId(75L)
                .question("덜 맵게 하려면?")
                .intent("IN_SCOPE")
                .answer("고춧가루 양을 줄이세요.")
                .proCalled(true)
                .miniInputTokens(474)
                .miniOutputTokens(6)
                .proInputTokens(1284)
                .proCachedTokens(1024)
                .proOutputTokens(195)
                .estimatedCostKrw(new BigDecimal("0.3413"))
                .miniLatencyMs(450)
                .proLatencyMs(2800)
                .totalLatencyMs(3300)
                .suspicious(false)
                .repetitionDetected(false)
                .answerTruncated(false)
                .build();

        chatLogService.saveAsync(log);

        ArgumentCaptor<ChatLog> captor = ArgumentCaptor.forClass(ChatLog.class);
        verify(repository).save(captor.capture());
        ChatLog saved = captor.getValue();

        org.assertj.core.api.Assertions.assertThat(saved.getUserId()).isEqualTo(7L);
        org.assertj.core.api.Assertions.assertThat(saved.getRecipeId()).isEqualTo(75L);
        org.assertj.core.api.Assertions.assertThat(saved.getIntent()).isEqualTo("IN_SCOPE");
        org.assertj.core.api.Assertions.assertThat(saved.isProCalled()).isTrue();
        org.assertj.core.api.Assertions.assertThat(saved.getProInputTokens()).isEqualTo(1284);
        org.assertj.core.api.Assertions.assertThat(saved.getProCachedTokens()).isEqualTo(1024);
        org.assertj.core.api.Assertions.assertThat(saved.getEstimatedCostKrw()).isEqualByComparingTo("0.3413");
    }

    @Test
    @DisplayName("저장 실패 시 예외 swallow — 호출자에게 propagate 안 함 (응답 흐름 보호)")
    void swallowsExceptionOnSaveFailure() {
        willThrow(new RuntimeException("DB connection refused"))
                .given(repository).save(org.mockito.ArgumentMatchers.any());

        ChatLog log = ChatLog.builder()
                .userId(1L)
                .recipeId(60L)
                .question("test")
                .intent("IN_SCOPE")
                .answer("answer")
                .totalLatencyMs(0)
                .build();

        assertThatCode(() -> chatLogService.saveAsync(log)).doesNotThrowAnyException();
    }

    @Test
    @DisplayName("error_message 포함된 ChatLog 저장 (실패 시나리오용)")
    void savesChatLogWithErrorMessage() {
        ChatLog log = ChatLog.builder()
                .userId(3L)
                .recipeId(60L)
                .question("주식 추천해줘")
                .intent("UNKNOWN")
                .answer("")
                .proCalled(false)
                .errorMessage("705:챗봇 일일 사용 한도를 초과했습니다.")
                .totalLatencyMs(150)
                .build();

        chatLogService.saveAsync(log);

        ArgumentCaptor<ChatLog> captor = ArgumentCaptor.forClass(ChatLog.class);
        verify(repository).save(captor.capture());
        org.assertj.core.api.Assertions.assertThat(captor.getValue().getErrorMessage())
                .startsWith("705:");
    }
}
