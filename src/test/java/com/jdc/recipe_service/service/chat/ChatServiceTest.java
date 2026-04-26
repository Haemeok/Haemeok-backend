package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.dto.chat.ChatResponse;
import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import com.jdc.recipe_service.domain.repository.chat.ChatLogRepository;
import com.jdc.recipe_service.domain.type.chat.Intent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.UserMessage;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ChatServiceTest {

    @Mock private ChatConfigService chatConfig;
    @Mock private ChatQuotaService quotaService;
    @Mock private RateLimitService rateLimitService;
    @Mock private ChatRecipeLoader recipeLoader;
    @Mock private IntentClassifier classifier;
    @Mock private AnswerGenerator generator;
    @Mock private RepetitionGuard repetitionGuard;
    @Mock private SuspiciousDetector suspiciousDetector;
    @Mock private AnswerValidator answerValidator;
    @Mock private ChatLogService chatLogService;
    @Mock private ChatLogRepository chatLogRepository;
    @Mock private PromptLoader prompts;

    @InjectMocks
    private ChatService chatService;

    private static final Long RECIPE_ID = 60L;
    private static final String QUESTION = "이거 매워요?";

    @BeforeEach
    void setupHappyPathDefaults() {
        // Happy path: kill switch off, max length 500, history empty, recipe loads, no suspicious, no repetition, validator ok.
        given(chatConfig.getBoolValue("chat_enabled")).willReturn(true);
        given(chatConfig.getIntValue("max_question_length")).willReturn(500);
        given(suspiciousDetector.detect(anyString())).willReturn(SuspiciousResult.clean());
        given(recipeLoader.loadAsPromptText(anyLong(), anyLong())).willReturn("recipe text");
        given(chatLogRepository.findRecentForContext(anyLong(), anyLong(), any(), any()))
                .willReturn(List.of());
        given(repetitionGuard.guard(anyString())).willAnswer(inv -> inv.getArgument(0));
        given(answerValidator.validate(anyString())).willReturn(ValidationResult.ok());
        given(prompts.get("reject")).willReturn("거부 메시지입니다.");
        given(prompts.get("unclear")).willReturn("좀 더 자세히 알려주세요.");
    }

    @Test
    @DisplayName("IN_SCOPE 정상 흐름 — Pro 호출 + ChatLog saveAsync")
    void inScopeFlowReturnsProAnswer() {
        given(classifier.classify("이거 매워요?"))
                .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 5));
        given(generator.generate(eq("이거 매워요?"), eq("recipe text"), any()))
                .willReturn(new GenerationResult("매콤한 김치찌개입니다.", 1000, 50, 200));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        assertThat(response.getIntent()).isEqualTo(Intent.IN_SCOPE);
        assertThat(response.isFromLlm()).isTrue();
        assertThat(response.getAnswer()).isEqualTo("매콤한 김치찌개입니다.");
        verify(chatLogService).saveAsync(any(ChatLog.class));
        verify(quotaService).checkAndIncrement(1L);
        verify(rateLimitService).checkUserRate(1L);
    }

    @Test
    @DisplayName("OUT_OF_SCOPE — reject 정형 응답, Pro 호출 X")
    void outOfScopeReturnsRejectAndSkipsPro() {
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.OUT_OF_SCOPE, 100, 5));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        assertThat(response.getIntent()).isEqualTo(Intent.OUT_OF_SCOPE);
        assertThat(response.isFromLlm()).isFalse();
        assertThat(response.getAnswer()).isEqualTo("거부 메시지입니다.");
        verify(generator, never()).generate(anyString(), anyString(), any());
    }

    @Test
    @DisplayName("UNCLEAR — unclear 정형 응답")
    void unclearReturnsUnclearAndSkipsPro() {
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.UNCLEAR, 100, 8));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        assertThat(response.getIntent()).isEqualTo(Intent.UNCLEAR);
        assertThat(response.isFromLlm()).isFalse();
        assertThat(response.getAnswer()).isEqualTo("좀 더 자세히 알려주세요.");
        verify(generator, never()).generate(anyString(), anyString(), any());
    }

    @Test
    @DisplayName("kill switch ON (chat_enabled=false) — CHAT_DISABLED throw, classifier 호출 X")
    void killSwitchOnThrowsChatDisabled() {
        given(chatConfig.getBoolValue("chat_enabled")).willReturn(false);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_DISABLED);

        verify(classifier, never()).classify(anyString());
    }

    @Test
    @DisplayName("rate limit 초과 — CHAT_RATE_LIMITED propagate")
    void rateLimitExceededThrows() {
        willThrow(new CustomException(ErrorCode.CHAT_RATE_LIMITED))
                .given(rateLimitService).checkUserRate(1L);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_RATE_LIMITED);

        verify(classifier, never()).classify(anyString());
    }

    @Test
    @DisplayName("쿼터 초과 — CHAT_QUOTA_EXCEEDED propagate, classifier 호출 X")
    void quotaExceededThrows() {
        willThrow(new CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED))
                .given(quotaService).checkAndIncrement(1L);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUOTA_EXCEEDED);

        verify(classifier, never()).classify(anyString());
    }

    @Test
    @DisplayName("비공개 레시피 — RECIPE_PRIVATE_ACCESS_DENIED propagate")
    void privateRecipeBlocked() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(recipeLoader).loadAsPromptText(1L, 60L);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verify(quotaService, never()).checkAndIncrement(anyLong());
        verify(classifier, never()).classify(anyString());
    }

    @Test
    @DisplayName("질문 길이 초과 — CHAT_QUESTION_TOO_LONG throw")
    void tooLongQuestionThrows() {
        String longQuestion = "a".repeat(600);
        given(chatConfig.getIntValue("max_question_length")).willReturn(500);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, longQuestion))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_QUESTION_TOO_LONG);
    }

    @Test
    @DisplayName("RepetitionGuard truncate — ChatLog.repetitionDetected=true 저장")
    void repetitionDetectedRecordedInChatLog() {
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 5));
        given(generator.generate(anyString(), anyString(), any()))
                .willReturn(new GenerationResult("반복되는 답변 반복되는 답변 반복되는 답변", 1000, 50, 200));
        given(repetitionGuard.guard("반복되는 답변 반복되는 답변 반복되는 답변"))
                .willReturn("반복되는...");

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        assertThat(response.getAnswer()).isEqualTo("반복되는...");

        ArgumentCaptor<ChatLog> captor = ArgumentCaptor.forClass(ChatLog.class);
        verify(chatLogService).saveAsync(captor.capture());
        ChatLog saved = captor.getValue();
        assertThat(saved.isRepetitionDetected()).isTrue();
        assertThat(saved.isAnswerTruncated()).isTrue();
    }

    @Test
    @DisplayName("AnswerValidator invalid — reject으로 fallback, fromLlm=false")
    void promptLeakDetectedFallbacksToReject() {
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 5));
        given(generator.generate(anyString(), anyString(), any()))
                .willReturn(new GenerationResult("# 4가지 원칙 노출", 1000, 50, 50));
        given(answerValidator.validate("# 4가지 원칙 노출"))
                .willReturn(new ValidationResult(false, "potential_prompt_leak:# 4가지 원칙"));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        assertThat(response.isFromLlm()).isFalse();
        assertThat(response.getAnswer()).isEqualTo("거부 메시지입니다.");
    }

    @Test
    @DisplayName("Suspicious 질문 — ChatLog.suspicious=true 기록 (분류는 그대로 진행)")
    void suspiciousFlaggedInChatLog() {
        given(suspiciousDetector.detect(anyString()))
                .willReturn(new SuspiciousResult(true, "injection_attempt"));
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.OUT_OF_SCOPE, 100, 5));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION);

        ArgumentCaptor<ChatLog> captor = ArgumentCaptor.forClass(ChatLog.class);
        verify(chatLogService).saveAsync(captor.capture());
        ChatLog saved = captor.getValue();
        assertThat(saved.isSuspicious()).isTrue();
        assertThat(saved.getSuspiciousReason()).isEqualTo("injection_attempt");
        assertThat(response.getIntent()).isEqualTo(Intent.OUT_OF_SCOPE);
    }

    @Test
    @DisplayName("history 있으면 chronological order로 messages 변환 후 generator에 전달")
    void historyConvertedToChronologicalMessages() {
        // chatLogRepository는 DESC로 반환 (최신 먼저). loadRecentHistory가 ASC로 뒤집어야 함.
        ChatLog newer = ChatLog.builder().question("Q2").answer("A2").build();
        ChatLog older = ChatLog.builder().question("Q1").answer("A1").build();
        given(chatLogRepository.findRecentForContext(anyLong(), anyLong(), any(), any()))
                .willReturn(List.of(newer, older));  // DESC

        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 5));
        given(generator.generate(anyString(), anyString(), any()))
                .willReturn(new GenerationResult("ok", 1000, 50, 50));

        chatService.chat(1L, RECIPE_ID, QUESTION);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<Message>> historyCaptor = ArgumentCaptor.forClass(List.class);
        verify(generator).generate(eq("이거 매워요?"), eq("recipe text"), historyCaptor.capture());

        List<Message> history = historyCaptor.getValue();
        assertThat(history).hasSize(4);  // 2 turns × (user + assistant)
        assertThat(history.get(0)).isInstanceOf(UserMessage.class);
        assertThat(history.get(0).getText()).isEqualTo("Q1");  // older first (ASC)
        assertThat(history.get(1)).isInstanceOf(AssistantMessage.class);
        assertThat(history.get(1).getText()).isEqualTo("A1");
        assertThat(history.get(2)).isInstanceOf(UserMessage.class);
        assertThat(history.get(2).getText()).isEqualTo("Q2");  // newer second
        assertThat(history.get(3)).isInstanceOf(AssistantMessage.class);
        assertThat(history.get(3).getText()).isEqualTo("A2");
    }

    @Test
    @DisplayName("admin bypass — rateLimit / quota skip, killswitch는 그대로 적용")
    void adminBypassSkipsRateLimitAndQuotaButNotKillswitch() {
        given(classifier.classify(anyString()))
                .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 5));
        given(generator.generate(anyString(), anyString(), any()))
                .willReturn(new GenerationResult("관리자 답변", 1000, 50, 200));

        ChatResponse response = chatService.chat(1L, RECIPE_ID, QUESTION, true);

        assertThat(response.getIntent()).isEqualTo(Intent.IN_SCOPE);
        verify(rateLimitService, never()).checkUserRate(anyLong());
        verify(quotaService, never()).checkAndIncrement(anyLong());
        verify(chatLogService).saveAsync(any(ChatLog.class));
    }

    @Test
    @DisplayName("admin bypass=true 여도 chat_enabled=false면 CHAT_DISABLED throw (킬스위치는 admin도 적용)")
    void adminBypassStillRespectsKillswitch() {
        given(chatConfig.getBoolValue("chat_enabled")).willReturn(false);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION, true))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.CHAT_DISABLED);
    }

    @Test
    @DisplayName("CustomException 발생 시 errorMessage 포함된 ChatLog 저장 후 rethrow")
    void exceptionPathStillSavesChatLogWithErrorMessage() {
        willThrow(new CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED))
                .given(quotaService).checkAndIncrement(1L);

        assertThatThrownBy(() -> chatService.chat(1L, RECIPE_ID, QUESTION))
                .isInstanceOf(CustomException.class);

        ArgumentCaptor<ChatLog> captor = ArgumentCaptor.forClass(ChatLog.class);
        verify(chatLogService).saveAsync(captor.capture());
        ChatLog saved = captor.getValue();
        assertThat(saved.getErrorMessage()).startsWith("705:");
        assertThat(saved.getIntent()).isEqualTo(Intent.UNKNOWN.name());
    }
}
