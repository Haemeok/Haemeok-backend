package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.dto.chat.ChatHistoryItem;
import com.jdc.recipe_service.domain.dto.chat.ChatResponse;
import com.jdc.recipe_service.domain.dto.chat.QuotaResponse;
import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import com.jdc.recipe_service.domain.repository.chat.ChatLogRepository;
import com.jdc.recipe_service.domain.type.chat.Intent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatService {

    private static final BigDecimal MINI_PER_TOKEN_USD       = new BigDecimal("0.00000015");
    private static final BigDecimal PRO_INPUT_PER_TOKEN_USD  = new BigDecimal("0.00000015");
    private static final BigDecimal PRO_CACHED_PER_TOKEN_USD = new BigDecimal("0.000000015");
    private static final BigDecimal PRO_OUTPUT_PER_TOKEN_USD = new BigDecimal("0.00000060");
    private static final BigDecimal USD_TO_KRW               = new BigDecimal("1400");

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");
    private static final long HISTORY_GAP_HOURS = 6;
    private static final int HISTORY_TRAILING_TURNS = 5;

    private final ChatConfigService chatConfig;
    private final ChatQuotaService quotaService;
    private final RateLimitService rateLimitService;
    private final ChatRecipeLoader recipeLoader;
    private final IntentClassifier classifier;
    private final AnswerGenerator generator;
    private final RepetitionGuard repetitionGuard;
    private final SuspiciousDetector suspiciousDetector;
    private final AnswerValidator answerValidator;
    private final ChatLogService chatLogService;
    private final ChatLogRepository chatLogRepository;
    private final PromptLoader prompts;

    public ChatResponse chat(Long userId, Long recipeId, String question, String sessionId) {
        return chat(userId, recipeId, question, sessionId, false);
    }

    /**
     * @param sessionId    페이지/컴포넌트 단위 대화 세션. null이면 stateless (history 미합침).
     * @param bypassLimits true면 rate-limit + daily quota skip (ROLE_ADMIN 전용).
     *                     killswitch와 chat_log 기록은 동일하게 적용.
     */
    public ChatResponse chat(Long userId, Long recipeId, String question, String sessionId, boolean bypassLimits) {
        long start = System.currentTimeMillis();
        ChatLog.ChatLogBuilder logBuilder = ChatLog.builder()
                .userId(userId)
                .recipeId(recipeId)
                .sessionId(sessionId);

        // 부분 정보 보존을 위해 try 밖에 선언 (catch 시점까지 수집된 값 그대로 chat_log에 기록).
        String cleanQuestion = null;
        SuspiciousResult suspicious = null;
        ClassificationResult miniResult = null;
        GenerationResult proResult = null;
        long miniLatency = 0;
        long proLatency = 0;
        boolean repetitionDetected = false;
        boolean answerTruncated = false;

        try {
            if (!chatConfig.getBoolValue("chat_enabled")) {
                throw new CustomException(ErrorCode.CHAT_DISABLED);
            }
            if (!bypassLimits) {
                rateLimitService.checkUserRate(userId);
            }

            cleanQuestion = InputSanitizer.sanitize(question);
            if (cleanQuestion == null || cleanQuestion.isBlank()) {
                throw new CustomException(ErrorCode.CHAT_INVALID_QUESTION);
            }
            int maxLen = chatConfig.getIntValue("max_question_length");
            if (cleanQuestion.length() > maxLen) {
                throw new CustomException(ErrorCode.CHAT_QUESTION_TOO_LONG);
            }
            suspicious = suspiciousDetector.detect(cleanQuestion);

            String recipeText = recipeLoader.loadAsPromptText(userId, recipeId);
            if (!bypassLimits) {
                quotaService.checkAndIncrement(userId);
            }

            List<Message> historyMessages = loadRecentHistory(userId, recipeId, sessionId);

            if (chatConfig.getBoolValue("mini_classifier_enabled")) {
                long miniStart = System.currentTimeMillis();
                miniResult = classifier.classify(cleanQuestion, historyMessages);
                miniLatency = System.currentTimeMillis() - miniStart;
            } else {
                miniResult = new ClassificationResult(Intent.IN_SCOPE, 0, 0);
            }

            Intent intent = miniResult.intent();
            String answer;
            boolean fromLlm;

            if (intent == Intent.OUT_OF_SCOPE) {
                answer = staticResponse("reject");
                fromLlm = false;
            } else {
                long proStart = System.currentTimeMillis();
                proResult = generator.generate(cleanQuestion, recipeText, historyMessages);
                proLatency = System.currentTimeMillis() - proStart;

                String guarded = repetitionGuard.guard(proResult.answer());
                repetitionDetected = !guarded.equals(proResult.answer());
                answerTruncated = repetitionDetected;

                ValidationResult validation = answerValidator.validate(guarded);
                if (!validation.valid()) {
                    log.warn("Prompt leak detected, fallback to reject: reason={}", validation.reason());
                    answer = staticResponse("reject");
                    fromLlm = false;
                } else {
                    answer = guarded;
                    fromLlm = true;
                }
            }

            long totalLatency = System.currentTimeMillis() - start;
            ChatLog logEntry = buildChatLog(logBuilder,
                    cleanQuestion, intent, answer, fromLlm,
                    miniResult, miniLatency, proResult, proLatency,
                    suspicious, repetitionDetected, answerTruncated,
                    (int) totalLatency, null);
            chatLogService.saveAsync(logEntry);

            log.info("Chat completed: userId={} recipeId={} intent={} elapsed={}ms answerLen={}",
                    userId, recipeId, intent, totalLatency, answer.length());

            return ChatResponse.builder()
                    .answer(answer)
                    .intent(intent)
                    .fromLlm(fromLlm)
                    .build();

        } catch (CustomException e) {
            long totalLatency = System.currentTimeMillis() - start;
            String questionForLog = cleanQuestion != null ? cleanQuestion : question;
            ChatLog errorLog = buildChatLog(logBuilder,
                    questionForLog, Intent.UNKNOWN, "", false,
                    miniResult, miniLatency, proResult, proLatency,
                    suspicious, repetitionDetected, answerTruncated,
                    (int) totalLatency,
                    e.getErrorCode().getCode() + ":" + e.getMessage());
            chatLogService.saveAsync(errorLog);
            throw e;
        }
    }

    @Transactional(readOnly = true)
    public List<ChatHistoryItem> getHistoryForDisplay(Long userId, Long recipeId, int limit) {
        int safeLimit = Math.max(1, Math.min(limit, 50));
        return chatLogRepository
                .findForDisplay(userId, recipeId, PageRequest.of(0, safeLimit))
                .stream()
                .map(ChatHistoryItem::from)
                .toList();
    }

    @Transactional(readOnly = true)
    public QuotaResponse getQuota(Long userId) {
        int dailyLimit = chatConfig.getIntValue("daily_quota_per_user");
        int remaining = quotaService.getRemainingQuota(userId);
        int used = Math.max(0, dailyLimit - remaining);
        LocalDateTime resetAt = LocalDate.now(KST).plusDays(1).atStartOfDay();
        return QuotaResponse.builder()
                .dailyLimit(dailyLimit)
                .used(used)
                .remaining(remaining)
                .resetAt(resetAt)
                .build();
    }

    private List<Message> loadRecentHistory(Long userId, Long recipeId, String sessionId) {
        // sessionId 없으면 stateless — 페이지/세션 boundary 표시 X.
        if (sessionId == null || sessionId.isBlank()) return Collections.emptyList();

        LocalDateTime since = LocalDateTime.now(KST).minusHours(HISTORY_GAP_HOURS);
        List<ChatLog> recentDesc = chatLogRepository.findRecentForContext(
                userId, recipeId, sessionId, since, PageRequest.of(0, HISTORY_TRAILING_TURNS));
        if (recentDesc.isEmpty()) return Collections.emptyList();

        List<Message> messages = new ArrayList<>(recentDesc.size() * 2);
        for (int i = recentDesc.size() - 1; i >= 0; i--) {
            ChatLog log = recentDesc.get(i);
            messages.add(new UserMessage(log.getQuestion()));
            messages.add(new AssistantMessage(log.getAnswer()));
        }
        return messages;
    }

    private String staticResponse(String key) {
        String content = prompts.get(key);
        List<String> variants = Arrays.stream(content.split("(?m)^---\\s*$"))
                .map(String::trim)
                .filter(s -> !s.isBlank())
                .toList();
        if (variants.isEmpty()) {
            return content;
        }
        return variants.get(ThreadLocalRandom.current().nextInt(variants.size()));
    }

    private BigDecimal calculateCost(ClassificationResult mini, GenerationResult pro) {
        BigDecimal usd = BigDecimal.ZERO;

        if (mini != null) {
            BigDecimal miniTokens = BigDecimal.valueOf(mini.inputTokens() + mini.outputTokens());
            usd = usd.add(miniTokens.multiply(MINI_PER_TOKEN_USD));
        }
        if (pro != null) {
            int uncached = pro.inputTokens() - pro.cachedTokens();
            BigDecimal uncachedCost = BigDecimal.valueOf(uncached).multiply(PRO_INPUT_PER_TOKEN_USD);
            BigDecimal cachedCost = BigDecimal.valueOf(pro.cachedTokens()).multiply(PRO_CACHED_PER_TOKEN_USD);
            BigDecimal outputCost = BigDecimal.valueOf(pro.outputTokens()).multiply(PRO_OUTPUT_PER_TOKEN_USD);
            usd = usd.add(uncachedCost).add(cachedCost).add(outputCost);
        }

        return usd.multiply(USD_TO_KRW).setScale(4, RoundingMode.HALF_UP);
    }

    private ChatLog buildChatLog(ChatLog.ChatLogBuilder builder,
                                 String question, Intent intent, String answer, boolean fromLlm,
                                 ClassificationResult miniResult, long miniLatency,
                                 GenerationResult proResult, long proLatency,
                                 SuspiciousResult suspicious,
                                 boolean repetitionDetected, boolean answerTruncated,
                                 int totalLatencyMs, String errorMessage) {
        builder
                .question(question != null ? question : "")
                .intent(intent.name())
                .answer(answer != null ? answer : "")
                .proCalled(fromLlm)
                .totalLatencyMs(totalLatencyMs)
                .errorMessage(errorMessage)
                .repetitionDetected(repetitionDetected)
                .answerTruncated(answerTruncated)
                .estimatedCostKrw(calculateCost(miniResult, proResult));

        if (miniResult != null) {
            builder.miniLatencyMs((int) miniLatency)
                    .miniInputTokens(miniResult.inputTokens())
                    .miniOutputTokens(miniResult.outputTokens());
        }
        if (proResult != null) {
            builder.proLatencyMs((int) proLatency)
                    .proInputTokens(proResult.inputTokens())
                    .proCachedTokens(proResult.cachedTokens())
                    .proOutputTokens(proResult.outputTokens());
        }
        if (suspicious != null) {
            builder.suspicious(suspicious.suspicious())
                    .suspiciousReason(suspicious.reason());
        }
        return builder.build();
    }
}
