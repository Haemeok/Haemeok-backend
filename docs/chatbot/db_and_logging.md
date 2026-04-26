# 레시피 챗봇 - DB 설정 & 로깅 보강 설계

MD 기본 구조에 **2가지 중요 기능** 추가:
1. DB 기반 쿼터 설정 (운영 중 실시간 변경 가능)
2. 대화 기록 저장 (모니터링·분석·증거)

---

## 1. DB 스키마 추가

### 1.1 chat_config (동적 설정 테이블)

```sql
CREATE TABLE chat_config (
    id BIGSERIAL PRIMARY KEY,
    config_key VARCHAR(100) NOT NULL UNIQUE,
    config_value VARCHAR(500) NOT NULL,
    description TEXT,
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_by VARCHAR(100)
);

-- 초기 데이터
INSERT INTO chat_config (config_key, config_value, description) VALUES
('daily_quota_per_user', '20', '유저당 일일 채팅 쿼터'),
('chat_enabled', 'true', '챗봇 기능 활성화 여부 (긴급 차단용)'),
('mini_classifier_enabled', 'true', 'Mini 분류기 사용 여부 (false면 바로 Pro)'),
('max_question_length', '500', '유저 질문 최대 길이 (문자)');
```

### 1.2 chat_log (대화 기록)

```sql
CREATE TABLE chat_log (
    id BIGSERIAL PRIMARY KEY,
    
    -- 누가, 어디서
    user_id BIGINT NOT NULL,
    recipe_id BIGINT NOT NULL,
    session_id VARCHAR(50),  -- 같은 대화 세션 묶기 (옵션)
    
    -- 무엇을 물었고, 어떻게 답했나
    question TEXT NOT NULL,
    intent VARCHAR(20) NOT NULL,  -- IN_SCOPE / OUT_OF_SCOPE / UNCLEAR
    answer TEXT NOT NULL,
    pro_called BOOLEAN NOT NULL,  -- Pro 실제 호출 여부
    
    -- 성능·비용 추적
    mini_latency_ms INTEGER,
    pro_latency_ms INTEGER,
    total_latency_ms INTEGER NOT NULL,
    mini_input_tokens INTEGER,
    mini_output_tokens INTEGER,
    pro_input_tokens INTEGER,
    pro_cached_tokens INTEGER,
    pro_output_tokens INTEGER,
    estimated_cost_krw DECIMAL(10, 4),  -- 원 단위 (소수점 4자리)
    
    -- 후처리
    repetition_detected BOOLEAN DEFAULT FALSE,
    answer_truncated BOOLEAN DEFAULT FALSE,
    
    -- 프롬프트 버전 추적 (나중에 A/B 테스트 위해)
    classifier_version VARCHAR(20) NOT NULL DEFAULT 'v2',
    chat_version VARCHAR(20) NOT NULL DEFAULT 'v6',
    
    -- 에러
    error_message TEXT,
    
    -- 유저 피드백 (나중 추가)
    user_feedback VARCHAR(20),  -- GOOD / BAD / NULL
    feedback_comment TEXT,
    
    -- 시간
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    
    -- 인덱스
    INDEX idx_chat_log_user_id_created (user_id, created_at DESC),
    INDEX idx_chat_log_recipe_id (recipe_id),
    INDEX idx_chat_log_intent (intent),
    INDEX idx_chat_log_created (created_at DESC)
);
```

### 1.3 chat_daily_usage (일일 쿼터 카운터)

```sql
CREATE TABLE chat_daily_usage (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    usage_date DATE NOT NULL,
    call_count INTEGER NOT NULL DEFAULT 0,
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    
    UNIQUE (user_id, usage_date),
    INDEX idx_chat_daily_usage_date (usage_date)
);
```

**참고**: 기존 `DailyQuotaService`가 테이블을 어떻게 쓰는지 확인 후, 같은 패턴이면 재사용. 다르면 위 테이블로 별도 관리.

---

## 2. 자바 구현 추가

### 2.1 ChatConfigService (동적 설정 조회)

```java
// service/chat/ChatConfigService.java
package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.ChatConfig;
import com.jdc.recipe_service.domain.repository.ChatConfigRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ChatConfigService {
    
    private final ChatConfigRepository repository;
    
    @Cacheable(value = "chatConfig", key = "#key")
    public String getValue(String key) {
        return repository.findByConfigKey(key)
            .map(ChatConfig::getConfigValue)
            .orElseThrow(() -> new IllegalStateException("Config not found: " + key));
    }
    
    public int getIntValue(String key) {
        return Integer.parseInt(getValue(key));
    }
    
    public boolean getBoolValue(String key) {
        return Boolean.parseBoolean(getValue(key));
    }
    
    // 관리자가 변경 시 캐시 무효화
    @CacheEvict(value = "chatConfig", key = "#key")
    public void updateValue(String key, String newValue, String updatedBy) {
        ChatConfig config = repository.findByConfigKey(key)
            .orElseThrow(() -> new IllegalStateException("Config not found: " + key));
        config.setConfigValue(newValue);
        config.setUpdatedBy(updatedBy);
        repository.save(config);
    }
}
```

**캐시 전략**: `@Cacheable`로 DB 조회 최소화. 설정 변경 시 `@CacheEvict`로 무효화 → **즉시 반영**.

### 2.2 ChatLogService (기록 저장)

```java
// service/chat/ChatLogService.java
package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.ChatLog;
import com.jdc.recipe_service.domain.repository.ChatLogRepository;
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
    
    /**
     * 대화 로그 비동기 저장 (응답 속도 영향 X)
     * 유저에게 답변은 이미 보냈고, 백그라운드에서 DB 저장.
     */
    @Async
    @Transactional
    public void saveAsync(ChatLog log) {
        try {
            repository.save(log);
        } catch (Exception e) {
            // 로깅 실패가 전체 응답 실패로 이어지면 안 됨
            log.error("Failed to save chat log", e);
        }
    }
}
```

**핵심**: `@Async`로 **비동기 저장**. 유저는 이미 답변 받았으니 DB 저장은 백그라운드.

### 2.3 ChatService 통합 (수정)

```java
// service/chat/ChatService.java
@Service
@RequiredArgsConstructor
public class ChatService {
    
    private final IntentClassifier classifier;
    private final AnswerGenerator generator;
    private final RepetitionGuard guard;
    private final PromptLoader prompts;
    private final RecipeService recipeService;
    private final ChatConfigService chatConfig;      // NEW
    private final ChatLogService chatLogService;     // NEW
    private final DailyQuotaService quotaService;    // NEW (기존 재사용)
    
    public ChatResponse chat(Long userId, ChatRequest request) {
        long start = System.currentTimeMillis();
        
        // 1. 킬스위치 체크
        if (!chatConfig.getBoolValue("chat_enabled")) {
            throw new CustomException(ErrorCode.CHAT_DISABLED);
        }
        
        // 2. 쿼터 체크
        int quota = chatConfig.getIntValue("daily_quota_per_user");
        quotaService.checkAndIncrement(userId, "chat", quota);  
        // 초과 시 CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED)
        
        // 3. 질문 길이 체크
        int maxLen = chatConfig.getIntValue("max_question_length");
        if (request.question().length() > maxLen) {
            throw new CustomException(ErrorCode.CHAT_QUESTION_TOO_LONG);
        }
        
        // 4. Recipe 로드 + String 변환 (트랜잭션 안)
        String recipeText = recipeService.loadAsPromptString(request.recipeId());
        
        // 5. Mini 분류 (트랜잭션 밖)
        long miniStart = System.currentTimeMillis();
        ClassificationResult miniResult = classifier.classify(request.question());
        long miniLatency = System.currentTimeMillis() - miniStart;
        
        // 6. 분기 처리
        String answer;
        boolean proCalled = false;
        long proLatency = 0;
        GenerationResult proResult = null;
        boolean repetitionDetected = false;
        boolean truncated = false;
        
        Intent intent = miniResult.intent();
        
        if (intent == Intent.OUT_OF_SCOPE) {
            answer = prompts.get("reject");
        } else if (intent == Intent.UNCLEAR) {
            answer = prompts.get("unclear");
        } else {
            // IN_SCOPE → Pro 호출
            long proStart = System.currentTimeMillis();
            proResult = generator.generate(request.question(), recipeText);
            proLatency = System.currentTimeMillis() - proStart;
            proCalled = true;
            
            // 후처리
            String guarded = guard.guard(proResult.answer());
            repetitionDetected = !guarded.equals(proResult.answer());
            truncated = repetitionDetected;
            answer = guarded;
        }
        
        long totalLatency = System.currentTimeMillis() - start;
        
        // 7. 로그 저장 (비동기)
        ChatLog log = ChatLog.builder()
            .userId(userId)
            .recipeId(request.recipeId())
            .question(request.question())
            .intent(intent.name())
            .answer(answer)
            .proCalled(proCalled)
            .miniLatencyMs((int) miniLatency)
            .proLatencyMs((int) proLatency)
            .totalLatencyMs((int) totalLatency)
            .miniInputTokens(miniResult.inputTokens())
            .miniOutputTokens(miniResult.outputTokens())
            .proInputTokens(proResult != null ? proResult.inputTokens() : 0)
            .proCachedTokens(proResult != null ? proResult.cachedTokens() : 0)
            .proOutputTokens(proResult != null ? proResult.outputTokens() : 0)
            .estimatedCostKrw(calculateCost(miniResult, proResult))
            .repetitionDetected(repetitionDetected)
            .answerTruncated(truncated)
            .classifierVersion("v2")
            .chatVersion("v6")
            .build();
        
        chatLogService.saveAsync(log);  // 비동기 저장
        
        return new ChatResponse(answer, intent, proCalled);
    }
    
    private BigDecimal calculateCost(ClassificationResult mini, GenerationResult pro) {
        // Mini: flat $0.15/1M
        double miniCost = (mini.inputTokens() + mini.outputTokens()) * 0.15 / 1_000_000;
        // Pro: input $0.15, cached $0.015, output $0.60
        double proCost = 0;
        if (pro != null) {
            int uncached = pro.inputTokens() - pro.cachedTokens();
            proCost = (uncached * 0.15 + pro.cachedTokens() * 0.015 + pro.outputTokens() * 0.60) / 1_000_000;
        }
        // USD → KRW
        double totalKrw = (miniCost + proCost) * 1400;
        return BigDecimal.valueOf(totalKrw).setScale(4, RoundingMode.HALF_UP);
    }
}
```

### 2.4 DailyQuotaService 확장

기존 `DailyQuotaService`가 `ai.quota.per-day` (레시피 생성용)만 지원한다면, 챗봇용 분리 필요:

```java
// 기존: checkAndIncrement(userId) - 레시피 생성용
// 추가: checkAndIncrement(userId, String quotaType, int limit)

public void checkAndIncrement(Long userId, String quotaType, int limit) {
    LocalDate today = LocalDate.now();
    ChatDailyUsage usage = chatDailyUsageRepository
        .findByUserIdAndUsageDate(userId, today)
        .orElseGet(() -> ChatDailyUsage.builder()
            .userId(userId)
            .usageDate(today)
            .callCount(0)
            .build());
    
    if (usage.getCallCount() >= limit) {
        throw new CustomException(ErrorCode.CHAT_QUOTA_EXCEEDED);
    }
    
    usage.setCallCount(usage.getCallCount() + 1);
    chatDailyUsageRepository.save(usage);
}
```

### 2.5 @EnableAsync 설정 필요

```java
// config/AsyncConfig.java (이미 있을 수 있음)
@Configuration
@EnableAsync
public class AsyncConfig {
    @Bean("chatLogExecutor")
    public Executor chatLogExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(2);
        executor.setMaxPoolSize(5);
        executor.setQueueCapacity(100);
        executor.setThreadNamePrefix("chat-log-");
        executor.initialize();
        return executor;
    }
}
```

---

## 3. 추가 ErrorCode

기존 703~ 대역에 추가:

```java
// 703: CHAT_CLASSIFICATION_FAILED (Mini 호출 실패)
// 704: CHAT_ANSWER_GENERATION_FAILED (Pro 호출 실패)
// 705: CHAT_QUOTA_EXCEEDED (일일 쿼터 초과)
// 706: CHAT_DISABLED (관리자가 비활성화)
// 707: CHAT_QUESTION_TOO_LONG (질문 너무 김)
```

---

## 4. 관리자 API (나중 단계, 우선순위 낮음)

```java
@RestController
@RequestMapping("/api/admin/chat-config")
@PreAuthorize("hasRole('ADMIN')")
@RequiredArgsConstructor
public class ChatConfigController {
    
    private final ChatConfigService chatConfigService;
    
    @GetMapping
    public ResponseEntity<Map<String, String>> getAll() { ... }
    
    @PutMapping("/{key}")
    public ResponseEntity<Void> update(
        @PathVariable String key,
        @RequestBody UpdateConfigRequest request,
        @AuthenticationPrincipal UserPrincipal admin
    ) {
        chatConfigService.updateValue(key, request.value(), admin.getUsername());
        return ResponseEntity.ok().build();
    }
}
```

**MVP 단계에서는 DB 직접 UPDATE로 충분**. 관리자 페이지는 나중에.

---

## 5. 모니터링 쿼리 예시

운영 중에 돌릴 유용한 쿼리:

```sql
-- 오늘 호출 수·비용
SELECT 
    COUNT(*) as calls,
    SUM(estimated_cost_krw) as total_cost,
    AVG(total_latency_ms) as avg_latency
FROM chat_log 
WHERE created_at >= CURRENT_DATE;

-- Intent 분포
SELECT intent, COUNT(*) 
FROM chat_log 
WHERE created_at >= NOW() - INTERVAL '7 days'
GROUP BY intent;

-- 가장 많이 질문받는 레시피 TOP 10
SELECT recipe_id, COUNT(*) as question_count
FROM chat_log
WHERE created_at >= NOW() - INTERVAL '30 days'
GROUP BY recipe_id
ORDER BY question_count DESC
LIMIT 10;

-- UNCLEAR로 분류된 질문들 (분류기 개선 기반)
SELECT question, COUNT(*) as cnt
FROM chat_log
WHERE intent = 'UNCLEAR'
  AND created_at >= NOW() - INTERVAL '7 days'
GROUP BY question
ORDER BY cnt DESC
LIMIT 20;

-- 유저별 오늘 사용량 (쿼터 모니터링)
SELECT user_id, COUNT(*) as today_calls
FROM chat_log
WHERE created_at >= CURRENT_DATE
GROUP BY user_id
HAVING COUNT(*) >= 15
ORDER BY today_calls DESC;
```

---

## 체크리스트

Claude Code가 구현할 때 체크할 것:

- [ ] `chat_config`, `chat_log`, `chat_daily_usage` 테이블 생성 (Flyway/Liquibase 있으면 migration 파일로)
- [ ] ChatConfig, ChatLog, ChatDailyUsage 엔티티 생성
- [ ] `@EnableAsync` 활성화 (이미 있으면 재사용)
- [ ] ChatConfigService + @Cacheable 설정
- [ ] ChatLogService + @Async 저장
- [ ] DailyQuotaService 확장 (챗봇용 오버로드 메서드)
- [ ] ErrorCode 703~707 추가
- [ ] chat_config 초기 데이터 INSERT (마이그레이션에 포함)
