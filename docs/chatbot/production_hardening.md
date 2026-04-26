# 레시피 챗봇 - 프로덕션 하드닝

MVP라도 **실제 서비스에 붙일 거라면** 반드시 필요한 항목들.
기존 `db_and_logging.md`에 이어서 구현해야 함.

---

## 1. 안정성 (Resilience)

### 1.1 Upstage API 타임아웃 설정

**문제**: Upstage 서버 느려지면 유저 30초+ 대기 → 이탈

**해결**: `application.yml`에 명시적 타임아웃

```yaml
spring:
  ai:
    openai:
      base-url: https://api.upstage.ai
      api-key: ${UPSTAGE_API_KEY}
      chat:
        completions-path: /v1/solar/chat/completions
        options:
          model: solar-pro3
          temperature: 0.5
          max-tokens: 600
        
# RestClient 타임아웃 (Spring AI 1.0 내부에서 사용)
spring.http.client:
  connect-timeout: 5s      # Upstage 연결 대기 5초
  read-timeout: 30s        # 응답 읽기 30초 (긴 답변 대응)
```

**추가 Bean으로 강제**:

```java
// config/AiClientConfig.java 에 추가
@Bean
public RestClient.Builder upstageRestClientBuilder() {
    return RestClient.builder()
        .requestFactory(createRequestFactory());
}

private ClientHttpRequestFactory createRequestFactory() {
    SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
    factory.setConnectTimeout(5_000);  // 5초
    factory.setReadTimeout(30_000);    // 30초
    return factory;
}
```

### 1.2 재시도 로직 (Spring Retry)

**문제**: 일시적 네트워크 오류(503, 504)에 바로 실패 → 유저 재시도 강요

**해결**: Spring Retry로 자동 재시도

`build.gradle`:
```groovy
implementation 'org.springframework.boot:spring-boot-starter-aop'
implementation 'org.springframework.retry:spring-retry'
```

`@EnableRetry` 활성화:
```java
// config/RetryConfig.java
@Configuration
@EnableRetry
public class RetryConfig {}
```

IntentClassifier, AnswerGenerator 양쪽에 적용:

```java
@Service
@RequiredArgsConstructor
@Slf4j
public class IntentClassifier {
    
    private final ChatClient miniClient;
    private final PromptLoader prompts;
    
    @Retryable(
        retryFor = { 
            ResourceAccessException.class,      // 타임아웃
            HttpServerErrorException.class,     // 5xx
            TransientAiException.class          // Spring AI 일시 오류
        },
        maxAttempts = 3,
        backoff = @Backoff(delay = 500, multiplier = 2)  // 0.5s → 1s → 2s
    )
    public ClassificationResult classify(String question) {
        // ... 기존 로직
    }
    
    @Recover
    public ClassificationResult recoverClassify(Exception e, String question) {
        log.error("Mini 분류 최종 실패, IN_SCOPE fallback: {}", question, e);
        // 최종 실패 시 IN_SCOPE로 fallback (유저에게 답변은 주자)
        return new ClassificationResult(Intent.IN_SCOPE, 0, 0);
    }
}
```

**AnswerGenerator도 동일 패턴** (다만 `@Recover`에서 `CHAT_ANSWER_GENERATION_FAILED` 에러 발생).

### 1.3 Circuit Breaker (Phase 2 - 선택)

Spring Resilience4j로 Upstage 연속 실패 시 일시적 차단:
- 5분간 실패율 50% 초과 → 30초 동안 호출 차단
- 챗봇 기능 일시 중단, 정형 메시지 반환

**MVP에서는 재시도로 충분**. Phase 2에서 고려.

---

## 2. 보안 (Security)

### 2.1 질문 입력값 sanitize

**문제**: `<script>alert('xss')</script>` 같은 입력이 DB 저장 → 관리자 페이지 렌더링 시 XSS

**해결**: ChatController에서 질문 입력 시 sanitize

```java
// util/InputSanitizer.java (새로 만들거나 기존 util 확인)
public class InputSanitizer {
    
    private static final Pattern HTML_TAG = Pattern.compile("<[^>]+>");
    private static final int MAX_LENGTH = 500;
    
    public static String sanitize(String input) {
        if (input == null) return null;
        
        // 1. HTML 태그 제거
        String cleaned = HTML_TAG.matcher(input).replaceAll("");
        
        // 2. 연속 공백 정리
        cleaned = cleaned.replaceAll("\\s+", " ").trim();
        
        // 3. 길이 제한
        if (cleaned.length() > MAX_LENGTH) {
            cleaned = cleaned.substring(0, MAX_LENGTH);
        }
        
        return cleaned;
    }
}
```

**적용**:
```java
// ChatController or ChatService 초입
String cleanQuestion = InputSanitizer.sanitize(request.question());
if (cleanQuestion.isBlank()) {
    throw new CustomException(ErrorCode.CHAT_INVALID_QUESTION);
}
```

**참고**: OWASP Java Encoder 라이브러리 쓰면 더 철저. 기존 프로젝트에 있으면 재사용.

### 2.2 Recipe 접근 권한 체크

**문제**: 유저가 자기 레시피가 아닌 `recipeId`로 접근 가능?

**정책 결정 필요**:
- 공개 레시피 앱이면 상관없음 (모든 레시피 조회 가능)
- 유저별 비공개 레시피 있으면 반드시 체크

```java
// ChatService에서
Recipe recipe = recipeRepository.findById(recipeId)
    .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

// 정책에 따라: 비공개 레시피면 소유자만 접근
if (recipe.getVisibility() == Visibility.PRIVATE 
    && !recipe.getOwnerId().equals(userId)) {
    throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
}
```

**Claude Code에 확인 요청**: "기존 Recipe에 visibility/owner 필드 있는지, 있다면 챗봇에서도 동일 정책 적용"

### 2.3 인젝션 공격 패턴 별도 추적

**문제**: chat_log에 전부 섞여서 공격 패턴 분석 어려움

**해결**: chat_log에 플래그 추가 (db_and_logging.md 수정)

```sql
ALTER TABLE chat_log ADD COLUMN suspicious BOOLEAN DEFAULT FALSE;
ALTER TABLE chat_log ADD COLUMN suspicious_reason VARCHAR(100);
```

감지 로직:
```java
// service/chat/SuspiciousDetector.java
@Component
public class SuspiciousDetector {
    
    private static final List<Pattern> INJECTION_PATTERNS = List.of(
        Pattern.compile("(?i)ignore\\s+(all\\s+)?previous", Pattern.CASE_INSENSITIVE),
        Pattern.compile("(?i)system\\s*prompt", Pattern.CASE_INSENSITIVE),
        Pattern.compile("(?i)너는\\s+이제", Pattern.CASE_INSENSITIVE),
        Pattern.compile("(?i)이전\\s+지시", Pattern.CASE_INSENSITIVE),
        Pattern.compile("(?i)repeat\\s+(above|the)\\s+instructions", Pattern.CASE_INSENSITIVE)
    );
    
    public SuspiciousResult detect(String question, Intent classifiedAs) {
        for (Pattern p : INJECTION_PATTERNS) {
            if (p.matcher(question).find()) {
                return new SuspiciousResult(true, "injection_attempt");
            }
        }
        // OUT_OF_SCOPE인데도 여러 번 시도한 경우 (쿼터 누적)
        // → 별도 로직으로 판단 가능
        return new SuspiciousResult(false, null);
    }
}
```

ChatLog 저장 시 플래그 포함. 관리자가 주기적으로 조회:
```sql
SELECT user_id, question, COUNT(*) 
FROM chat_log 
WHERE suspicious = TRUE 
  AND created_at >= NOW() - INTERVAL '7 days'
GROUP BY user_id, question
ORDER BY COUNT(*) DESC;
```

### 2.4 답변에 시스템 프롬프트 유출 감지

**문제**: Pro가 실수로 시스템 프롬프트 내용 답변에 포함 가능

**해결**: RepetitionGuard 옆에 유출 감지 추가

```java
// service/chat/AnswerValidator.java
@Component
public class AnswerValidator {
    
    private static final List<String> LEAK_PATTERNS = List.of(
        "4가지 원칙",
        "절대 금지",
        "답변 구조",
        "분류기",
        "system prompt",
        "IN_SCOPE",
        "OUT_OF_SCOPE",
        "UNCLEAR"
    );
    
    public ValidationResult validate(String answer) {
        for (String pattern : LEAK_PATTERNS) {
            if (answer.contains(pattern)) {
                return new ValidationResult(false, "potential_prompt_leak:" + pattern);
            }
        }
        return new ValidationResult(true, null);
    }
}
```

**정책**: 유출 감지 시 정형 거부 답변으로 대체 + 관리자 알림

---

## 3. Rate Limit (Burst 방어)

### 문제

일일 쿼터만으론 1초에 100번 공격 방어 불가.

### 해결: Bucket4j

`build.gradle`:
```groovy
implementation 'com.bucket4j:bucket4j-core:8.10.1'
```

```java
// config/RateLimitConfig.java
@Configuration
public class RateLimitConfig {
    
    @Bean
    public ConcurrentHashMap<Long, Bucket> userBuckets() {
        return new ConcurrentHashMap<>();
    }
}

// service/chat/RateLimitService.java
@Service
@RequiredArgsConstructor
public class RateLimitService {
    
    private final ConcurrentHashMap<Long, Bucket> userBuckets;
    
    public void checkUserRate(Long userId) {
        Bucket bucket = userBuckets.computeIfAbsent(userId, this::createBucket);
        if (!bucket.tryConsume(1)) {
            throw new CustomException(ErrorCode.CHAT_RATE_LIMITED);
        }
    }
    
    private Bucket createBucket(Long userId) {
        // 유저당: 분당 최대 10건 (burst 10개), 이후 6초당 1개 리필
        Bandwidth limit = Bandwidth.classic(
            10,  // capacity
            Refill.intervally(10, Duration.ofMinutes(1))
        );
        return Bucket.builder()
            .addLimit(limit)
            .build();
    }
}
```

**ChatService.chat()에서 호출**:
```java
public ChatResponse chat(Long userId, ChatRequest request) {
    // 킬스위치 체크
    // Rate limit 체크 (burst)  ← 추가
    rateLimitService.checkUserRate(userId);
    // 일일 쿼터 체크
    // ... 
}
```

**레벨별**:
- 1차: Rate limit (분당 10건, burst 방어)
- 2차: 일일 쿼터 (일 20건, 비용 방어)

---

## 4. Health Check

### 4.1 Actuator 활성화

`build.gradle`:
```groovy
implementation 'org.springframework.boot:spring-boot-starter-actuator'
```

`application.yml`:
```yaml
management:
  endpoints:
    web:
      exposure:
        include: health, info, metrics
  endpoint:
    health:
      show-details: when-authorized
      probes:
        enabled: true
```

### 4.2 커스텀 Health Indicator (Upstage 상태)

```java
// health/UpstageHealthIndicator.java
@Component
@RequiredArgsConstructor
public class UpstageHealthIndicator implements HealthIndicator {
    
    private final ChatClient miniChatClient;
    
    @Override
    public Health health() {
        try {
            // 최소 비용 호출 (max_tokens=1)
            String response = miniChatClient.prompt()
                .user("ping")
                .options(OpenAiChatOptions.builder().maxTokens(1).build())
                .call()
                .content();
            
            return Health.up()
                .withDetail("provider", "Upstage")
                .withDetail("model", "solar-mini")
                .build();
        } catch (Exception e) {
            return Health.down()
                .withDetail("error", e.getMessage())
                .build();
        }
    }
}
```

**주의**: Health check가 너무 자주 호출되면 비용 발생. `/actuator/health` 는 그대로 두고, 별도 `/health/ai`로 분리:

```java
@RestController
@RequestMapping("/api/health")
public class HealthController {
    
    private final UpstageHealthIndicator upstageHealth;
    
    // 관리자만 접근
    @GetMapping("/ai")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Map<String, Object>> ai() {
        Health h = upstageHealth.health();
        return ResponseEntity.ok(Map.of(
            "status", h.getStatus().getCode(),
            "details", h.getDetails()
        ));
    }
}
```

---

## 5. 테스트

### 5.1 단위 테스트

RepetitionGuard는 반드시 커버:

```java
// service/chat/RepetitionGuardTest.java
@ExtendWith(MockitoExtension.class)
class RepetitionGuardTest {
    
    private final RepetitionGuard guard = new RepetitionGuard();
    
    @Test
    void 반복_없는_답변은_그대로_반환() {
        String input = "안녕하세요! 김치찌개 끓이는 법을 알려드릴게요.";
        assertThat(guard.guard(input)).isEqualTo(input);
    }
    
    @Test
    void 같은_문구_3번_반복되면_자름() {
        String input = "고춧가루를 빼세요 고춧가루를 빼세요 고춧가루를 빼세요 고춧가루를 빼세요";
        String result = guard.guard(input);
        assertThat(result).doesNotContain("고춧가루를 빼세요 고춧가루를 빼세요 고춧가루를 빼세요");
    }
    
    @Test
    void null_입력_안전하게_처리() {
        assertThat(guard.guard(null)).isNull();
    }
    
    @Test
    void 짧은_답변은_그대로() {
        String input = "짧아요";
        assertThat(guard.guard(input)).isEqualTo(input);
    }
}
```

### 5.2 Upstage Mock (비용 0 테스트)

`test/resources/application-test.yml`:
```yaml
spring:
  ai:
    openai:
      base-url: http://localhost:${wiremock.port}
      api-key: test-key
```

WireMock으로 Upstage 응답 가상화:

```java
@SpringBootTest
@AutoConfigureWireMock(port = 0)
class ChatServiceIntegrationTest {
    
    @BeforeEach
    void setup() {
        stubFor(post(urlEqualTo("/v1/solar/chat/completions"))
            .withRequestBody(containing("solar-mini"))
            .willReturn(aResponse()
                .withStatus(200)
                .withBody("""
                    {"choices":[{"message":{"content":"IN_SCOPE"}}],
                     "usage":{"prompt_tokens":100,"completion_tokens":2}}
                """)));
        
        stubFor(post(urlEqualTo("/v1/solar/chat/completions"))
            .withRequestBody(containing("solar-pro3"))
            .willReturn(aResponse()
                .withStatus(200)
                .withBody("""
                    {"choices":[{"message":{"content":"김치찌개는..."}}],
                     "usage":{"prompt_tokens":500,"completion_tokens":200}}
                """)));
    }
    
    @Test
    void 정상_질문_IN_SCOPE_답변() {
        // given, when, then
    }
}
```

### 5.3 통합 테스트 (MockMvc)

```java
@SpringBootTest
@AutoConfigureMockMvc
class ChatControllerTest {
    
    @Autowired MockMvc mockMvc;
    @MockBean IntentClassifier classifier;
    @MockBean AnswerGenerator generator;
    
    @Test
    @WithMockUser
    void POST_api_chat_정상_응답() throws Exception {
        given(classifier.classify(anyString()))
            .willReturn(new ClassificationResult(Intent.IN_SCOPE, 100, 2));
        given(generator.generate(anyString(), anyString()))
            .willReturn(new GenerationResult("답변", 500, 0, 200));
        
        mockMvc.perform(post("/api/chat")
            .contentType(MediaType.APPLICATION_JSON)
            .content("""
                {"recipeId": 1, "question": "매워요?"}
            """))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.answer").value("답변"));
    }
    
    @Test
    @WithMockUser
    void 쿼터_초과_시_429() throws Exception {
        // DailyQuotaService mock
        // ...
    }
}
```

---

## 6. 추가 ErrorCode

db_and_logging.md의 703~707에 추가:

```
708: CHAT_RATE_LIMITED (burst 제한 초과)
709: CHAT_INVALID_QUESTION (빈 질문, sanitize 후 0자)
710: CHAT_PROMPT_LEAK_DETECTED (답변에 프롬프트 유출 감지)
711: RECIPE_ACCESS_DENIED (비공개 레시피 접근)
```

---

## 7. 구현 순서 재조정

기존 12단계에 삽입:

```
1단계: 의존성 (Spring AI + spring-retry + bucket4j + actuator)
2단계: DB 마이그레이션 (chat_config, chat_log, chat_daily_usage)
      + suspicious 컬럼 추가
3단계: 엔티티 + Repository
4단계: 프롬프트 파일 배치
5단계: AiClientConfig (+ RestClient 타임아웃)
6단계: RetryConfig (@EnableRetry)
7단계: PromptLoader
8단계: ChatConfigService (@Cacheable)
9단계: InputSanitizer, SuspiciousDetector, AnswerValidator (유틸)
10단계: IntentClassifier (+@Retryable), AnswerGenerator (+@Retryable),
       RepetitionGuard
11단계: ChatLogService (@Async), DailyQuotaService 확장,
       RateLimitService (Bucket4j)
12단계: ChatService (전체 조합 + 모든 체크)
13단계: ChatController (@AuthenticationPrincipal + sanitize)
14단계: UpstageHealthIndicator + /api/health/ai
15단계: 테스트 (RepetitionGuard 단위 + Controller 통합 + Upstage Mock)
```

---

## 8. ChatService 최종 플로우 (정리)

```java
public ChatResponse chat(Long userId, ChatRequest request) {
    long start = System.currentTimeMillis();
    ChatLog.ChatLogBuilder logBuilder = ChatLog.builder()
        .userId(userId)
        .recipeId(request.recipeId());
    
    try {
        // 1. 킬스위치
        if (!chatConfig.getBoolValue("chat_enabled")) {
            throw new CustomException(ErrorCode.CHAT_DISABLED);
        }
        
        // 2. Rate limit (burst)
        rateLimitService.checkUserRate(userId);
        
        // 3. 일일 쿼터
        int dailyQuota = chatConfig.getIntValue("daily_quota_per_user");
        dailyQuotaService.checkAndIncrement(userId, "chat", dailyQuota);
        
        // 4. 입력 sanitize
        String cleanQuestion = InputSanitizer.sanitize(request.question());
        if (cleanQuestion.isBlank()) {
            throw new CustomException(ErrorCode.CHAT_INVALID_QUESTION);
        }
        
        // 5. 의심스러운 패턴 감지 (로그용)
        SuspiciousResult suspicious = suspiciousDetector.detect(cleanQuestion);
        logBuilder.suspicious(suspicious.isSuspicious());
        logBuilder.suspiciousReason(suspicious.reason());
        
        // 6. Recipe 로드 + String 변환 (트랜잭션 안)
        String recipeText = recipeService.loadAsPromptString(request.recipeId(), userId);
        
        // 7. Mini 분류 (트랜잭션 밖, Retryable)
        ClassificationResult miniResult = classifier.classify(cleanQuestion);
        
        // 8. 분기 처리
        Intent intent = miniResult.intent();
        String answer;
        GenerationResult proResult = null;
        boolean repetitionDetected = false;
        
        if (intent == Intent.OUT_OF_SCOPE) {
            answer = prompts.get("reject");
        } else if (intent == Intent.UNCLEAR) {
            answer = prompts.get("unclear");
        } else {
            // IN_SCOPE
            proResult = generator.generate(cleanQuestion, recipeText);
            
            // 후처리: 무한반복 감지
            String guarded = repetitionGuard.guard(proResult.answer());
            repetitionDetected = !guarded.equals(proResult.answer());
            
            // 후처리: 프롬프트 유출 감지
            ValidationResult validation = answerValidator.validate(guarded);
            if (!validation.isValid()) {
                log.warn("Prompt leak detected: {}", validation.reason());
                answer = prompts.get("reject");  // 유출되면 거부 메시지로 대체
            } else {
                answer = guarded;
            }
        }
        
        // 9. 로그 저장 (비동기)
        long totalLatency = System.currentTimeMillis() - start;
        chatLogService.saveAsync(
            logBuilder
                .question(cleanQuestion)
                .intent(intent.name())
                .answer(answer)
                .proCalled(proResult != null)
                .totalLatencyMs((int) totalLatency)
                .miniInputTokens(miniResult.inputTokens())
                .miniOutputTokens(miniResult.outputTokens())
                .proInputTokens(proResult != null ? proResult.inputTokens() : 0)
                .proCachedTokens(proResult != null ? proResult.cachedTokens() : 0)
                .proOutputTokens(proResult != null ? proResult.outputTokens() : 0)
                .estimatedCostKrw(calculateCost(miniResult, proResult))
                .repetitionDetected(repetitionDetected)
                .classifierVersion("v2")
                .chatVersion("v6")
                .build()
        );
        
        return new ChatResponse(answer, intent, proResult != null);
        
    } catch (CustomException e) {
        // 예외도 로그 저장
        chatLogService.saveAsync(
            logBuilder
                .question(request.question())
                .errorMessage(e.getMessage())
                .totalLatencyMs((int)(System.currentTimeMillis() - start))
                .build()
        );
        throw e;
    }
}
```

---

## 9. 최종 체크리스트

### 안정성
- [ ] Upstage connect/read timeout 설정
- [ ] Spring Retry로 3회 재시도
- [ ] @Recover에서 fallback 동작

### 보안
- [ ] 질문 입력 sanitize
- [ ] Recipe 접근 권한 체크 (정책 결정 후)
- [ ] 의심 패턴 감지 + chat_log에 플래그
- [ ] 답변 프롬프트 유출 감지

### Rate Limit
- [ ] Bucket4j 유저당 분당 10건
- [ ] 일일 쿼터 20건 (chat_config)
- [ ] 429 응답

### 모니터링
- [ ] Actuator health 활성화
- [ ] Upstage Health Indicator
- [ ] /api/health/ai 관리자용

### 테스트
- [ ] RepetitionGuard 단위 테스트 4개+
- [ ] WireMock으로 Upstage Mock
- [ ] ChatController 통합 테스트 (@WithMockUser)
- [ ] 쿼터 초과 / Rate limit 초과 테스트

### 운영
- [ ] chat_config 초기 데이터 migration
- [ ] ErrorCode 703~711 추가
- [ ] 관리자가 DB 직접 UPDATE로 설정 변경 가능
