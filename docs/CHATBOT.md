# 레시피오 AI 챗봇

레시피별 질문 답변 시스템. Solar Pro 3 + Solar Mini.

## 개요

- **모델**: Spring AI 1.0 GA + Upstage Solar Pro 3 (답변) + Solar Mini (분류)
- **비용**: 호출당 ~₩0.246, 월 9만 호출 ~₩22K (검증된 수치)
- **보안**: 4-layer defense-in-depth
- **테스트**: 167+ 테스트, chat 영역 line 95% / branch 79% 커버리지

## 4-Layer 보안 모델

```
사용자 질문
    ↓
[1] InputSanitizer
    HTML 태그 제거, 길이 제한 (max_question_length)
    ↓
[2] SuspiciousDetector
    정규식 14 패턴, 200건 csv 적중률 93.3%
    감지 시 chat_log.suspicious=true + reason 라벨 기록 (분류 흐름은 진행)
    ↓
[3] Mini Classifier (Solar Mini)
    IN_SCOPE → Pro 호출
    UNCLEAR → Pro 호출 (맥락 기반 답변 또는 짧은 되묻기)
    OUT_OF_SCOPE → reject 정형 응답
    ↓
[4] Pro 답변 (Solar Pro 3)
    chat-v6 system prompt로 누설/위험 답변 차단
    ↓
[5] RepetitionGuard
    무한 반복 답변 truncate, repetitionDetected=true 기록
    ↓
[6] AnswerValidator
    누설 마커 검사 (# 4가지 원칙, 시스템 프롬프트, instruction 등)
    감지 시 reject fallback (fromLlm=false)
    ↓
사용자 응답
```

한 layer 통과해도 다른 layer가 catch. 200건 csv 인젝션 카테고리에서 미감지 1건도 Pro 답변에 누설 마커 0건으로 검증됨.

## API

| Method | Path | 설명 |
|---|---|---|
| `POST` | `/api/recipes/{recipeId}/chat` | 챗봇 질문 (auth 필수) |
| `GET` | `/api/recipes/{recipeId}/chat/history?limit=` | 대화 기록 (DESC, max 50) — **선택**, 별도 "예전 질문 보기" UI용. Pro 컨텍스트 합치기는 sessionId로 자동 |
| `GET` | `/api/chat/quota` | 일일 쿼터 (user-scoped) |

상세 명세는 OpenAPI(Swagger UI: `/swagger-ui.html`) 참조.
프론트 협업 가이드 (호출 예시 / 에러 코드 / UX 권장): [docs/api/chatbot.html](api/chatbot.html)

## 핵심 흐름

요청:
```json
POST /api/recipes/x9Lb3a7Q/chat        // recipeId는 path variable (hashids, raw long 둘 다 OK)
{
  "question": "이거 매워요?",
  "sessionId": "550e8400-e29b-41d4-a716-446655440000"  // optional. 같은 sessionId만 history 합침
}
```

**`sessionId` 라이프사이클**:
- frontend 챗봇 컴포넌트 mount 시 `crypto.randomUUID()` 생성 → React state 보관
- 같은 컴포넌트 살아있는 동안 (백그라운드 다녀와도) 같은 sessionId → 컨텍스트 유지
- 컴포넌트 unmount (페이지 떠남) → state 사라짐 → 다음 mount 시 새 sessionId → 새 대화
- `sessionId` 미전송 시 stateless (history 합치지 않음, 그 호출만 1회성)

응답:
```json
{
  "intent": "IN_SCOPE",
  "answer": "보통 매운맛이에요...",
  "fromLlm": true
}
```

대화 컨텍스트는 **server-side reload** (클라이언트는 question + sessionId만 전송). 이유: 프롬프트 인젝션 공격면 축소.

같은 `sessionId` + 같은 `(user, recipe)` + 6시간 안전망 안의 정상 답변(`error_message IS NULL`) 최근 5턴이 자동 포함됨 (DESC → ASC 변환). `sessionId` 미전송 시 stateless.

## Dev Setup

### 환경변수 (.env)

```
UPSTAGE_API_KEY=...
APP_HASHIDS_SALT=...
```

### 시드 데이터

- `chat_config` 4 row (`V20260425_001_chat_chatbot_tables.sql`로 자동 마이그레이션)
  - `daily_quota_per_user=20`, `rate_limit_per_minute=10`
  - `chat_enabled=true`, `max_question_length=500`
  - `chat_enabled`는 긴급 킬스위치라 캐시를 우회해 DB 변경을 즉시 반영
- 테스트 유저 (`provider='test'`, `oauth_id='apple_reviewer'`) — `POST /api/token/test-login` 용

### 실행

```bash
redis-server --daemonize yes  # rate limit / DAU
./gradlew bootRun             # 또는 IntelliJ ▶️ Run
```

## 알려진 함정 (다음 dev 위해)

### 1. H2 ddl-auto DEFAULT 갭 (영구 fix됨)

증상: `chat_log.created_at`이 NULL로 저장 → history 정렬·since 필터 깨짐.

원인: ddl-auto가 `@Generated(EventType.INSERT)` + `insertable=false` 조합에서 column DEFAULT를 emit하지 않음.

해결: `ChatLog.java`에 명시적 컬럼 정의.

```java
@Column(columnDefinition = "DATETIME(6) DEFAULT CURRENT_TIMESTAMP(6)")
```

영향: 운영 Flyway는 처음부터 DEFAULT 명시 → 정상. dev H2에서만 발생하던 갭.

### 2. H2 Profile MODE=MySQL

H2 데이터소스 URL `MODE=MySQL` + `dialect=MySQLDialect`로 `INSERT ... ON DUPLICATE KEY UPDATE` 같은 MySQL 전용 문법을 dev에서 검증. `ChatDailyUsageRepository.incrementUsage`가 이 패턴.

### 3. AOP self-invocation 회피

`ChatService.chat()`이 같은 클래스 내부 `@Transactional` 메서드를 직접 호출하면 Spring 프록시 우회. 그래서 레시피 로딩은 **별도 component** `ChatRecipeLoader(@Component, @Transactional(readOnly=true))`로 분리. 같은 함정 반복하지 말 것.

## 모니터링

### 일별 비용

```sql
SELECT DATE(created_at), COUNT(*), SUM(estimated_cost_krw)
FROM chat_log GROUP BY DATE(created_at);
```

### 캐시 hit ratio (비용 영향)

```sql
SELECT AVG(pro_cached_tokens * 1.0 / NULLIF(pro_input_tokens, 0))
FROM chat_log WHERE pro_input_tokens > 0;
```

### Suspicious 패턴 추적 (Phase 3 보강 우선순위)

```sql
SELECT suspicious_reason, COUNT(*)
FROM chat_log WHERE suspicious=true
GROUP BY suspicious_reason ORDER BY 2 DESC;
```

### Admin bypass

`ROLE_ADMIN` 사용자는 `rate_limit_per_minute` + `daily_quota_per_user` 자동 skip.
운영 디버깅 / 검증용. `chat_log`는 동일하게 기록 (감사 추적), `chat_enabled` 킬스위치는 admin도 적용.

### 알림 임계

| 지표 | 임계 |
|---|---|
| 시간당 비용 | > ₩2,000 (정상 ~₩900~1100) |
| 분당 suspicious | > 10건 (인젝션 폭증) |
| 5분당 5xx | > 5% |
| Pro p99 latency | > 5초 |

### 무한반복 모니터링 (multi-turn 위험 추적)

200건 csv (single-turn) 분석에서 무한반복 0건. multi-turn 시뮬레이션(A2)에서 1건 발견 — `(\S){2,10}\s*대신\s*\1` 자기참조 패턴이 RepetitionGuard에 추가됨 (P1, 2026-04-26).

| 지표 | 임계 | 의미 | 대응 |
|---|---|---|---|
| 일별 `repetition_detected=true` 비율 | > 1% | Pro stuck loop 비율 | 패턴 분석 + RepetitionGuard 강화 |
| 답변 길이 > 1300자 빈도 | > 5%/day | 비정상 긴 답변 (반복 의심) | 샘플링 검토 |
| 같은 user의 quota 소진 비율 | > 50% | abuse 또는 만족도 ↓ | UX/rate limit 조정 |

### Multi-turn 회귀 데이터 수집 (Phase 3 input)

운영 1~2주 후 stuck loop 발생 답변 수집:

```sql
-- 자기 참조 또는 길이 이상치
SELECT id, user_id, recipe_id, question, answer,
       repetition_detected, answer_truncated, LENGTH(answer) AS ans_len, created_at
FROM chat_log
WHERE repetition_detected = true
   OR LENGTH(answer) > 1300
   OR answer REGEXP '([가-힣A-Za-z]{2,10}) 대신 \\1'
ORDER BY created_at DESC
LIMIT 100;
```

이 데이터로 Phase 3에서:
- multi-turn 회귀 set 구축 (1차 답변 + follow-up 질문)
- RepetitionGuard `MIN_CHUNK_LEN` 조정 (현재 10, 메모리 task #19)
- self-correction loop 휴리스틱 (`(다시)`, `(죄송합니다)` 카운터)
- chat-v7 프롬프트 (반복 금지 + 막히면 짧게 마무리 룰)
- 답변 길이 hard limit (예: 1200자) 검토

## 테스트

```bash
./gradlew test --tests "*chat*"            # 챗봇만
./gradlew test --tests "*RegressionTest"   # 200건 회귀
./gradlew clean test jacocoTestReport      # 풀 + 커버리지
```

리포트: `build/reports/jacoco/test/html/index.html`

### 통계

| 종류 | 메서드 |
|---|---|
| 단위 (Sanitizer / Validator / Detector / Intent / RepetitionGuard) | 79+ |
| 200건 회귀 (Intent / Validator / Suspicious) | 9 |
| 통합 — Service 레이어 (Quota / RateLimit / Log / ChatService) | 30 |
| HTTP — WireMock + MockMvc (5 시나리오 + history + quota) | 7 |
| **총** | **167+** |

## Phase 2 / 3 강화 영역

### 보안 (운영 데이터 보고 결정)
- SuspiciousDetector 패턴 추가 (현재 14, 93.3%)
  - 남은 1건 미감지: "이제부터 X 전문가 역할" (no "너는") — Phase 3 후보
- AnswerValidator marker 보수성 검토 (자연 유출 false negative 운영 측정 필요)

### 운영 가시성
- chat_log analytics 대시보드
- cached_tokens 비율 일별 추적 (현재 6%~81% 변동)

### 코드 분리 (chat_config DB로 이동)
- 비용 단가 BigDecimal 상수 → DB 키
- `HISTORY_GAP_HOURS=6`, `HISTORY_TRAILING_TURNS=5` → DB 키

### 확장
- session_id 활용 (frontend "새 대화" UX)
- bucket4j-Redis 분산 rate limit (멀티 인스턴스 시)

## 변경 이력

### 2026-04-26 (v0.1.0 — MVP)
- 챗봇 시스템 구현 (1~14단계) + 14.6 server-side history reload
- 종합 테스트 167+ (15.x)
- B1+B2: SuspiciousDetector 적중률 40% → 93.3% (per-pattern reason 도입)

## 관련 자료

- 코드: `src/main/java/com/jdc/recipe_service/service/chat/`
- 프롬프트: `src/main/resources/prompts/`
- 마이그레이션: `src/main/resources/db/migration/V20260425_001_chat_chatbot_tables.sql`
- 테스트: `src/test/java/com/jdc/recipe_service/service/chat/`
- 200건 csv: `src/test/resources/test-data/test_results_pipeline_200.csv`
- 설계 노트: `docs/chatbot/` (production_hardening, db_and_logging, spring_ai_structure)
