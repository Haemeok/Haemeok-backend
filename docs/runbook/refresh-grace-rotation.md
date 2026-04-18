# Refresh Grace Rotation — 배포 Runbook

이 문서는 Android WebView CookieManager flush 누락을 메우기 위한 DB grace 기반 refresh 회전
변경의 **배포 순서, precheck, 롤백, 모니터링**을 정리한다. 관련 변경 범위:

- `V20260418_001__add_unique_index_refresh_tokens_token.sql`
- `V20260418_002__add_grace_columns_refresh_tokens.sql`
- `AuthService#refresh` + `RefreshTokenRepository#findByTokenForUpdate` / `findByPreviousTokenInGraceForUpdate`
- `GlobalExceptionHandler#handleConcurrencyFailure` (refresh 경로 한정)
- 메트릭: `auth_refresh_total{result}`

---

## 1. 배포 전 precheck (필수)

### 1-1. refresh_tokens.token 중복 검사

V20260418_001이 UNIQUE 제약을 추가하므로, 이미 중복된 token 값이 존재하면 Flyway가 실패한다.
운영 MySQL에서 아래 쿼리를 반드시 먼저 실행한다.

```sql
SELECT token, COUNT(*) AS c
FROM refresh_tokens
GROUP BY token
HAVING c > 1;
```

**기대 결과: 0 rows.**

### 1-2. 중복이 발견되었을 때

마이그레이션을 건드리지 않고, 운영 SQL로 정리한 뒤 다시 1-1을 0 rows로 만든다. 각 중복 그룹에서
`expired_at`이 가장 크고(동률이면 id가 가장 큰) row 1개만 남기고 나머지는 삭제한다. `MAX(expired_at)`
기준 JOIN은 같은 만료시각을 공유하는 행이 여러 개일 때 여러 행을 남겨 UNIQUE 추가를 다시 막는다.
MySQL 8의 ROW_NUMBER()로 그룹당 정확히 1행만 남기게 한다.

```sql
-- 1) 유지할 row들의 id 집합 (그룹별로 expired_at DESC, id DESC 기준 정확히 1행)
CREATE TEMPORARY TABLE rt_keep AS
SELECT id
FROM (
    SELECT id,
           token,
           ROW_NUMBER() OVER (
               PARTITION BY token
               ORDER BY expired_at DESC, id DESC
           ) AS rn
    FROM refresh_tokens
) ranked
WHERE ranked.rn = 1;

-- 2) 중복 그룹(>= 2행)에서 keep 집합 외 row 삭제
DELETE rt
FROM refresh_tokens rt
JOIN (
    SELECT token
    FROM refresh_tokens
    GROUP BY token
    HAVING COUNT(*) > 1
) g ON g.token = rt.token
WHERE rt.id NOT IN (SELECT id FROM rt_keep);

-- 3) 정리
DROP TEMPORARY TABLE rt_keep;

-- 4) 재확인: 0 rows여야 한다
SELECT token, COUNT(*) AS c
FROM refresh_tokens
GROUP BY token
HAVING c > 1;
```

> 정리 과정에서 일부 세션이 로그아웃될 수 있다. 이 side effect를 **스키마 변경 히스토리에
> 묻지 말고** 별도 운영 작업으로 기록한다.

### 1-3. long-running tx 확인

ALTER TABLE은 metadata lock을 잡는다. 트래픽이 낮은 시간대에 적용하고, 적용 직전 실행 중인
장시간 트랜잭션이 없는지 확인한다.

```sql
SELECT * FROM information_schema.innodb_trx ORDER BY trx_started ASC LIMIT 5;
SHOW PROCESSLIST;
```

---

## 2. 배포 순서

1. **DB**: Flyway가 V20260418_001 → V20260418_002 순으로 적용한다. (app 시작 전 또는 CI에서)
   - 001: UNIQUE 인덱스 추가. precheck가 통과했으면 실패하지 않는다.
   - 002: previous_token, previous_token_grace_until 컬럼 + idx_rt_prev_token 추가. nullable이므로
     기존 row 영향 없음.
2. **App**: 새 AuthService/Controller/Handler가 포함된 JAR 배포.
   - 구 버전 app + 새 스키마 조합도 호환된다(기존 쿼리는 새 컬럼을 쓰지 않음).
   - 새 app은 새 컬럼을 읽고 쓰지만, row에 NULL이 들어 있어도 2단계 lookup 모두 cleanly fallthrough.
3. **Monitor**: 아래 §4 대시보드 주시.

---

## 3. 롤백 계획

### 3-1. 앱 롤백

새 버전 app에 문제가 있으면 **구 버전 app으로 즉시 롤백 가능**하다. 새 컬럼은 구 app이
무시하므로 호환성이 유지된다. 스키마를 되돌릴 필요 없다.

### 3-2. 스키마 롤백 (극단적인 경우에만)

`previous_token` / `previous_token_grace_until` 컬럼은 additive라 drop해도 데이터 손실은 없다.
UNIQUE 인덱스(uk_rt_token)는 일단 걸리고 나면 중복 insert가 실패하게 되므로, 구 app에서
"동일 token 덮어쓰기" 경로가 있다면 영향을 받을 수 있다. 단 현재 코드에는 그런 경로가 없다.

```sql
-- 필요 시 (역순으로)
ALTER TABLE refresh_tokens
    DROP INDEX idx_rt_prev_token,
    DROP COLUMN previous_token_grace_until,
    DROP COLUMN previous_token;

ALTER TABLE refresh_tokens
    DROP INDEX uk_rt_token;
```

Flyway 히스토리와 어긋나므로 스키마 롤백 시 `flyway_schema_history`에서 해당 row를 함께 정리한다.

---

## 4. 모니터링 (배포 후 24시간 집중)

### 4-1. 주요 메트릭

`auth_refresh_total{result=...}` counter에 다음 result 태그가 올라온다.

| result | 의미 | 기대 비율 |
|---|---|---|
| `rotated` | 정상 회전 (현재 토큰 매치) | 대부분 (> 95%) |
| `grace_replay` | grace 윈도우 재전송 (주로 Android) | 소수 (< 5%, 플랫폼 비중 따름) |
| `invalid` | JWT 유효하지만 DB 어디에도 없음 | 아주 드묾 |
| `expired` | DB row expired_at 초과 | 드묾 |
| `jwt_invalid` | JWT 서명/형식 오류 | 매우 드묾 |
| `jwt_expired` | JWT 만료 | 간헐적 |
| `lock_timeout` | innodb_lock_wait_timeout 만료 | 0에 가까워야 함 |

### 4-2. 경보 기준 (후속 제안)

- `rate(auth_refresh_total{result="lock_timeout"}[5m]) > 0.1 /s` → 지속되면 문의
- `rate(auth_refresh_total{result="invalid"}[5m])`가 갑자기 상승 → 쿠키 유실 폭증 가능
- `rate(auth_refresh_total{result="grace_replay"}[1h])` 일평균 대비 2배 급등 → 앱 재시도 로직
  또는 배포 이슈 의심

### 4-3. Android 증상 회복 확인

변경의 원래 목표는 **Android 간헐 force-logout 감소**다. 아래 지표를 함께 본다.

- 프론트 `REFRESH_TOKEN_EXPIRED` force-logout 이벤트 (프론트 텔레메트리)
- User-Agent 기준 Android 비중이 높은 실패가 줄었는지
- 같은 userId가 짧은 시간 내 재로그인하는 패턴 감소

---

## 5. 관련 테스트

- `AuthServiceTest` — 단위 테스트 (Mockito, SimpleMeterRegistry). rotated / grace_replay /
  jwt_invalid / jwt_expired / expired / invalid / replay_suspected 각 카운터 경로 검증
- `GlobalExceptionHandlerTest` — 단위 테스트 (Mockito). refresh 경로 한정으로
  `ConcurrencyFailureException`을 INVALID_REFRESH_TOKEN으로 매핑하고 `lock_timeout` 카운터를 올리는지,
  비 refresh 경로에서는 원 예외가 rethrow되는지 검증
- `RefreshTokenRepositoryTest` — @DataJpaTest + H2. 쿼리 shape 고정 (MySQL 고유 기능을 쓰지 않으므로
  testing rule 기본값인 H2로 빠르게 돈다). `existsByPreviousToken`, `findAllByTokenOrPreviousToken`도 포함
- `RefreshTokenConcurrencyIT` — @SpringBootTest + Testcontainers(MySQL), `@Tag("concurrency")`.
  InnoDB row-lock + UNIQUE 동작에 의존하는 시나리오라 Testcontainers가 필요하다. 기본 CI에서는
  태그로 제외하고 별도 잡에서 돌리는 것을 권장. 동시 refresh 2개에 대해 ROTATED 1 + GRACE_REPLAY 1
  불변성 검증
