# ADR-0004 Recipe 상태 3-컬럼 분리 (lifecycle × visibility × listing)

- Status: Proposed — Expand phase in progress (dual-write/dual-read 구간)
- Date: 2026-03-21 (초안) · 2026-04-19 (재검토)
- Owners: minseo0943

## Context

초기 `recipe`는 공개/비공개만을 boolean `is_private` 한 컬럼으로 표현했다.
서비스가 자라면서 사실상 세 가지 서로 다른 축이 이 한 컬럼에 눌려 들어왔다.

1. **Lifecycle** — 작성 중인 draft인지, 게시된 published인지, 삭제(soft-deleted)된 상태인지
2. **Visibility** — 소유자 외에 누가 접근 가능한지 (public / private / friends only 같은 접근 제어)
3. **Listing** — 피드·검색·추천에 노출되는지 (모더레이션·차단·숨김 처리)

한 boolean으로 이 세 축을 표현하다 보니:

- 신고로 노출만 차단하고 싶은데 소유자의 접근까지 막히거나
- 관리자 모더레이션(“공개는 유지, 피드에서만 내림”)이 표현 불가능해 우회 로직이 생기거나
- Remix 소스 검증이 "원본이 접근 가능한가"와 "원본이 현재 노출 중인가"를 구분하지 못함

2026-03 모더레이션·차단 요구가 본격화되면서 단일 boolean으로는 더 이상 버티지 못한다는 결론에 도달.

## Decision Drivers

- **표현력**: 모더레이션(listing OFF but visibility public)을 1급으로 표현 가능해야 함
- **접근 제어 vs 노출 제어 분리**: 소유자/친구의 "접근"과 "피드 노출"은 다른 정책
- **기존 데이터 무결성**: `is_private = true`인 레시피 수만 건을 잃지 않고 이관
- **Remix 소스 검증**: 원본의 세 차원을 독립 검사할 수 있어야 함
- **2인 팀 운영 부담**: 복잡한 상태 머신보단 enum 3개 × 단순 조합이 낫다
- **롤백 가능성**: 각 단계가 이전 단계로 되돌아갈 수 있어야 함 (expand-contract)

## Considered Options

### A. 단일 status enum으로 확장
하나의 enum에 `DRAFT / PUBLIC / PRIVATE / HIDDEN / DELETED` 등을 몰아넣는다.

- 장점: 컬럼 1개, 상태 전이가 한 곳에서 관리됨
- 단점: "공개인데 피드에서만 내림" 같은 **두 축의 조합**을 표현하려면 상태가 조합 폭발함
  (`PUBLIC_LISTED`, `PUBLIC_UNLISTED`, `PRIVATE_LISTED`, ...). 신규 정책 추가마다 enum 값이 기하급수적으로 늘어남

### B. 2-컬럼 (lifecycle + visibility)
Lifecycle과 Visibility만 분리하고 Listing은 Visibility에 녹인다.

- 장점: 표현력이 A보다 훨씬 높고 마이그레이션 복잡도는 낮음
- 단점: 모더레이션이 Visibility를 변형하는 구조라 "소유자 접근"과 "외부 노출"을 끝내 구분하지 못함.
  모더레이션이 걸리면 소유자도 자기 레시피를 못 보게 되는 이슈가 재현됨

### C. 3-컬럼 독립 enum (채택)
`RecipeLifecycleStatus × RecipeVisibility × RecipeListingStatus`를 각각 독립 컬럼으로 둔다.

- 장점
  - 각 축의 변경이 다른 축에 부수 효과를 주지 않음
  - 모더레이션(listing OFF) · 접근 제어(visibility) · 생명 주기(lifecycle)가 직교
  - Remix 소스 검증에서 세 차원을 독립 체크 가능
  - 나중에 네 번째 축(예: featured)을 도입해도 기존 enum을 건드릴 필요 없음
- 단점
  - 컬럼 3개, 인덱스 재설계 필요
  - 쿼리에서 세 컬럼을 모두 체크해야 함 (피드 조회, 검색, 추천 모두)
  - 정합성 없는 조합(`lifecycle=DELETED` 인데 `listing=LISTED`) 방지를 애플리케이션에서 보장해야 함

## Decision

**옵션 C (3-컬럼 독립 enum)** 을 채택한다.

### 스키마

```sql
ALTER TABLE recipe
  ADD COLUMN lifecycle ENUM('DRAFT','PUBLISHED','DELETED') NOT NULL DEFAULT 'PUBLISHED',
  ADD COLUMN visibility ENUM('PUBLIC','PRIVATE','UNLISTED') NOT NULL DEFAULT 'PUBLIC',
  ADD COLUMN listing ENUM('LISTED','UNLISTED','MODERATED') NOT NULL DEFAULT 'LISTED';

-- 피드 핫패스용 복합 인덱스
CREATE INDEX idx_recipe_feed
  ON recipe (lifecycle, visibility, listing, popularity_score DESC);
```

### 이관 전략 (Expand-Contract)

1. **Expand**: 위 3개 컬럼 nullable로 추가, 기본값 주입. `is_private` 유지.
2. **Dual-write**: 쓰기 경로가 `is_private`과 3-컬럼을 동시에 갱신.
3. **Backfill**: `is_private = true` → `visibility = PRIVATE`, `is_private = false` → `visibility = PUBLIC`.
   lifecycle·listing은 기본값 유지.
4. **Dual-read**: 읽기 경로가 3-컬럼만 보도록 전환. `is_private`은 무시하되 쓰기는 계속.
5. **Verify**: 24h 모니터링. `RecipeDashboardQueries`로 `is_private`과 `visibility` 불일치 0건 확인.
6. **Contract**: `is_private` 삭제 마이그레이션.

각 단계는 독립 배포 단위이고, 이전 단계로 롤백 가능.

### Remix 소스 검증

`RecipeService#validateRemixSource`에서 세 차원을 모두 체크:

```java
if (origin.getLifecycle() == DELETED) throw ...;           // 삭제된 원본 remix 금지
if (origin.getVisibility() == PRIVATE && !isOwner) throw ...;  // 비공개 원본 접근 차단
if (origin.getListing() == MODERATED) throw ...;           // 모더레이션 걸린 원본 remix 금지
```

단일 boolean에서는 "모더레이션 걸렸지만 소유자는 접근 가능"을 표현할 수 없어 이 분기 자체가 불가능했다.

## Consequences

### Positive
- 모더레이션·차단·피드 숨김을 접근 제어와 섞지 않고 1급으로 표현 가능
- 신규 정책(예: 피드 featured) 추가 시 컬럼 하나 더하거나 enum만 확장
- Remix 검증이 의미 있는 단위로 분해됨

### Negative
- 인덱스 재설계 필요 (`idx_recipe_feed`가 hot 쿼리 대부분을 흡수하도록 설계)
- 세 컬럼을 모두 잊지 않고 체크하는 규율이 팀 차원에서 필요
- Expand-Contract 동안 `is_private`과 3-컬럼이 공존하는 구간의 정합성 감시 필요

### Neutral / Known Risks
- 기본값 주입이 backfill 전 잘못 세팅되면 "공개였던 레시피가 전부 PUBLIC으로 찍힘" → backfill이 이를 덮어씀.
  순서가 중요하고 **Expand → Dual-write → Backfill** 순서를 엄격히 지킬 것.
- 애플리케이션이 세 컬럼 정합성(`DELETED + LISTED` 같은 조합 금지)을 보장해야 함.
  이는 `RecipeStateValidator`로 추출하고 쓰기 경로에서 호출.

## Status / Rollout

- **현재(2026-04-19)**: Expand 완료, dual-read 구간.
  프론트와 검색은 이미 3-컬럼 기반으로 전환. `is_private`은 쓰기만 유지 중.
- **다음 단계**: 24h verification 후 Contract(`is_private` 삭제) 마이그레이션.
- **ADR Status**: Contract 완료 후 `Accepted`로 전환 예정.

## Related
- ADR-0002 popularity_score 비정규화 — `idx_recipe_feed` 인덱스 설계와 결합
- `.claude/rules/db-safety.md` — Expand-Contract 규칙
- `docs/migrations/` — 개별 마이그레이션 파일
