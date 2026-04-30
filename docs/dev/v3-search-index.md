# Dev V3 검색 인덱스 운영 가이드 (Phase A1)

> Phase A1 산출물. RESTRICTED 활성화 + 4-enum 기반 검색의 blocker였던 OpenSearch 인덱스 분리 작업의 운영 절차.

## 개요

운영 V1/V2 검색은 `recipes` 인덱스(`isPrivate` 기준 필터)를 그대로 사용한다. dev V3는 별도 alias `recipes_v3_dev`를 두고 `visibility` / `listingStatus` / `lifecycleStatus` / `source` / `userId` 5개 신규 필드를 추가한 dev document를 mirror write한다.

**핵심 invariant:**
- `search.dev-index.enabled = false` (default)면 dev mirror는 완전히 no-op — 운영 검색에 영향 없음
- dev mirror 실패는 **절대** 운영 indexing path에 throw되지 않음 (swallow + log + metric)
- dev concrete index와 alias는 코드가 자동 생성하지 않음 — 이 문서의 절차로 명시 생성

---

## 명명 규약

| 종류 | 이름 | 비고 |
|---|---|---|
| Alias | `recipes_v3_dev` | `application.yml`의 `search.dev-index.alias` 기본값 |
| Concrete index | `recipes_v3_dev_{YYYYMMDD}_{seq}` | 예: `recipes_v3_dev_20260426_001` |

concrete index에 버전 suffix를 붙이는 이유는 향후 매핑 변경 시 새 인덱스를 만들고 alias만 swap하면 zero-downtime 전환이 가능하기 때문이다.

---

## 1. 인덱스 생성 절차

`DevRecipeIndexingService`는 **index 생성과 alias 연결을 분리**한다. swap 시 (새 concrete index를 만들 때마다) alias가 양쪽에 모두 붙어 multi-index alias가 되는 것을 막기 위함이다 — multi-index alias가 되면 dev 검색 결과가 중복으로 보이고 mirror write의 target index가 모호해진다.

### 최초 bootstrap (alias가 아직 어디에도 안 붙어 있을 때)

```java
@Autowired DevRecipeIndexingService devRecipeIndexingService;

// 1. concrete index 생성 (alias는 안 붙음)
boolean created = devRecipeIndexingService.createDevRecipeIndex("recipes_v3_dev_20260426_001");

// 2. alias attach — alias가 이미 존재하면 거부 (warn log + false 반환).
//    매핑 변경으로 옮길 때는 attachDevAlias 대신 swapDevAlias 사용.
boolean attached = devRecipeIndexingService.attachDevAlias("recipes_v3_dev_20260426_001");
```

이 메서드는:
1. `createDevRecipeIndex`: settings + mapping(운영 base + 5 신규 필드)으로 concrete index 생성
2. `attachDevAlias`: alias `recipes_v3_dev` 가 어떤 index에도 안 붙어 있을 때만 attach

### 운영 데이터 복사 — `_reindex` API의 한계

> ⚠️ **`_reindex`만으로는 A2 검색 검증이 불충분합니다.** 운영 `recipes` 인덱스에는 dev V3 신규 5 필드(`visibility`, `listingStatus`, `lifecycleStatus`, `source`, `userId`)가 없어서, 복사된 문서의 해당 필드는 모두 null입니다. A2의 `publicListedActiveFilter()`는 `ACTIVE && PUBLIC && LISTED`를 filter로 강제하므로, **복사된 거의 모든 기존 문서가 검색에서 빠집니다**.
>
> 실제 검증을 의미 있게 하려면:
> - **운영 indexing이 자연 발생하기를 기다려서** dev mirror가 신규 필드를 채우거나
> - **DB Recipe entity 기반 backfill batch** (별도 phase 작업)로 모든 기존 recipe에 대해 `mirrorReindex`를 강제 실행
>
> `_reindex`는 매핑 호환되는 인덱스 마이그레이션 (구 dev concrete index → 새 dev concrete index) 용도로만 적합합니다.

```bash
# 인덱스 구조 마이그레이션 시 (dev → dev) 사용 예
POST _reindex
{
  "source": { "index": "recipes_v3_dev_20260426_001" },
  "dest":   { "index": "recipes_v3_dev_20260501_002" }
}
```

---

## 2. enable / disable

### enable (dev 검증 환경)

`application-local.yml` 또는 환경별 override:

```yaml
search:
  dev-index:
    enabled: true
    alias: recipes_v3_dev   # default
```

enable 직후 동작:
- 운영 `RecipeIndexingService.indexRecipeSafelyWithRetry` 성공 후 → dev alias에도 mirror write
- `deleteRecipeSafelyWithRetry` 성공 후 → dev alias에서 mirror delete
- `updatePrivacyStatusSafely` 성공 후 → dev alias에 full reindex (운영의 isPrivate-only partial update와 달리 visibility/listingStatus/lifecycleStatus 모두 반영해야 함)

### disable (default)

```yaml
search:
  dev-index:
    enabled: false
```

모든 mirror 호출이 즉시 no-op. 운영 path에는 어떤 영향도 없음.

---

## 3. 모니터링

### 메트릭 (Micrometer)

```
dev_index_mirror_total{operation="index|delete|reindex", result="success|failure|disabled"}
```

총 9 조합 (3 × 3) — 카디널리티 안전.

대시보드 권장 패널:
- `failure` rate가 0이 아니면 dev OpenSearch 클러스터 상태 또는 매핑 충돌 점검
- `disabled` count가 운영 indexing 호출 수와 비슷하면 flag가 의도대로 꺼져 있다는 헬스 시그널

### 로그

`com.jdc.recipe_service.dev.opensearch.service.DevRecipeIndexingService` 패키지에 `[DevIdx]` 접두 로그.

- 성공: DEBUG (운영 로그 노이즈 방지)
- 실패: WARN (`swallowed` 키워드 포함 — 운영 path에 영향 없음을 명시)

---

## 4. 매핑 정의

`DevRecipeIndexingService.indexMappingJson()` 참고. 운영 mapping과 차이는 다음 5 필드 추가:

```json
"visibility":      { "type": "keyword" },
"listingStatus":   { "type": "keyword" },
"lifecycleStatus": { "type": "keyword" },
"source":          { "type": "keyword" },
"userId":          { "type": "long" }
```

운영 mapping의 모든 분석기/필터(`korean_analyzer`, `autocomplete_analyzer`, `infix_analyzer` + 동의어)는 그대로 복제 — A2 검색 API가 운영과 동등한 검색 품질을 가질 수 있게.

---

## 5. 매핑 변경 시 swap 절차 (향후)

매핑이 호환 안 되는 변경(예: 필드 타입 변경)이 필요하면:

```java
// 1. 새 concrete index 생성 (alias는 안 붙음 — bootstrap과 swap 절차 모두 동일 동작)
devRecipeIndexingService.createDevRecipeIndex("recipes_v3_dev_20260501_002");

// 2. 기존 데이터 복사 (raw OpenSearch _reindex)
//    POST _reindex
//    {
//      "source": { "index": "recipes_v3_dev_20260426_001" },
//      "dest":   { "index": "recipes_v3_dev_20260501_002" }
//    }

// 3. atomic alias swap — 한 번의 _aliases 호출로 처리해서 alias가 끊기는 순간 없음
boolean ok = devRecipeIndexingService.swapDevAlias(
        "recipes_v3_dev_20260426_001",
        "recipes_v3_dev_20260501_002");

// 4. 검증 후 옛 인덱스 삭제 (raw)
//    DELETE recipes_v3_dev_20260426_001
```

raw OpenSearch로 직접 swap하려면:

```bash
POST _aliases
{
  "actions": [
    { "remove": { "index": "recipes_v3_dev_20260426_001", "alias": "recipes_v3_dev" }},
    { "add":    { "index": "recipes_v3_dev_20260501_002", "alias": "recipes_v3_dev" }}
  ]
}
```

> **주의**: `createDevRecipeIndex`가 alias를 자동 연결하지 않으므로, swap 절차에서도 step 1과 step 3 사이에 새 index가 alias 없이 떠 있는 시간이 잠시 있다. 이 시간 동안 dev mirror write는 옛 index에 계속 들어가고, swap 후부터 새 index로 들어간다. step 2의 `_reindex`가 끝나기 전에 swap하지 않도록 순서 엄수.
>
> **⚠️ reindex 중 mirror write 유실 가능성**: step 2 `_reindex` 시작 시점부터 step 3 swap 직전까지 운영 indexing이 발생하면, 그 변경은 alias가 가리키는 옛 index에는 들어가지만 새 index에는 누락된다 (`_reindex`가 그 시점의 옛 index 스냅샷을 복사하기 때문).
>
> dev 검증 환경에서는 보통 감수 가능 (운영 traffic 영향 0이고, 다음 운영 indexing이 새 index에 직접 mirror write로 다시 채움). 그러나 정확한 cutover가 필요한 시나리오에서는:
> - **write pause**: swap 직전에 운영 indexing을 잠시 멈추거나 (일반적으로 실용적이지 않음)
> - **catch-up reindex**: swap 후 timestamp/version 기반 delta `_reindex`를 한 번 더 실행하거나
> - **dual-write to new index**: `RecipeIndexingService`의 mirror hook을 swap 직전부터 옛/새 두 index에 모두 write하도록 임시 수정 (가장 안전하지만 코드 변경 필요)
>
> 매핑 변경이 production-critical하면 위 셋 중 하나를 채택. dev에서는 첫 옵션도 보통 무시 가능.

---

## 6. 후속 phase에서 다룰 것

- **Phase A2/A3**: dev alias를 사용하는 `/api/dev/recipes/search|popular|budget` API
- **Backfill batch**: 운영 데이터를 dev alias에 일괄 복사하는 별도 job (현재는 운영 indexing이 발생할 때 자연 backfill)
- **Admin endpoint**: `createDevRecipeIndex()`를 호출하는 보호된 endpoint (현재는 코드 호출만)
- **운영 alias도 properties 이관**: 현재 운영 `"recipes"` 이름이 코드에 하드코딩 — `search.prod-index.alias` 같은 형태로 일관성 정리

---

## 관련 파일

- `src/main/java/com/jdc/recipe_service/util/SearchProperties.java` — `DevIndex` nested config
- `src/main/java/com/jdc/recipe_service/dev/opensearch/dto/DevRecipeDocument.java` — dev document 모델
- `src/main/java/com/jdc/recipe_service/dev/opensearch/service/DevRecipeIndexingService.java` — mirror service + createDevRecipeIndex
- `src/main/java/com/jdc/recipe_service/dev/opensearch/service/DevRecipeSearchFilters.java` — A2에서 사용할 BoolQuery filter
- `src/main/java/com/jdc/recipe_service/opensearch/service/RecipeIndexingService.java` — 운영 + 3개 메서드에 mirror hook 추가
- `src/main/resources/application.yml` — `search.dev-index` 기본값
