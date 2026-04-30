# Dev V3 enum 적용 감사 (Phase 6 / Task #14)

> 산출물: 2026-04-26 Phase 6 audit 결과.
> 목적: dev V3 코드가 4개 enum (`RecipeLifecycleStatus`, `RecipeVisibility`, `RecipeListingStatus`, `RecipeSourceType`)을
> 생성/조회/토글 경로에서 어떻게 적용하는지 점검 + 갭 + 후속 작업 명시.

## 감사 매트릭스

### 1. RecipeSourceType (USER / AI / YOUTUBE / REELS)

| 경로 | 적용 |
|---|---|
| `DevAiRecipeFacade` | `recipeService.createRecipeAndGenerateUrls(..., RecipeSourceType.AI, ...)` ✅ |
| `DevYoutubeRecipeExtractionFacade` | `recipeService.createRecipeAndGenerateUrls(..., RecipeSourceType.YOUTUBE, null)` ✅ |
| `DevRecipeDetailService` | response DTO에 `source` 필드 노출 ✅ |
| `DevRecipeVisibilityService` | (변경 안 함) |

REELS는 dev V3 scope 외. 향후 reels extraction 추가 시 같은 패턴 따르면 됨.

### 2. RecipeLifecycleStatus (ACTIVE / HIDDEN / BANNED / DELETED)

| 경로 | 적용 |
|---|---|
| 생성 (dev AI/YouTube) | Recipe entity `@Builder.Default = ACTIVE` 사용 (명시 setting 없음) ✅ |
| `DevRecipeDetailService` | response DTO에 `lifecycleStatus` 노출 ✅. 다만 V2 prod와 동등하게 **non-ACTIVE도 detail 조회는 허용** (V2 parity 유지) |
| `DevRecipeVisibilityService` | **non-ACTIVE 시 RECIPE_NOT_FOUND throw 가드 추가** (V1 togglePrivacy에는 없는 dev V3 강화점) |

**갭/후속**: 검색/listing 쿼리(V2 prod)는 `lifecycleStatus != ACTIVE` 필터를 명시 적용 안 함 (deletedAt 같은 legacy field 의존). dev/prod 공통 기술 부채 — 이번 dev 범위 외.

### 3. RecipeListingStatus (LISTED / UNLISTED)

| 경로 | 적용 |
|---|---|
| 생성 | `applyVisibility(...)`로 자동 동기화 (PUBLIC→LISTED, PRIVATE/RESTRICTED→UNLISTED) ✅ |
| `DevRecipeDetailService` | response DTO에 `listingStatus` 노출 ✅ |
| `DevRecipeVisibilityService` | `applyVisibility()` 호출 시 자동 동기화 ✅ |

**갭/후속**: 검색/listing 쿼리는 `listingStatus`를 보지 않고 `isPrivate`만 봄. **RESTRICTED 활성화의 blocker** — dev visibility update에서 RESTRICTED 차단 중 (Phase 5-2에서 적용). 활성화하려면:
- `RecipeSearchServiceV2.searchRecipes`/`getPopularRecipesStaticV2` 등에 `listingStatus = LISTED` 필터 추가
- OpenSearch 인덱스 문서에 `listingStatus` 필드 + 검색 쿼리에 반영
- `DevRecipeVisibilityService`에서 RESTRICTED 차단 해제

### 4. RecipeVisibility (PUBLIC / PRIVATE / RESTRICTED)

| 경로 | 적용 |
|---|---|
| 생성 | dev YouTube/AI 모두 image 성공 시 `applyVisibility(PUBLIC)` ✅ |
| `DevRecipeDetailService` | response DTO에 `visibility` 노출 + `visibility=PRIVATE && !owner` → 403 가드 ✅ (V2의 isPrivate 가드와 별개로 추가 방어) |
| `DevRecipeVisibilityService` | `applyVisibility()` 헬퍼만 사용 — 단일 setter 직접 호출 금지 ✅ |

**갭/후속**: V2 prod (`RecipeSearchServiceV2.getRecipeDetail`)는 isPrivate만 검사. dev V3가 추가 visibility 가드를 깔아둔 게 안전판. swap 후에는 V2 가드를 제거하고 visibility로만 가도 됨.

## 확인된 invariants

dev V3 코드에서 보장되는 것:
- 모든 Recipe 생성은 `lifecycleStatus = ACTIVE` (entity default)
- 모든 visibility 변경은 `applyVisibility()` 헬퍼로만 일어남 → 트리플(visibility/listingStatus/isPrivate) 동기화 보장
- dev API는 응답에 4 enum 모두 노출 (frontend가 구분 가능)
- non-ACTIVE 레시피는 dev visibility update 차단 (admin 조치 우회 방지)

## 후속 작업 (별도 phase)

- [ ] **RESTRICTED 활성화** (Phase 5-2 차단 해제):
  - V2 검색 쿼리에 `listingStatus = LISTED` 필터 추가
  - OpenSearch 문서/쿼리에 `listingStatus` 반영
  - `DevRecipeVisibilityService`의 RESTRICTED 차단 코드 제거 + 테스트 업데이트
- [ ] **lifecycleStatus 검색 필터** (V1/V2/dev 공통 기술 부채):
  - 검색/listing API에 `lifecycleStatus = ACTIVE` 명시 필터
  - OpenSearch 인덱스 문서에 `lifecycleStatus` 반영
- [x] **자정 경계 quotaUsedOn** (2026-04-26 완료):
  - `RecipeGenerationJob.quotaUsedOn DATE` 컬럼 + Flyway `V20260426_006`
  - `DailyQuotaDao.tryConsume(..., usedOn)` + `refund(..., usedOn)` date-aware 오버로드 (H2/MySQL)
  - `DevYoutubeQuotaService.tryStartOrThrow` / `chargeGeminiUpgrade(usedOn)` 모두 date-aware 경로 사용
    (service-DAO 사이 자정 race 제거, BASIC + Gemini upgrade가 같은 row에 누적)
  - dev YouTube facade가 차감 날짜 저장 + Gemini upgrade/refund 모두 managed entity의 quotaUsedOn 기준
    (durable idempotency 패턴 일관 적용)
  - chargeGeminiUpgrade + managed save를 단일 tx로 묶어 atomicity 보장 (+3 누수 차단)

## 관련 파일

- `dev/service/recipe/DevRecipeVisibilityService.java` — 라이프사이클 가드 + applyVisibility 헬퍼 사용 + OpenSearch indexing
- `dev/service/recipe/DevRecipeDetailService.java` — visibility=PRIVATE 가드 + 4 enum DTO 노출
- `dev/facade/DevAiRecipeFacade.java` — RecipeSourceType.AI 사용
- `dev/facade/DevYoutubeRecipeExtractionFacade.java` — RecipeSourceType.YOUTUBE 사용
- `domain/entity/Recipe.java` — `applyVisibility()` 트리플 동기화 메서드
