# Dev V3 frontend API sweep

작성일: 2026-04-27

대상 프론트 저장소: `C:\workspace\projects\recipio\Capstone-frontend`

## 결론

프론트가 실제로 사용하는 레시피 구조 영향 API는 dev V3 mirror가 준비되어 있다.

다만 프론트 전환은 단순히 `/api`를 `/api/dev`로 바꾸면 안 된다. 인증, 사용자 프로필, 냉장고 재료 CRUD, 알림, 결제/크레딧, 설문처럼 운영 path를 그대로 써야 하는 API가 섞여 있고, 일부 레시피 API는 운영 path와 dev path shape가 다르다.

따라서 프론트는 레시피 구조 영향 API만 선택적으로 dev path로 매핑해야 한다.

## 반드시 dev path로 전환

| Frontend current path | Dev V3 path | 비고 |
|---|---|---|
| `GET /recipes/{id}` | `GET /dev/recipes/{id}` | detail gate + dev fields |
| `GET /v2/recipes/{id}` | `GET /dev/recipes/{id}` | static detail도 dev detail로 매핑 |
| `GET /recipes/search` | `GET /dev/recipes/search` | 검색/listing strict |
| `GET /v2/recipes/search` | `GET /dev/recipes/search` | static search |
| `GET /recipes/popular` | `GET /dev/recipes/popular` | popular strict |
| `GET /v2/recipes/popular` | `GET /dev/recipes/popular` | static popular |
| `GET /recipes/budget` | `GET /dev/recipes/budget` | budget strict |
| `GET /v2/recipes/budget` | `GET /dev/recipes/budget` | static budget |
| `GET /me/fridge/recipes` | `GET /dev/me/fridge/recipes` | fridge recommendation strict |
| `GET /ingredients/{id}` | `GET /dev/ingredients/{id}` | ingredient detail recipes strict |
| `GET /users/{userId}/recipes` | `GET /dev/users/{userId}/recipes` | user lists strict |
| `GET /me/recipes` | `GET /dev/me/recipes` | owner list strict |
| `GET /me/favorites` | `GET /dev/me/favorites` | favorites strict |
| `GET /me/recipe-books` | `GET /dev/me/recipe-books` | book counts strict |
| `GET /me/recipe-books/{bookId}` | `GET /dev/me/recipe-books/{bookId}` | book items strict |
| `GET /recipes/{id}/saved-books` | `GET /dev/recipes/{id}/saved-books` | self collection metadata |
| `GET /v2/recipes/{id}/status` | `GET /dev/recipes/{id}/status` | status probing 차단 |
| `POST /v2/recipes/status` | `POST /dev/recipes/status` | batch status silent filter |
| `POST /recipes/{id}/like` | `POST /dev/recipes/{id}/like` | interaction gate |
| `POST /recipes/{id}/favorite` | `POST /dev/recipes/{id}/favorite` | interaction gate |
| `POST /comments/{commentId}/like` | `POST /dev/comments/{commentId}/like` | comment recipe gate |
| `POST /ratings/recipe/{id}` | `POST /dev/ratings/recipe/{id}` | rating gate |
| `GET /ratings/recipe/{id}/me` | `GET /dev/ratings/recipe/{id}/me` | self read parity |
| `DELETE /ratings/recipe/{id}` | `DELETE /dev/ratings/recipe/{id}` | cleanup right |
| `GET /recipes/{id}/comments` | `GET /dev/recipes/{id}/comments` | comment list gate |
| `POST /recipes/{id}/comments` | `POST /dev/recipes/{id}/comments` | comment write gate |
| `GET /recipes/{id}/comments/{commentId}/replies` | `GET /dev/recipes/{id}/comments/{commentId}/replies` | reply list gate |
| `POST /recipes/{id}/comments/{parentId}/replies` | `POST /dev/recipes/{id}/comments/{parentId}/replies` | reply write gate |
| `DELETE /recipes/{id}/comments/{commentId}` | `DELETE /dev/recipes/{id}/comments/{commentId}` | cleanup right |
| `GET /recipes/{id}/recommendations` | `GET /dev/recipes/{id}/recommendations` | base gate + candidate post-filter |
| `GET /recipes/{id}/remixes` | `GET /dev/recipes/{id}/remixes` | origin gate + strict repo |
| `POST /recipes` | `POST /dev/recipes` | create + remix origin gate |
| `PUT /recipes/{id}` | `PUT /dev/recipes/{id}` | owner + ACTIVE |
| `DELETE /recipes/{id}` | `DELETE /dev/recipes/{id}` | cleanup right |
| `POST /recipes/{id}/presigned-urls` | `POST /dev/recipes/{id}/presigned-urls` | owner + ACTIVE |
| `POST /recipes/{id}/finalize` | `POST /dev/recipes/{id}/finalize` | owner + ACTIVE |
| `PUT /recipes/{id}/images` | `PUT /dev/recipes/{id}/images` | owner + ACTIVE |
| `POST /recipes/{id}/reports` | `POST /dev/recipes/{id}/reports` | report gate |
| `GET /me/calendar?year&month` | `GET /dev/me/calendar?year&month` | monthly aggregate recompute |
| `GET /me/calendar?date` | `GET /dev/me/calendar?date` | day records silent filter |
| `GET /me/records/timeline` | `GET /dev/me/records/timeline` | timeline silent filter |
| `GET /me/records/{recordId}` | `GET /dev/me/records/{recordId}` | detail displayable gate |
| `POST /me/records?recipeId=...` | `POST /dev/me/records?recipeId=...` | record create gate |
| `DELETE /me/records/{recordId}` | `DELETE /dev/me/records/{recordId}` | cleanup right |
| `POST /me/recipe-books` | `POST /dev/me/recipe-books` | write parity |
| `PATCH /me/recipe-books/{bookId}` | `PATCH /dev/me/recipe-books/{bookId}` | write parity |
| `DELETE /me/recipe-books/{bookId}` | `DELETE /dev/me/recipe-books/{bookId}` | write parity |
| `PUT /me/recipe-books/order` | `PUT /dev/me/recipe-books/order` | write parity |
| `POST /me/recipe-books/{bookId}/recipes` | `POST /dev/me/recipe-books/{bookId}/recipes` | add pre-filter |
| `DELETE /me/recipe-books/{bookId}/recipes` | `DELETE /dev/me/recipe-books/{bookId}/recipes` | cleanup right |
| `GET /recipes/youtube/check` | `GET /dev/recipes/youtube/check` | strict duplicate check |
| `GET /recipes/ai/status/{jobId}` | `GET /dev/recipes/ai/status/{jobId}` | AI job status |

## 경로 shape가 바뀐 API

이 항목은 프론트에서 단순 prefix 치환으로 처리하면 깨진다.

| Current path | Dev V3 path | 변경점 |
|---|---|---|
| `POST /recipes/{id}/private` | `PATCH /dev/recipes/{id}/visibility` | body `{ "visibility": "PUBLIC" | "PRIVATE" | "RESTRICTED" }` 필요 |
| `POST /recipes/ai` | `POST /dev/recipes/ai` | 운영 sync 응답과 dev async 응답 shape가 다름. legacy sync UI는 job polling flow로 전환 필요 |
| `POST /recipes/ai/v2` | `POST /dev/recipes/ai` | dev에는 `/v2` suffix 없음. `Idempotency-Key` 유지 |
| `POST /recipes/extract` | `POST /dev/recipes/youtube/extract` | 운영 sync 응답과 dev async 응답 shape가 다름. legacy sync store는 V2 job flow로 전환 필요 |
| `POST /recipes/extract/v2` | `POST /dev/recipes/youtube/extract` | dev는 youtube namespace로 분리 |
| `GET /recipes/youtube/status/{jobId}` | `GET /dev/recipes/youtube/status/{jobId}` | status namespace 변경 |
| Next BFF `PUT/DELETE /api/bff/recipes/{id}` | backend `/dev/recipes/{id}` | BFF 내부 forwarding URL 수정 필요 |
| Next BFF `POST /api/recipes/ai` | backend `/dev/recipes/ai` | 이 BFF route를 계속 쓰면 forwarding URL 수정 필요 |

## 운영 path 그대로 사용 가능

| Path group | 이유 |
|---|---|
| `/token/**`, OAuth login/logout | recipe visibility/lifecycle/listing/source 무관 |
| `/me`, `/users/{id}`, profile image presign | user profile data |
| `/me/fridge/items/**`, `/ingredients`, `/ingredients/names`, `/search/ingredients` | ingredient/fridge item CRUD 또는 search. recipe detail list가 아님 |
| `/tags`, `/dish-types` | enum metadata |
| `/notifications/**`, `/notification-preferences/**`, `/ws-ticket` | notification/session infra |
| `/products`, `/credits/history`, `/webhooks/**` | billing/credits infra |
| `/me/survey` | user survey data |
| `/logs/**` | tracking |
| `/me/streak` | self aggregate only. recipe id/image/status를 노출하지 않음 |
| `/recipes/youtube/recommend` | `YoutubeRecommendation` 테이블 기반. `Recipe` visibility와 무관 |
| `/recipes/sitemap` | repository가 `ACTIVE + PUBLIC + LISTED` 조건을 사용 중 |

## 프론트 전환 주의사항

1. 전역 rewrite로 `/api/:path* -> /api/dev/:path*` 하면 안 된다. 운영 그대로 써야 하는 API가 많고, `/v2/recipes/**`, `/recipes/ai/v2`, `/recipes/extract/v2`는 dev에 같은 shape가 없다.
2. `BASE_API_URL`을 dev로 바꾸는 것만으로도 부족하다. server-side fetch는 `api.server.ts`에 하드코딩된 `/v2/recipes/**`, `/recipes/recommendations`, `/recipes/sitemap` 등을 개별 매핑해야 한다.
3. `RESTRICTED` 전환은 `dev.visibility.restricted-enabled=true` 환경에서만 가능하다. 운영 default는 false다.
4. dev search OpenSearch mirror를 쓰려면 `search.dev-index.enabled=true`와 dev alias bootstrap이 필요하다. disabled면 QueryDSL fallback이 동작한다.

## Sweep validation

- Backend compile: `./gradlew compileJava compileTestJava` 성공.
- 프론트 endpoint inventory: `src/shared/config/constants/api.ts`, `entities/recipe/model/api.ts`, `entities/recipe/model/api.server.ts`, recipe create/import/visibility/BFF route 기준 확인.
