package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.OpenSearchIndexService;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;
import java.util.Objects;

@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/opensearch")
@PreAuthorize("hasRole('ADMIN')")
@Tag(name = "OpenSearch 관리 API", description = "OpenSearch 인덱스 생성, 삭제, 재색인을 위한 관리자 전용 API입니다.")
public class OpenSearchAdminController {

    private final OpenSearchIndexService indexService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeRepository recipeRepository;

    @PostMapping("/ingredients/index")
    @Operation(summary = "재료 인덱스 재생성", description = "'ingredients' 인덱스를 삭제하고 재생성하며 전체 재료를 다시 인덱싱합니다.")
    public ResponseEntity<Map<String, ?>> recreateIngredientsIndex() {
        log.info("재료 인덱스 재생성 시작");
        try {
            boolean reindexed = indexService.recreateIngredientIndex();
            log.info("인덱스 재생성 결과: {}", reindexed);
            return ResponseEntity.ok(Map.of("reindexed", reindexed));
        }
        catch (CustomException ce) {
            log.error("인덱스 재생성 실패 (CustomException): {}", ce.getMessage(), ce);
            return ResponseEntity
                    .status(ce.getErrorCode().getStatus())
                    .body(Map.of(
                            "code",   ce.getErrorCode().getCode(),
                            "message", ce.getMessage()
                    ));
        }
        catch (Exception e) {
            log.error("인덱스 재생성 중 알 수 없는 오류", e);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of(
                            "code",    ErrorCode.INTERNAL_SERVER_ERROR.getCode(),
                            "message", ErrorCode.INTERNAL_SERVER_ERROR.getMessage()
                    ));
        }
    }

    @DeleteMapping("/ingredients/index")
    @Operation(summary = "재료 인덱스 삭제", description = "'ingredients' 인덱스를 삭제합니다.")
    public ResponseEntity<Map<String, Boolean>> deleteIngredientsIndex() {
        boolean deleted = indexService.deleteIngredientIndex();
        return ResponseEntity.ok(Map.of("deleted", deleted));
    }

    @DeleteMapping("/index/{name}")
    @Operation(summary = "지정 인덱스 삭제", description = "지정한 이름의 인덱스를 삭제합니다.")
    public ResponseEntity<Map<String, ?>> deleteIndexByName(
            @Parameter(description = "삭제할 인덱스 이름")
            @PathVariable String name) {
        log.info("관리자 요청: 인덱스 '{}' 삭제", name);
        try {
            boolean deleted = indexService.deleteIndex(name);
            return ResponseEntity.ok(Map.of("deleted", deleted));
        } catch (CustomException ce) {
            log.error("인덱스 '{}' 삭제 실패: {}", name, ce.getMessage());
            return ResponseEntity
                    .status(ce.getErrorCode().getStatus())
                    .body(Map.of(
                            "code", ce.getErrorCode().getCode(),
                            "message", ce.getMessage()
                    ));
        } catch (Exception e) {
            log.error("인덱스 '{}' 삭제 중 알 수 없는 오류", name, e);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of(
                            "code", ErrorCode.INTERNAL_SERVER_ERROR.getCode(),
                            "message", ErrorCode.INTERNAL_SERVER_ERROR.getMessage()
                    ));
        }
    }

    @PostMapping("/create-recipes-index")
    @Operation(summary = "레시피 인덱스 생성",
            description = "OpenSearch에 명시 mapping으로 인덱스를 생성합니다. " +
                    "indexName 미지정 시 'recipes'에 직접 생성. alias swap 운영 시에는 " +
                    "indexName=recipes_v2 같은 신규 이름으로 생성 후 reindex-recipes + swap-recipes-alias 순서로 진행하세요.")
    public ResponseEntity<?> createRecipesIndex(
            @Parameter(description = "생성할 인덱스 이름 (alias swap 시 신규 이름). 미지정 시 'recipes'")
            @RequestParam(name = "indexName", required = false) String indexName) {
        try {
            boolean ok = (indexName == null || indexName.isBlank())
                    ? recipeIndexingService.createRecipeIndex()
                    : recipeIndexingService.createRecipeIndex(indexName);
            return ResponseEntity.ok(Map.of("created", ok, "indexName",
                    indexName == null || indexName.isBlank() ? "recipes" : indexName));
        } catch (IOException e) {
            return ResponseEntity.status(500).body(Map.of(
                    "error", "인덱스 생성 실패",
                    "message", e.getMessage()
            ));
        }
    }

    @PostMapping("/create-ingredients-index")
    @Operation(summary = "재료 인덱스 생성", description = "OpenSearch에 'ingredients' 인덱스를 새로 생성합니다.")
    public ResponseEntity<Map<String, ?>> createIngredientsIndex() {
        log.info("재료 인덱스 생성 시작");
        try {
            boolean created = indexService.createIngredientIndex();
            log.info("인덱스 생성 결과: {}", created);
            return ResponseEntity.ok(Map.of("created", created));
        }
        catch (CustomException ce) {
            log.error("인덱스 생성 실패 (CustomException): {}", ce.getMessage(), ce);
            return ResponseEntity
                    .status(ce.getErrorCode().getStatus())
                    .body(Map.of(
                            "code",    ce.getErrorCode().getCode(),
                            "message", ce.getMessage()
                    ));
        }
        catch (IOException ioe) {
            log.error("인덱스 생성 중 I/O 오류", ioe);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of(
                            "code",    ErrorCode.SEARCH_FAILURE.getCode(),
                            "message", "인덱스 생성 중 I/O 오류: " + ioe.getMessage()
                    ));
        }
        catch (Exception e) {
            log.error("인덱스 생성 중 알 수 없는 오류", e);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of(
                            "code",    ErrorCode.INTERNAL_SERVER_ERROR.getCode(),
                            "message", ErrorCode.INTERNAL_SERVER_ERROR.getMessage()
                    ));
        }
    }

    @PostMapping("/reindex-recipes")
    @Operation(summary = "레시피 전체 재색인 (Bulk)",
            description = "DB에서 모든 레시피를 Bulk API로 재색인합니다. " +
                    "alias swap 운영 시 indexName=recipes_v2 같은 신규 이름을 지정하면 그 인덱스에 색인. 미지정 시 'recipes'. " +
                    "응답에 totalIndexed/failedCount/failedIds 포함 — failedCount > 0이면 alias swap을 진행하면 안 됩니다.")
    public ResponseEntity<Map<String, Object>> reindexAllRecipes(
            @Parameter(description = "색인 대상 인덱스 (alias swap 시 신규 이름). 미지정 시 'recipes'")
            @RequestParam(name = "indexName", required = false) String indexName) {
        RecipeIndexingService.ReindexResult result = (indexName == null || indexName.isBlank())
                ? recipeIndexingService.indexAllRecipes()
                : recipeIndexingService.indexAllRecipes(indexName);
        Map<String, Object> body = new java.util.LinkedHashMap<>();
        body.put("indexName", result.indexName());
        body.put("totalIndexed", result.totalIndexed());
        body.put("failedCount", result.failedIds().size());
        body.put("failedIds", result.failedIds());
        body.put("hasFailures", result.hasFailures());
        body.put("message", result.hasFailures()
                ? "Bulk 재색인 부분 실패. **alias swap 금지** — failedIds 원인 조사 후 재실행하세요."
                : "Bulk 재색인 완료. " + (RecipeIndexingService.RECIPE_INDEX_ALIAS.equals(result.indexName())
                        ? "직접 'recipes'에 색인됐습니다."
                        : "검증 후 swap-recipes-alias로 alias 전환하세요."));
        return result.hasFailures()
                ? ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(body)
                : ResponseEntity.ok(body);
    }

    @PostMapping("/swap-recipes-alias")
    @Operation(summary = "레시피 alias 전환 (atomic)",
            description = "alias 'recipes'를 indexName이 가리키는 신규 인덱스로 atomic하게 전환합니다. " +
                    "기존 alias가 가리키던 인덱스는 응답에 포함 — 전환 후 운영자가 별도로 삭제해야 합니다. " +
                    "indexName이 'recipes' 자체이면 거부 (alias name과 동일하면 alias 등록 자체가 불가). " +
                    "alias가 처음 등록되는 케이스는 previousIndices가 빈 배열로 응답.")
    public ResponseEntity<?> swapRecipesAlias(
            @Parameter(description = "alias가 가리킬 신규 인덱스 이름 (필수). 예: recipes_v2", required = true)
            @RequestParam(name = "indexName") String indexName) {
        if (indexName == null || indexName.isBlank()) {
            return ResponseEntity.badRequest().body(Map.of(
                    "code", ErrorCode.INVALID_INPUT_VALUE.getCode(),
                    "message", "indexName 파라미터가 필요합니다."));
        }
        // alias name과 동일한 concrete index는 OpenSearch가 공존시킬 수 없어 swap이 결국 실패한다 — 미리 거부.
        if (RecipeIndexingService.RECIPE_INDEX_ALIAS.equals(indexName)) {
            return ResponseEntity.badRequest().body(Map.of(
                    "code", ErrorCode.INVALID_INPUT_VALUE.getCode(),
                    "message", "indexName='" + RecipeIndexingService.RECIPE_INDEX_ALIAS
                            + "'은 alias 자기 자신을 swap target으로 지정한 것 — 새 인덱스 이름(예: recipes_v2)을 지정하세요."));
        }
        try {
            var previous = recipeIndexingService.swapRecipeAlias(indexName);
            return ResponseEntity.ok(Map.of(
                    "alias", RecipeIndexingService.RECIPE_INDEX_ALIAS,
                    "newIndex", indexName,
                    "previousIndices", previous,
                    "note", "previousIndices의 구 인덱스는 검증 후 별도 삭제하세요 (delete-index?name=...)."));
        } catch (IOException e) {
            log.error("alias swap 실패 — newIndex={}", indexName, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(Map.of(
                    "code", ErrorCode.SEARCH_FAILURE.getCode(),
                    "message", "alias swap 실패: " + e.getMessage()));
        }
    }
}