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
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;
import java.util.Objects;

@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/opensearch")
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
    @Operation(summary = "레시피 인덱스 생성", description = "OpenSearch에 'recipes' 인덱스를 생성합니다.")
    public ResponseEntity<?> createRecipesIndex() {
        try {
            boolean ok = recipeIndexingService.createRecipeIndex();
            return ResponseEntity.ok(Map.of("created", ok));
        } catch (IOException e) {
            return ResponseEntity.status(500).body(Map.of(
                    "error", "recipes 인덱스 생성 실패",
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
    @Operation(summary = "레시피 전체 재색인 (Bulk)", description = "모든 레시피 데이터를 Bulk API를 사용하여 효율적으로 재색인합니다.")
    public ResponseEntity<Map<String, String>> reindexAllRecipes() {
        recipeIndexingService.indexAllRecipes();
        return ResponseEntity.ok(Map.of("message", "전체 레시피 Bulk 재색인 작업이 시작되었습니다. 서버 로그를 확인하세요."));
    }
}