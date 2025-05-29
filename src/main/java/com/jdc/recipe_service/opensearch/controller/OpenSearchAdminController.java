package com.jdc.recipe_service.opensearch.controller;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.OpenSearchIndexService;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
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
public class OpenSearchAdminController {

    private final OpenSearchIndexService indexService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeRepository recipeRepository;

    /**
     * 'ingredients' 인덱스를 삭제하고 재생성 모든 재료를 다시 인덱싱
     */
    @PostMapping("/ingredients/index")
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

    /**
     * 'ingredients' 인덱스를 삭제
     */
    @DeleteMapping("/ingredients/index")
    public ResponseEntity<Map<String, Boolean>> deleteIngredientsIndex() {
        boolean deleted = indexService.deleteIngredientIndex();
        return ResponseEntity.ok(Map.of("deleted", deleted));
    }

    /**
     * 지정한 이름의 인덱스를 삭제
     */
    @DeleteMapping("/index/{name}")
    public ResponseEntity<Map<String, ?>> deleteIndexByName(
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

    /** recipes 인덱스를 생성하는 관리자용 API */
    @PostMapping("/create-recipes-index")
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

    /** 전체 레시피를 한 번에 색인합니다 */
    @PostMapping("/reindex-recipes")
    public ResponseEntity<Map<String, Boolean>> reindexAllRecipes() {
        recipeRepository.findAll().stream()
                .filter(Objects::nonNull)
                .map(Recipe::getId)
                .forEach(recipeIndexingService::indexRecipe);
        return ResponseEntity.ok(Map.of("reindexed", true));
    }
}
