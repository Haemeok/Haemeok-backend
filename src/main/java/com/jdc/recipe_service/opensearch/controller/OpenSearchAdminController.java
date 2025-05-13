package com.jdc.recipe_service.opensearch.controller;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.opensearch.service.OpenSearchIndexService;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;
import java.util.Objects;

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
    public ResponseEntity<Map<String, Boolean>> recreateIngredientsIndex() {
        boolean reindexed = indexService.recreateIngredientIndex();
        return ResponseEntity.ok(Map.of("reindexed", reindexed));
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
    public ResponseEntity<Map<String, Boolean>> deleteIndexByName(
            @PathVariable("name") String indexName) {
        boolean deleted;
        try {
            deleted = indexService.deleteIndex(indexName);
        } catch (Exception e) {
            return ResponseEntity.status(500)
                    .body(Map.of("error", false));
        }
        return ResponseEntity.ok(Map.of("deleted", deleted));
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

    /** 전체 레시피를 한 번에 색인합니다 */
    @PostMapping("/reindex-recipes")
    public ResponseEntity<Map<String, Boolean>> reindexAllRecipes() {
        recipeRepository.findAll().stream()
                // (혹시 null이 섞여 들어올 수 있다면)
                .filter(Objects::nonNull)
                // Recipe → Long id 로 변환
                .map(Recipe::getId)
                // id 기반 indexRecipe(Long) 호출
                .forEach(recipeIndexingService::indexRecipe);
        return ResponseEntity.ok(Map.of("reindexed", true));
    }
}
