package com.jdc.recipe_service.opensearch.controller;
import com.jdc.recipe_service.opensearch.service.OpenSearchIndexService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/opensearch")
public class OpenSearchAdminController {

    private final OpenSearchIndexService indexService;

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
}
