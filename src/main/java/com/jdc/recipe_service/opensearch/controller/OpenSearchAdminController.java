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

    /** recipes 인덱스를 생성하는 관리자용 API */
    @PostMapping("/create-index")
    public ResponseEntity<?> createIndex() {
        try {
            boolean ok = indexService.createRecipeIndex();
            return ResponseEntity.ok(Map.of("created", ok));
        } catch (IOException e) {
            return ResponseEntity.status(500).body(Map.of(
                    "error", "인덱스 생성 실패",
                    "message", e.getMessage()
            ));
        }
    }
}
