package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.client.CoupangApiClient;
import com.jdc.recipe_service.scheduler.IngredientLinkBatchScheduler;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/admin/coupang")
@RequiredArgsConstructor
public class AdminCoupangController {

    private final CoupangApiClient coupangApiClient;
    private final IngredientLinkBatchScheduler ingredientLinkBatchService;

    /**
     * 1. 딥링크 변환기 (테스트용)
     */
    @PostMapping("/deeplink")
    public ResponseEntity<?> generateDeepLink(@RequestBody Map<String, String> request) {
        String rawUrl = request.get("url");
        String deepLink = coupangApiClient.createDeepLink(rawUrl);

        return ResponseEntity.ok(Map.of(
                "originalUrl", rawUrl,
                "deepLink", deepLink == null ? "변환 실패" : deepLink
        ));
    }

    /**
     * 2. 정규 재료(Ingredients) 일괄 업데이트 트리거
     * 주의: 재료가 700개면 약 23분 소요됨 (Time-out 주의)
     */
    @PostMapping("/batch/ingredients")
    public ResponseEntity<?> runIngredientBatch() {
        int count = ingredientLinkBatchService.updateAllIngredientLinks();

        return ResponseEntity.ok(Map.of(
                "message", "정규 재료 배치 실행 완료",
                "updatedCount", count
        ));
    }

    /**
     * 3. 커스텀 재료(RecipeIngredients) 배치 트리거 (다음 30개 이름 처리)
     * 한 번 호출할 때마다 처리 안 된 다음 30개 이름을 가져와서 처리함
     */
    @PostMapping("/batch/custom-ingredients")
    public ResponseEntity<?> runCustomIngredientBatch() {
        int updatedRows = ingredientLinkBatchService.updateCustomIngredientsContinuous();

        return ResponseEntity.ok(Map.of(
                "message", "커스텀 재료 배치 실행 완료 (다음 청크 처리)",
                "updatedRows", updatedRows
        ));
    }

    /**
     * 4. 특정 재료(단건) 강제 업데이트
     */
    @PostMapping("/update/{ingredientId}")
    public ResponseEntity<?> updateIngredientLink(@PathVariable Long ingredientId) {
        String updatedLink = ingredientLinkBatchService.updateLinkByIngredientId(ingredientId);

        return ResponseEntity.ok(Map.of(
                "ingredientId", ingredientId,
                "message", "단건 업데이트 성공",
                "currentLink", updatedLink
        ));
    }
}