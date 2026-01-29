package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.scheduler.IngredientStatisticsScheduler;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/admin/ingredients")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
@Tag(name = "관리자 재료 API", description = "관리자 권한으로 재료 데이터 관리 및 통계 갱신을 수행하는 API입니다.")
public class AdminIngredientController {

    private final IngredientStatisticsScheduler ingredientStatisticsScheduler;

    @PostMapping("/popularity")
    @Operation(summary = "재료 인기순위(통계) 수동 갱신", description = "전체 레시피를 분석하여 재료별 사용 횟수(인기 점수)를 최신화합니다.")
    public ResponseEntity<String> updateIngredientPopularity() {

        int updatedCount = ingredientStatisticsScheduler.updateUsageCountsManually();

        return ResponseEntity.ok(String.format("재료 인기순위 갱신 완료 (갱신된 재료: %d개)", updatedCount));
    }

}