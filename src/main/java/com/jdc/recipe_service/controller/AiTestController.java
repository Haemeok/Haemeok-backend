package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.scheduler.RecipeBatchScheduler;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ai.RecipeTestService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/test/ai-recipe")
@RequiredArgsConstructor
public class AiTestController {

    private final RecipeTestService recipeService;
    private final RecipeBatchScheduler recipeBatchScheduler;

    /**
     * [TEST] 텍스트 생성 테스트 (이미지 X, DB 저장 X)
     */
    @PostMapping
    @Operation(summary = "AI 레시피 텍스트 생성 테스트", description = "이미지 생성이나 DB 저장 없이, 순수하게 AI가 생성한 JSON 결과만 확인합니다.")
    public ResponseEntity<RecipeCreateRequestDto> generateTextRecipe(
            @Parameter(description = "요리 컨셉 (INGREDIENT_FOCUS, COST_EFFECTIVE, NUTRITION_BALANCE 등)")
            @RequestParam AiRecipeConcept concept,
            @RequestBody AiRecipeRequestDto aiRequest) {

        RecipeCreateRequestDto result = recipeService.generateRecipeTextOnly(
                concept,
                aiRequest
        );
        return ResponseEntity.ok(result);
    }

    /**
     * [REAL] 실제 DB 저장 + 커스텀 이미지 생성 테스트
     * 요청: 레시피 JSON (AI가 만든 결과물을 수정해서 넣어도 됨)
     * 결과: 내 계정(Token)으로 DB에 저장됨 & S3에 이미지 저장됨
     */
    @PostMapping("/image")
    @Operation(summary = "실제 DB 저장 및 이미지 생성", description = "로그인한 유저의 계정으로 레시피를 저장하고, 입력한 프롬프트로 이미지를 생성해 연결합니다.")
    public ResponseEntity<RecipeCreateRequestDto> createRealRecipeWithImage(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody RecipeCreateRequestDto request) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        RecipeCreateRequestDto result = recipeService.createRealRecipeWithCustomImage(userId, request);
        return ResponseEntity.ok(result);
    }

    /**
     * [TEST] 분석 로직 테스트
     */
    @PostMapping("/analyze/{recipeId}")
    @Operation(summary = "레시피 분석 테스트 (결과값만 확인)", description = "DB에 저장된 레시피 ID를 넣으면, 가격/팁/욕설여부 분석 결과를 JSON으로 반환합니다.")
    public ResponseEntity<RecipeAnalysisResponseDto> analyzeRecipeTest(
            @DecodeId Long recipeId) {

        RecipeAnalysisResponseDto result = recipeService.analyzeRecipeTest(recipeId);

        return ResponseEntity.ok(result);
    }

    /**
     * [TEST] 다중 레시피 일괄 등록 (테스트 계정 랜덤 배정)
     * 요청: 레시피 JSON 리스트 (Array)
     * 파라미터: type=odd (홀수 ID), type=even (짝수 ID)
     * 로직: 90001~90100 사이의 테스트 계정 중 조건에 맞는 ID를 랜덤 배정하여 등록
     */
    @PostMapping("/batch-insert")
    @Operation(summary = "레시피 일괄 등록 (테스트 계정 랜덤)", description = "JSON 리스트를 받아 다중 레시피를 저장하고, 입력한 프롬프트로 이미지를 생성해 연결합니다.")
    public ResponseEntity<List<RecipeCreateRequestDto>> batchInsertRecipes(
            @RequestBody List<RecipeCreateRequestDto> requests,
            @RequestParam String type) {

        List<RecipeCreateRequestDto> results = recipeService.batchInsertRecipes(requests, type);
        return ResponseEntity.ok(results);
    }

    /**
     * [ADMIN] S3 배치 수동 실행 (대량 등록용)
     * ex) POST /api/test/ai-recipe/scheduler/manual?type=odd&limit=2000
     */
    @PostMapping("/s3-import")
    @Operation(summary = "S3 배치 수동 실행 (ADMIN)", description = "S3에 있는 파일을 지정된 개수만큼 즉시 처리합니다. (비동기 실행)")
    public ResponseEntity<String> triggerManualBatch(
            @Parameter(description = "odd 또는 even") @RequestParam String type,
            @Parameter(description = "실행할 파일 개수 (예: 2000)") @RequestParam int limit) {

        recipeBatchScheduler.runManualBatch(type, limit);

        return ResponseEntity.ok(String.format(
                "배치 작업이 백그라운드에서 시작되었습니다. (Type: %s, Limit: %d) 서버 로그를 확인하세요.",
                type, limit
        ));
    }
}