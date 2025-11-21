package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.service.ai.RecipeTestService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/test/ai-recipe")
@RequiredArgsConstructor
public class AiTestController {

    private final RecipeTestService recipeService;

    @PostMapping
    public ResponseEntity<RecipeCreateRequestDto> generateTextRecipe(
            @RequestParam RobotType robotType,
            @RequestBody AiRecipeRequestDto aiRequest) {

        RecipeCreateRequestDto result = recipeService.generateRecipeTextOnly(
                robotType,
                aiRequest
        );
        return ResponseEntity.ok(result);
    }

    @PostMapping("/analyze/{recipeId}")
    @Operation(summary = "레시피 분석 테스트 (결과값만 확인)", description = "DB에 저장된 레시피 ID를 넣으면, 가격/팁/욕설여부 분석 결과를 JSON으로 반환합니다.")
    public ResponseEntity<RecipeAnalysisResponseDto> analyzeRecipeTest(
            @PathVariable Long recipeId) {

        RecipeAnalysisResponseDto result = recipeService.analyzeRecipeTest(recipeId);

        return ResponseEntity.ok(result);
    }


}