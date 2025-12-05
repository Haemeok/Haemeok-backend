package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.AiImageTestRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.AiPromptRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ai.RecipeTestService;
import com.jdc.recipe_service.service.ai.RecipeTestServiceV2;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/test/ai-recipe")
@RequiredArgsConstructor
public class AiTestController {

    private final RecipeTestService recipeService;
    private final RecipeTestServiceV2 recipeServiceV2;

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

    @PostMapping("/prompt")
    @Operation(summary = "프롬프트 템플릿 테스트", description = "프롬프트 내의 {{KEY}}를 requestData의 값으로 치환하여 AI 요청을 보냅니다. 프롬프트 엔지니어링용입니다.")
    public ResponseEntity<RecipeCreateRequestDto> testPromptTemplate(
            @RequestBody AiPromptRequestDto request) {

        RecipeCreateRequestDto result = recipeService.generateRecipeFromTemplate(request);

        return ResponseEntity.ok(result);
    }

    /**
     * [REAL] 실제 DB 저장 + 커스텀 이미지 생성 테스트
     * 요청: 레시피 JSON + 이미지 프롬프트
     * 결과: 내 계정(Token)으로 DB에 저장됨 & S3에 이미지 저장됨
     */
    @PostMapping("/image")
    @Operation(summary = "실제 DB 저장 및 이미지 생성", description = "로그인한 유저의 계정으로 레시피를 저장하고, 입력한 프롬프트로 이미지를 생성해 연결합니다.")
    public ResponseEntity<RecipeCreateRequestDto> createRealRecipeWithImage(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody AiImageTestRequestDto request) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        RecipeCreateRequestDto result = recipeService.createRealRecipeWithCustomImage(userId, request);
        return ResponseEntity.ok(result);
    }

    /**
     * [REAL V2] Grok All-in-One 로직 (New!)
     * Grok이 프롬프트 전체를 작성하고 Imagen이 그리는 최신 로직
     */
    @PostMapping("/image2")
    @Operation(summary = "V2: 실제 DB 저장 + Grok 자동 프롬프트 생성 (NEW)",
            description = "Grok이 미슐랭 셰프 페르소나로 완벽한 프롬프트를 작성해주는 V2 로직입니다. (껍질, 참치, 국물 오류 해결됨)")
    public ResponseEntity<RecipeCreateRequestDto> createRealRecipeWithImageV2(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody AiImageTestRequestDto request) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        RecipeCreateRequestDto result = recipeServiceV2.createRealRecipeWithCustomImage(userId, request);

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