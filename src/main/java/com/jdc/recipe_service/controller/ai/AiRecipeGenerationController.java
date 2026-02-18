package com.jdc.recipe_service.controller.ai;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ai.AiRecipeGenerationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "AI Recipe Generation V2 API", description = "AI 레시피 생성 (V2) 전용 API")
@RestController
@RequestMapping("/api/dev/recipes/ai")
@RequiredArgsConstructor
@Slf4j
public class AiRecipeGenerationController {

    private final AiRecipeGenerationService aiRecipeGenerationService;

    @PostMapping
    @Operation(summary = "AI 레시피 생성 요청", description = "재료/상황 정보와 생성 모드(TEXT/IMAGE)를 받아 비동기로 작업을 시작합니다.")
    public ResponseEntity<JobIdResponse> generateAiRecipe(
            @RequestHeader(value = "Idempotency-Key", required = false) String idempotencyKey,
            @RequestParam("concept") AiRecipeConcept concept,
            @RequestParam(value = "mode", defaultValue = "TEXT_MODE") RecipeDisplayMode mode,
            @Valid @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        String finalKey = (idempotencyKey != null && !idempotencyKey.isBlank())
                ? idempotencyKey
                : UUID.randomUUID().toString();

        Long jobId = aiRecipeGenerationService.createAiGenerationJob(
                request, concept, userId, finalKey, mode
        );

        aiRecipeGenerationService.processAiGenerationAsync(
                jobId, request, concept, userId, mode
        );

        return ResponseEntity.ok(new JobIdResponse(jobId));
    }

    @GetMapping("/status/{jobId}")
    @Operation(summary = "생성 작업 상태 조회", description = "Job ID를 통해 생성 진행률과 결과를 조회합니다.")
    public ResponseEntity<JobStatusDto> getAiJobStatus(@DecodeId Long jobId) {
        return ResponseEntity.ok(aiRecipeGenerationService.getJobStatus(jobId));
    }
}