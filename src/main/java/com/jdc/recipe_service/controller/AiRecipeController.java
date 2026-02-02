package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.facade.AiRecipeFacade;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

@RestController
@RequestMapping("/api/recipes/ai")
@RequiredArgsConstructor
@Tag(name = "AI 레시피 생성 API", description = "AI를 이용한 레시피 생성 전용 API입니다.")
public class AiRecipeController {

    private final AiRecipeFacade aiRecipeFacade;

    @PostMapping
    @Operation(
            summary = "AI 레시피 생성 (동기 대기 방식)",
            description = "요청 시 텍스트 생성 후 DB에 저장하며, **이미지가 생성될 때까지(약 3~5초) 응답을 대기**합니다. 완료되면 이미지가 포함된 레시피 상세 정보를 반환합니다."
    )
    public DeferredResult<ResponseEntity<PresignedUrlResponse>> createAiRecipe(
            @Parameter(description = "요리 컨셉 선택 (INGREDIENT_FOCUS, COST_EFFECTIVE, NUTRITION_BALANCE, FINE_DINING)")
            @RequestParam(value = "concept") AiRecipeConcept concept,

            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "AI 생성 요청 정보")
            @RequestBody @Valid RecipeWithImageUploadRequest request,

            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        return aiRecipeFacade.generateAndSave(
                request,
                concept,
                userDetails.getUser().getId()
        );
    }

    @PostMapping("/v2")
    @Operation(summary = "[V2] AI 레시피 생성 요청 (비동기)", description = "아이폰 백그라운드 끊김 해결을 위한 비동기 API입니다.")
    public ResponseEntity<JobIdResponse> generateAiRecipeV2(
            @RequestHeader("Idempotency-Key") String idempotencyKey,
            @RequestParam AiRecipeConcept concept,
            @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) throw new CustomException(ErrorCode.UNAUTHORIZED);
        Long userId = userDetails.getUser().getId();

        Long jobId = aiRecipeFacade.createAiGenerationJobV2(request, concept, userId, idempotencyKey);

        aiRecipeFacade.processAiGenerationAsyncV2(jobId, request, concept, userId);

        return ResponseEntity.ok(new JobIdResponse(jobId));
    }

    @GetMapping("/status/{jobId}")
    @Operation(summary = "AI 생성 상태 조회 (Polling)", description = "진행률과 최종 생성된 레시피 ID를 확인합니다.")
    public ResponseEntity<JobStatusDto> getAiJobStatus(@DecodeId Long jobId) {
        return ResponseEntity.ok(aiRecipeFacade.getJobStatus(jobId));
    }
}