package com.jdc.recipe_service.dev.controller.ai;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.facade.DevAiRecipeFacade;
import com.jdc.recipe_service.dev.facade.DevAiRecipeFacade.JobCreateResult;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

/**
 * Dev V3 AI 레시피 생성 API.
 *
 * 운영 V1 (AiRecipeController)와 분리된 새 경로. V2 (AiRecipeFacade.*V2) 와 동일한 동작 +
 * imageGenModel 화이트리스트 라우팅 + image_generation_model 컬럼 기록 + 정확한 환불 + idempotency
 * 안전 보장 (재요청 시 async 중복 실행 차단).
 *
 * 검증 후 swap 시 운영 AiRecipeController가 이 facade를 호출하도록 갈아끼우면 됨.
 */
@Tag(name = "Dev AI Recipe Generation API", description = "AI 레시피 생성 (dev V3) — 이미지 모델 선택 + 멱등성 안전")
@RestController
@RequestMapping("/api/dev/recipes/ai")
@RequiredArgsConstructor
@Slf4j
public class DevAiRecipeGenerationController {

    private final DevAiRecipeFacade devAiRecipeFacade;

    @PostMapping
    @Operation(summary = "AI 레시피 생성 요청 (dev V3)",
               description = "재료/상황 정보와 이미지 모델 식별자를 받아 비동기로 작업을 시작합니다. " +
                             "동일 Idempotency-Key로 재요청 시 기존 jobId만 반환하고 async 재실행은 하지 않습니다.")
    @ApiResponses({
            @ApiResponse(responseCode = "200",
                    description = "작업 접수 성공 (jobId 반환). 신규 또는 기존 job 모두 200."),
            @ApiResponse(responseCode = "400",
                    description = "imageGenModel 화이트리스트 위반 (errorCode=703 UNSUPPORTED_IMAGE_MODEL) " +
                                  "또는 aiRequest 누락 (errorCode=901 INVALID_INPUT_VALUE) 등 입력 검증 실패."),
            @ApiResponse(responseCode = "401",
                    description = "인증 필요 (errorCode=103 UNAUTHORIZED)."),
            @ApiResponse(responseCode = "429",
                    description = "일일 AI 생성 한도(인당 2회) 초과 + 보유 토큰 없음 " +
                                  "(errorCode=429 DAILY_QUOTA_EXCEEDED, retryAfter 포함).")
    })
    public ResponseEntity<JobIdResponse> generateAiRecipe(
            @RequestHeader(value = "Idempotency-Key", required = false) String idempotencyKey,
            @RequestParam("concept") AiRecipeConcept concept,
            @Parameter(description = "이미지 생성 모델 식별자 (whitelist). " +
                                     "gemini-2.5-flash-image | gpt-image-2-low | gpt-image-2-medium | gpt-image-2-high",
                       example = "gemini-2.5-flash-image")
            @RequestParam(value = "imageGenModel", defaultValue = "gemini-2.5-flash-image") String imageGenModel,
            @Valid @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        String finalKey = (idempotencyKey != null && !idempotencyKey.isBlank())
                ? idempotencyKey
                : UUID.randomUUID().toString();

        JobCreateResult jobResult = devAiRecipeFacade.createAiGenerationJob(
                request, concept, imageGenModel, userId, finalKey);

        // 새로 생성된 job일 때만 async 처리. 기존 job 재사용이면 중복 실행 차단.
        if (jobResult.created()) {
            devAiRecipeFacade.processAiGenerationAsync(
                    jobResult.jobId(), request, concept, imageGenModel, userId, jobResult.usedToken());
        } else {
            log.info("🔁 [DevAi V3] idempotency 재요청 — async 호출 스킵, jobId={}", jobResult.jobId());
        }

        return ResponseEntity.ok(new JobIdResponse(jobResult.jobId()));
    }

    @GetMapping("/status/{jobId}")
    @Operation(summary = "생성 작업 상태 조회 (dev V3)",
               description = "Job ID를 통해 생성 진행률과 결과 레시피 ID를 조회합니다.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 (status: PENDING|IN_PROGRESS|COMPLETED|FAILED)"),
            @ApiResponse(responseCode = "404",
                    description = "Job ID를 찾을 수 없음 (errorCode=909 RESOURCE_NOT_FOUND).")
    })
    public ResponseEntity<JobStatusDto> getAiJobStatus(@DecodeId Long jobId) {
        return ResponseEntity.ok(devAiRecipeFacade.getJobStatus(jobId));
    }
}
