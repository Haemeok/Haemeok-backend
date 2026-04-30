package com.jdc.recipe_service.dev.controller.media;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.facade.DevYoutubeRecipeExtractionFacade;
import com.jdc.recipe_service.dev.facade.DevYoutubeRecipeExtractionFacade.JobCreateResult;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

/**
 * Dev V3 YouTube extraction API.
 *
 * 운영 V1/V2 (RecipeController + RecipeExtractionService)와 분리된 새 경로.
 * V2와 동일한 단계 + dev 신규: imageGenModel 라우팅, EvidenceLevel 산출, 변동 비용 quota,
 * RecipeYoutubeInfo/RecipeYoutubeExtractionInfo 저장, idempotency 안전.
 *
 * Swap 시 운영 RecipeController가 이 facade를 호출하도록 갈아끼우면 됨.
 */
@Tag(name = "Dev YouTube Recipe Extraction API",
        description = "YouTube 레시피 추출 (dev V3) — 이미지 모델 선택 + 변동 비용 quota + evidence 추적")
@RestController
@RequestMapping("/api/dev/recipes/youtube")
@RequiredArgsConstructor
@Slf4j
public class DevYoutubeRecipeExtractionController {

    private final DevYoutubeRecipeExtractionFacade devFacade;

    @PostMapping("/extract")
    @Operation(summary = "YouTube 레시피 추출 (dev V3)",
            description = "URL과 이미지 모델 식별자를 받아 비동기 추출을 시작합니다. " +
                    "동일 Idempotency-Key 재요청 시 기존 jobId만 반환하고 async 재실행은 하지 않습니다. " +
                    "기본 비용 2 토큰, Gemini Multimodal fallback 사용 시 +3 (총 5).")
    @ApiResponses({
            @ApiResponse(responseCode = "200",
                    description = "작업 접수 성공 (jobId 반환). 신규 또는 기존 job 모두 200."),
            @ApiResponse(responseCode = "400",
                    description = "URL 형식 오류 (errorCode=907 INVALID_URL_FORMAT) " +
                            "또는 imageGenModel 화이트리스트 위반 (errorCode=703 UNSUPPORTED_IMAGE_MODEL)."),
            @ApiResponse(responseCode = "401",
                    description = "인증 필요 (errorCode=103 UNAUTHORIZED)."),
            @ApiResponse(responseCode = "429",
                    description = "일일 YouTube 추출 한도(인당 20) 초과 (errorCode=429 DAILY_QUOTA_EXCEEDED).")
    })
    public ResponseEntity<JobIdResponse> extractYoutubeRecipe(
            @RequestHeader(value = "Idempotency-Key", required = false) String idempotencyKey,
            @Parameter(description = "YouTube 영상 URL (watch / shorts 모두 지원)", required = true)
            @RequestParam("url") String url,
            @Parameter(description = "이미지 생성 모델 식별자 (whitelist). " +
                    "gemini-2.5-flash-image | gpt-image-2-low | gpt-image-2-medium | gpt-image-2-high",
                    example = "gemini-2.5-flash-image")
            @RequestParam(value = "imageGenModel", defaultValue = "gemini-2.5-flash-image") String imageGenModel,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();

        String finalKey = (idempotencyKey != null && !idempotencyKey.isBlank())
                ? idempotencyKey
                : UUID.randomUUID().toString();

        JobCreateResult jobResult = devFacade.createYoutubeExtractionJob(url, imageGenModel, userId, finalKey);

        // 새 job일 때만 async — idempotency 재요청 시 중복 실행 차단
        if (jobResult.created()) {
            devFacade.processYoutubeExtractionAsync(jobResult.jobId(), url, imageGenModel, userId);
        } else {
            log.info("🔁 [DevYt V3] idempotency 재요청 — async 호출 스킵, jobId={}", jobResult.jobId());
        }

        return ResponseEntity.ok(new JobIdResponse(jobResult.jobId()));
    }

    @GetMapping("/status/{jobId}")
    @Operation(summary = "추출 작업 상태 조회 (dev V3)",
            description = "Job ID로 진행률과 결과 레시피 ID를 조회합니다.")
    @ApiResponses({
            @ApiResponse(responseCode = "200",
                    description = "조회 성공 (status: PENDING|IN_PROGRESS|COMPLETED|FAILED)"),
            @ApiResponse(responseCode = "404",
                    description = "Job ID를 찾을 수 없음 (errorCode=909 RESOURCE_NOT_FOUND).")
    })
    public ResponseEntity<JobStatusDto> getYoutubeJobStatus(
            @Parameter(description = "인코딩된 Job ID") @DecodeId Long jobId) {
        return ResponseEntity.ok(devFacade.getJobStatus(jobId));
    }
}
