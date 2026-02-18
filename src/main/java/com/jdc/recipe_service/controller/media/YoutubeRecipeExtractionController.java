package com.jdc.recipe_service.controller.media;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.media.YoutubeRecipeExtractionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Youtube Recipe API", description = "유튜브 레시피 추출용 API")
@RestController
@RequestMapping("/api/dev/recipes/youtube")
@RequiredArgsConstructor
@Slf4j
public class YoutubeRecipeExtractionController {

    private final YoutubeRecipeExtractionService youtubeRecipeExtractionService;

    @PostMapping("/extract")
    @Operation(summary = "[V2] 유튜브 추출 요청", description = "기존과 동일한 응답 구조를 유지하며, 프리미엄 옵션에 따라 크레딧 차감 및 이미지 생성이 결정됩니다.")
    public ResponseEntity<JobIdResponse> extractYoutubeRecipeV2(
            @RequestHeader("Idempotency-Key") String idempotencyKey,
            @RequestParam("url") String url,
            @RequestParam(value = "mode", defaultValue = "TEXT_MODE") RecipeDisplayMode mode,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new com.jdc.recipe_service.exception.CustomException(com.jdc.recipe_service.exception.ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        String nickname = userDetails.getUser().getNickname();

        Long jobId = youtubeRecipeExtractionService.createYoutubeExtractionJob(url, userId, nickname, idempotencyKey, mode);

        youtubeRecipeExtractionService.processYoutubeExtractionAsync(jobId, url, userId, nickname, mode);

        return ResponseEntity.ok(new JobIdResponse(jobId));
    }

    @GetMapping("/status/{jobId}")
    @Operation(summary = "[V2] 유튜브 추출 상태 조회")
    public ResponseEntity<JobStatusDto> getYoutubeJobStatus(@Parameter(description = "인코딩된 Job ID") @DecodeId Long jobId) {
        return ResponseEntity.ok(youtubeRecipeExtractionService.getJobStatus(jobId));
    }
}