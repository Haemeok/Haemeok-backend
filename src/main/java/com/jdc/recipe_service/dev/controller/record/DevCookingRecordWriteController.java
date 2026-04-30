package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.record.DevCookingRecordWriteService;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * Dev V3 요리 기록 쓰기 API.
 *
 * 운영 {@code POST /api/me/records?recipeId=...}, {@code DELETE /api/me/records/{recordId}} 미러.
 * dev V3 차이점은 {@link DevCookingRecordWriteService}가 담당:
 *  - <b>create</b>: recipe 가시성 게이트 (RESTRICTED/PRIVATE non-owner / non-ACTIVE 차단)
 *  - <b>delete</b>: cleanup right — 운영 ownership check 그대로
 *
 * GET (timeline / detail / 캘린더)는 {@link com.jdc.recipe_service.dev.controller.record.DevCookingRecordReadController}와
 * {@link com.jdc.recipe_service.dev.controller.calendar.DevCalendarController}가 담당.
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/me/records")
@Tag(name = "Dev V3 요리 기록 쓰기 API",
        description = "create는 recipe 가시성 게이트 (운영 leak 차단), delete는 cleanup right")
public class DevCookingRecordWriteController {

    private final DevCookingRecordWriteService devCookingRecordWriteService;
    private final Hashids hashids;

    @PostMapping
    @Operation(summary = "Dev V3 요리 기록 생성",
            description = """
                    운영 `POST /api/me/records?recipeId=...` 미러. dev V3 차이점:
                      - **🚨 운영 leak 차단**: 운영 CookingRecordService.createCookingRecord는 recipe visibility 검사 없이
                        record 생성 → RESTRICTED/PRIVATE non-owner / non-ACTIVE 레시피에도 record 가능. dev V3가 가시성 게이트로 차단
                      - 게이트 통과 시 운영 service에 위임 (cost/savings 계산, first-record 배지 처리 포함)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "생성 성공 — {recordId, message}"),
            @ApiResponse(responseCode = "400", description = "recipeId query param 누락 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 레시피 non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> createRecord(
            @Parameter(description = "레시피 ID (HashID)", required = true) @DecodeId("recipeId") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        // boundary null guard: @DecodeId resolver는 query param 누락 시 null 반환 (throw 안 함) →
        // service의 findById(null) 의존성에 기대지 않고 controller에서 명시 차단
        if (recipeId == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "recipeId는 필수입니다.");
        }
        Long userId = userDetails.getUser().getId();

        CookingRecord record = devCookingRecordWriteService.createCookingRecord(userId, recipeId);
        return ResponseEntity.ok(Map.of(
                "recordId", hashids.encode(record.getId()),
                "message", "요리 기록이 추가되었습니다."
        ));
    }

    @DeleteMapping("/{recordId}")
    @Operation(summary = "Dev V3 요리 기록 삭제",
            description = """
                    운영 `DELETE /api/me/records/{recordId}` 미러. dev V3 차이점 없음 — cleanup right.
                    - 운영 service의 ownership check (USER_ACCESS_DENIED)가 1차 방어
                    - recipe lifecycle 무관 (예전 PUBLIC일 때 만든 record를 RESTRICTED 됐다고 막지 않음)
                    - 인증 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 record 아님 (USER_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "record 없음 (COOKING_RECORD_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, String>> deleteRecord(
            @Parameter(description = "기록 ID (HashID)") @DecodeId("recordId") Long recordId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devCookingRecordWriteService.deleteCookingRecord(userId, recordId);
        return ResponseEntity.ok(Map.of("message", "요리 기록이 삭제되었습니다."));
    }
}
