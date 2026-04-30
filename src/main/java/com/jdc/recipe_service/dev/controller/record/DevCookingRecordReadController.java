package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.record.DevCookingRecordReadService;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordFeedResponse;
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
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 요리 기록 조회 API (timeline + detail).
 *
 * 운영 {@code GET /api/me/records/timeline}, {@code GET /api/me/records/{recordId}} 미러.
 * dev V3 차이점은 {@link DevCookingRecordReadService}가 담당:
 *  - timeline: record 안 RESTRICTED/non-ACTIVE/PENDING 레시피 silent filter (그룹 전부 차단되면 그룹 자체 제거)
 *  - detail: 단건이라 silent skip 대신 차단 → COOKING_RECORD_NOT_FOUND
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/me/records")
@Tag(name = "Dev V3 요리 기록 조회 API",
        description = "사용자 cooking record에 다른 사람 RESTRICTED/non-ACTIVE 레시피 정보 누수 silent filter")
public class DevCookingRecordReadController {

    private final DevCookingRecordReadService devCookingRecordReadService;

    @GetMapping("/timeline")
    @Operation(summary = "Dev V3 요리 기록 타임라인",
            description = """
                    운영 `GET /api/me/records/timeline` 미러. dev V3 차이점:
                      - **silent filter**: record 안 recipe가 현재 RESTRICTED/non-ACTIVE/PENDING/FAILED면 응답에서 silently 제외
                        (cooked 시점엔 PUBLIC이었더라도 현재 가시성 + imageReady 정책으로 재평가)
                      - 그룹의 모든 record가 차단되면 그룹 자체도 응답에서 제거
                      - 통계가 일부 손실될 수 있음 — favorites/saved-books와 동일한 silent filter 패턴
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — 날짜 그룹 + record summary"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<CookingRecordFeedResponse> getRecordFeed(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devCookingRecordReadService.getRecordFeed(userId, pageable));
    }

    @GetMapping("/{recordId}")
    @Operation(summary = "Dev V3 요리 기록 상세",
            description = """
                    운영 `GET /api/me/records/{recordId}` 미러. dev V3 차이점:
                      - **단건 차단**: record가 가리키는 recipe가 현재 inaccessible 또는 PENDING/FAILED면 COOKING_RECORD_NOT_FOUND 응답
                        (timeline과 달리 single record라 silent skip 대신 명시 차단)
                      - 운영 ownership check (USER_ACCESS_DENIED) → 운영 service 그대로 위임
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "404", description = "record 없음 또는 표시 불가 (COOKING_RECORD_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<CookingRecordDto> getRecordDetail(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "기록 ID (HashID)") @DecodeId("recordId") Long recordId) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devCookingRecordReadService.getRecordDetail(userId, recordId));
    }
}
