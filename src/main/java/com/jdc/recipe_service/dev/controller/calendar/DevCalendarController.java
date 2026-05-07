package com.jdc.recipe_service.dev.controller.calendar;

import com.jdc.recipe_service.dev.domain.dto.record.DevCookingRecordSummaryDto;
import com.jdc.recipe_service.dev.service.record.DevCookingRecordReadService;
import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.List;

/**
 * Dev V3 캘린더 API — date 기준 day records + year/month 기준 monthly summary.
 *
 * 운영 {@code GET /api/me/calendar?date=...}, {@code GET /api/me/calendar?year=...&month=...} 미러.
 * dev V3 차이점은 {@link DevCookingRecordReadService}가 담당:
 *  - day records 안 RESTRICTED/non-ACTIVE/PENDING 레시피 silent filter
 *  - monthly summary의 totalCount/totalSavings/firstImageUrl을 displayable 기준으로 재계산 (운영의 firstImageUrl 누수 차단)
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/me/calendar")
@Tag(name = "Dev V3 캘린더 API",
        description = "월간 summary + 특정 날짜 record list — RESTRICTED/non-ACTIVE/PENDING 레시피 silent filter (운영 firstImageUrl 누수 차단)")
public class DevCalendarController {

    private final DevCookingRecordReadService devCookingRecordReadService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @GetMapping(params = { "year", "month" })
    @Operation(summary = "Dev V3 월별 캘린더 요약",
            description = """
                    운영 `GET /api/me/calendar?year=...&month=...` 미러. dev V3 차이점:
                      - **🚨 운영 leak 차단**: 운영은 각 날짜별 첫 record의 recipe.imageKey를 firstImageUrl로 노출 →
                        RESTRICTED/non-ACTIVE/PENDING 레시피 이미지가 월간 캘린더에 노출. dev V3는 displayable 기준으로 재계산
                      - totalCount, monthlyTotalSavings, firstImageUrl 모두 displayable records만 집계
                      - 모든 record가 차단된 날짜는 응답 dailySummaries에서 제외
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — dailySummaries + monthlyTotalSavings"),
            @ApiResponse(responseCode = "400", description = "month가 1~12 범위 밖이거나 year가 1~9999 범위 밖 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<CalendarMonthSummaryDto> monthSummary(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "조회할 연도 (1~9999)", example = "2026") @RequestParam int year,
            @Parameter(description = "조회할 월 (1~12)", example = "4") @RequestParam int month) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        // boundary validation — 운영 LocalDate.of(year, 13, 1) 또는 year=Long.MAX_VALUE 모두 DateTimeException → 500.
        // 명시적 400으로 대체. ISO-8601 4자리 연도 (1~9999) + month 1~12로 제한 (캘린더 UI 의미상 충분).
        if (month < 1 || month > 12) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "month는 1~12 사이여야 합니다.");
        }
        if (year < 1 || year > 9999) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "year는 1~9999 사이여야 합니다.");
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devCookingRecordReadService.getMonthlySummary(
                userId, year, month, this::generateImageUrl));
    }

    @GetMapping(params = "date")
    @Operation(summary = "Dev V3 특정 날짜 요리 기록",
            description = """
                    dev V3 정책:
                      - **silent filter**: 그날 record 안 recipe가 현재 RESTRICTED/non-ACTIVE/PENDING/FAILED면 응답에서 silently 제외
                      - 각 record에 `visibility`, `isRemix` 포함
                      - 인증 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "400", description = "INVALID_INPUT_VALUE", content = @Content),
            @ApiResponse(responseCode = "401", description = "UNAUTHORIZED", content = @Content)
    })
    public ResponseEntity<List<DevCookingRecordSummaryDto>> dayRecords(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "조회할 날짜 (yyyy-MM-dd)", example = "2026-04-26", required = true)
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        if (date == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE);
        }
        Long userId = userDetails.getUser().getId();

        List<DevCookingRecordSummaryDto> summaries = devCookingRecordReadService.getDayRecords(
                userId, date, this::generateImageUrl);
        return ResponseEntity.ok(summaries);
    }
}
