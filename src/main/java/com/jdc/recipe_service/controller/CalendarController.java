package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.time.LocalDate;
import java.util.List;


@RestController
@RequiredArgsConstructor
@RequestMapping("/api/me/calendar")
@Tag(name = "요리 캘린더 API", description = "월별/일별 요리 기록 및 저장 비용 절감 내역을 조회하는 API입니다.")
public class CalendarController {

    private final CookingRecordService service;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s",
                bucketName, region, key);
    }

    @GetMapping(params = { "year", "month" })
    @Operation(summary = "월별 저장 내역 조회", description = "선택한 연도와 월의 일별 절감 금액과 월 전체 총합을 조회합니다.")
    public ResponseEntity<CalendarMonthSummaryDto> monthSummary(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "조회할 연도", example = "2025") @RequestParam int year,
            @Parameter(description = "조회할 월 (1~12)", example = "6") @RequestParam int month
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        var result = service.getMonthlySummary(userId, year, month);
        return ResponseEntity.ok(result);
    }

    @GetMapping(params = "date")
    @Operation(summary = "특정 날짜의 요리 기록 조회", description = "특정 날짜에 저장된 레시피 기록 리스트를 반환합니다.")
    public ResponseEntity<List<CookingRecordSummaryDto>> dayRecords(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "조회할 날짜 (yyyy-MM-dd 형식)", example = "2025-06-05")
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        if (date == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE);
        }

        Long userId = userDetails.getUser().getId();

        var entities = service.getDailyRecordEntities(userId, date);
        var summaries = entities.stream()
                .map(e -> CookingRecordSummaryDto.from(
                        e,
                        generateImageUrl(e.getRecipe().getImageKey())
                ))
                .toList();

        return ResponseEntity.ok(summaries);
    }

    @GetMapping("/records/{id}")
    @Operation(summary = "요리 기록 상세 조회", description = "요리한 레시피의 상세 기록을 조회합니다.")
    public ResponseEntity<CookingRecordDto> recordDetail(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "기록 ID", example = "1") @PathVariable Long id
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        CookingRecordDto detail = service.getRecordDetail(userId, id);
        return ResponseEntity.ok(detail);
    }
}
