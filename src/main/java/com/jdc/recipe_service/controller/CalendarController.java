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

import java.time.LocalDate;
import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/me/calendar")
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

    // 월별 (일별 savings 리스트 + 월합계 saving)
    @GetMapping(params = { "year", "month" })
    public ResponseEntity<CalendarMonthSummaryDto> monthSummary(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam int year,
            @RequestParam int month
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        var result = service.getMonthlySummary(userId, year, month);
        return ResponseEntity.ok(result);
    }

    // 2) 특정 날짜 기록 리스트
    @GetMapping(params = "date")
    public ResponseEntity<List<CookingRecordSummaryDto>> dayRecords(
            @AuthenticationPrincipal CustomUserDetails userDetails,
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

    // 개별 기록 상세
    @GetMapping("/records/{id}")
    public ResponseEntity<CookingRecordDto> recordDetail(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PathVariable Long id
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        CookingRecordDto detail = service.getRecordDetail(userId, id);
        return ResponseEntity.ok(detail);
    }
}
