package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import lombok.RequiredArgsConstructor;
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

    // 월별 (일별 savings 리스트 + 월합계 saving)
    @GetMapping
    public ResponseEntity<CalendarMonthSummaryDto> monthSummary(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam int year,
            @RequestParam int month
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        CalendarMonthSummaryDto result = service.getMonthlySummary(userId, year, month);
        return ResponseEntity.ok(result);
    }

    // 특정 날짜의 상세 기록 리스트
    @GetMapping("/{date}")
    public ResponseEntity<List<CookingRecordDto>> dayRecords(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        List<CookingRecordDto> records = service.getDailyRecords(userId, date);
        return ResponseEntity.ok(records);
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
