package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/me/records")
@Tag(name = "요리 기록 API", description = "요리 기록 생성, 삭제, 조회 API입니다.")
public class CookingRecordController {

    private final CookingRecordService service;
    private final Hashids hashids;

    @PostMapping
    @Operation(summary = "요리 기록 생성", description = "특정 레시피에 대한 요리 기록을 추가합니다.")
    public ResponseEntity<?> createRecord(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long recipeId) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        var record = service.createCookingRecord(userId, recipeId);
        return ResponseEntity.ok(Map.of("recordId", hashids.encode(record.getId()), "message", "요리 기록이 추가되었습니다."));
    }

    @DeleteMapping("/{recordId}")
    @Operation(summary = "요리 기록 삭제", description = "나의 요리 기록을 삭제합니다.")
    public ResponseEntity<?> deleteRecord(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long recordId) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        service.deleteCookingRecord(userId, recordId);
        return ResponseEntity.ok(Map.of("message", "요리 기록이 삭제되었습니다."));
    }

    @GetMapping("/{recordId}")
    @Operation(summary = "요리 기록 상세 조회", description = "요리한 레시피의 상세 기록을 조회합니다.")
    public ResponseEntity<CookingRecordDto> getRecord(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long recordId) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(service.getRecordDetail(userId, recordId));
    }
}