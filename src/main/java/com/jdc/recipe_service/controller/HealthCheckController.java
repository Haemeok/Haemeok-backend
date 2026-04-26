package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.service.chat.UpstageHealthIndicator;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.actuate.health.Health;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequiredArgsConstructor
@Tag(name = "헬스 체크 API", description = "서버 상태를 확인하기 위한 간단한 API입니다.")
public class HealthCheckController {

    private final UpstageHealthIndicator upstageHealthIndicator;

    @GetMapping("/api/health")
    @Operation(summary = "헬스 체크", description = "서버가 정상적으로 작동 중인지 확인합니다. 응답은 항상 'OK'입니다.")
    public String health() {
        return "OK";
    }

    @GetMapping("/api/health/ai")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Upstage AI 헬스 체크 (관리자 전용)",
            description = "Mini 모델에 max_tokens=1 ping을 보내 응답 여부 확인. 호출 시 비용 발생.")
    public ResponseEntity<Map<String, Object>> aiHealth() {
        Health h = upstageHealthIndicator.health();
        return ResponseEntity.ok(Map.of(
                "status", h.getStatus().getCode(),
                "details", h.getDetails()
        ));
    }
}
