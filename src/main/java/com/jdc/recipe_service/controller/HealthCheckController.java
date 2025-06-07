package com.jdc.recipe_service.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Tag(name = "헬스 체크 API", description = "서버 상태를 확인하기 위한 간단한 API입니다.")
public class HealthCheckController {

    @GetMapping("/api/health")
    @Operation(summary = "헬스 체크", description = "서버가 정상적으로 작동 중인지 확인합니다. 응답은 항상 'OK'입니다.")
    public String health() {
        return "OK";
    }
}