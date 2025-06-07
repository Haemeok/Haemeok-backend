package com.jdc.recipe_service.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@Tag(name = "홈", description = "서비스 상태 확인 및 Swagger 링크 안내 API")
public class HomeController {

    @GetMapping("/")
    @Operation(summary = "API 서버 상태 확인", description = "API 서버가 정상 작동 중인지 확인하고 Swagger 문서 링크를 반환합니다.")
    public String home() {
        return "Welcome to Recipe API! Swagger: /swagger-ui/index.html";
    }
}