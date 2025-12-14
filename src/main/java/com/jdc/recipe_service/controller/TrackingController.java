package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.log.LogRequestDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ActionLogService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/logs")
public class TrackingController {

    private final ActionLogService actionLogService;

    @PostMapping("/click")
    public ResponseEntity<String> trackClick(
            @RequestBody LogRequestDto requestDto,
            HttpServletRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        String ip = getClientIp(request);

        String userAgent = request.getHeader("User-Agent");

        Long loginUserId = null;
        if (userDetails != null) {
            loginUserId = userDetails.getId();
        }

        actionLogService.saveLog(
                requestDto.getAction(),
                requestDto.getUuid(),
                ip,
                userAgent,
                loginUserId
        );

        return ResponseEntity.ok("ok");
    }

    private String getClientIp(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_CLIENT_IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_X_FORWARDED_FOR");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }
}