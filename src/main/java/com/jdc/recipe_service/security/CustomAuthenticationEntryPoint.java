package com.jdc.recipe_service.security;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Slf4j
@Component
public class CustomAuthenticationEntryPoint implements AuthenticationEntryPoint {

    @Override
    public void commence(
            HttpServletRequest request,
            HttpServletResponse response,
            AuthenticationException authException
    ) throws IOException {

        String path = request.getRequestURI();
        log.warn("🚫 인증 실패 - 요청 경로: {}", path);

        // ✅ /login 관련 요청은 기본 페이지로 리디렉트
        if (path.equals("/login") || path.startsWith("/oauth2") || path.startsWith("/login")) {
            response.sendRedirect("/login/oauth2");
            return;
        }

        // 그 외는 JSON 에러 응답
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        response.getWriter().write("{\"error\": \"인증이 필요하거나 토큰이 만료되었습니다.\"}");
    }
}