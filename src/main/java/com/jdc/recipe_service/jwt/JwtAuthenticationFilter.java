package com.jdc.recipe_service.jwt;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Slf4j
@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtTokenProvider jwtTokenProvider;
    private final UserDetailsService userDetailsService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        String authHeader = request.getHeader("Authorization");
        log.info("🔐 Authorization 헤더: {}", authHeader);

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            log.info("📦 JWT 토큰 추출됨: {}", token);

            try {
                // 만약 validateToken 내부에서 만료/유효성 오류 발생 시 JwtException 던질 수 있음
                if (jwtTokenProvider.validateToken(token)) {
                    // ----- 토큰이 유효할 경우 -----
                    Long userId = jwtTokenProvider.getUserIdFromToken(token);
                    log.info("✅ 토큰 유효. userId: {}", userId);

                    UserDetails userDetails = userDetailsService.loadUserByUsername(userId.toString());
                    log.info("🙋‍♂️ UserDetails 로드됨: {}", userDetails.getUsername());

                    UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(
                            userDetails, null, userDetails.getAuthorities()
                    );
                    authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                    SecurityContextHolder.getContext().setAuthentication(authentication);
                    log.info("🔓 인증 완료. SecurityContext 에 저장됨.");

                } else {
                    // ----- validateToken이 false 일 때 (추가로 boolean 반환하게 만들 수도 있음) -----
                    log.warn("❌ 유효하지 않은 토큰.");
                    response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                    response.setContentType("application/json;charset=UTF-8");
                    response.getWriter().write("{\"message\":\"유효하지 않은 토큰입니다.\"}");
                    return; // 필터 체인 진행 중단
                }

            } catch (io.jsonwebtoken.JwtException e) {
                // ----- 만료되거나 서명이 틀린 토큰 등 예외 발생 -----
                log.warn("❌ JWTException 발생: {}", e.getMessage());
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.setContentType("application/json;charset=UTF-8");
                // e.getMessage()가 "토큰이 만료되었습니다." 등일 수 있음
                response.getWriter().write("{\"message\":\"" + e.getMessage() + "\"}");
                return; // 더 이상 진행하지 않고 401 응답
            }

        } else {
            log.warn("🚫 Authorization 헤더 없음 또는 'Bearer '로 시작하지 않음.");
        }

        // 토큰이 없거나 정상 인증된 경우 → 체인 계속 진행
        filterChain.doFilter(request, response);
    }
    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) throws ServletException {
        String path = request.getRequestURI();
        return path.equals("/") || path.equals("/login")
                || path.startsWith("/css/") || path.startsWith("/js/")
                || path.startsWith("/images/") || path.startsWith("/oauth2")
                || path.startsWith("/h2-console") || path.equals("/favicon.ico");
    }

}
