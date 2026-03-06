package com.jdc.recipe_service.config.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Slf4j
@Component
@Order(Ordered.HIGHEST_PRECEDENCE + 1)
public class RequestLoggingFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        long startTime = System.currentTimeMillis();

        try {
            filterChain.doFilter(request, response);
        } finally {
            long duration = System.currentTimeMillis() - startTime;
            int status = response.getStatus();
            String method = request.getMethod();
            String uri = request.getRequestURI();
            String queryString = request.getQueryString() != null ? "?" + request.getQueryString() : "";

            if (!uri.startsWith("/actuator") && !uri.equals("/")) {
                if (status >= 400) {
                    log.warn("HTTP {} {}{} - {} [{}ms]", method, uri, queryString, status, duration);
                } else {
                    log.info("HTTP {} {}{} - {} [{}ms]", method, uri, queryString, status, duration);
                }
            }
        }
    }
}