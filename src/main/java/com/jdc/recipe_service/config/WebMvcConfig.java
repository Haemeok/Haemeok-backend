package com.jdc.recipe_service.config;

import com.jdc.recipe_service.interceptor.UserVisitInterceptor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    @Autowired(required = false)
    private UserVisitInterceptor userVisitInterceptor;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        if (userVisitInterceptor != null) {
            registry.addInterceptor(userVisitInterceptor)
                    .addPathPatterns("/api/**")
                    .excludePathPatterns("/api/auth/**", "/api/ws-ticket", "/favicon.ico", "/error");
        }
    }
}