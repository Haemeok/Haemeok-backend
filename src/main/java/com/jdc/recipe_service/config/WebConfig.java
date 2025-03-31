package com.jdc.recipe_service.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Override
    public void addViewControllers(ViewControllerRegistry registry) {
        // /login 및 /login/ 요청을 login/index.html로 포워딩
        registry.addViewController("/login").setViewName("forward:/login/index.html");
        registry.addViewController("/login/").setViewName("forward:/login/index.html");
    }
}
