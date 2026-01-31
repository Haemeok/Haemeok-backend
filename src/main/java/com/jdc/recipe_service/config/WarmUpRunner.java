package com.jdc.recipe_service.config;

import com.jdc.recipe_service.service.RecipeWarmUpService;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class WarmUpRunner implements ApplicationRunner {

    private final RecipeWarmUpService warmUpService;

    @Override
    public void run(ApplicationArguments args) {
        warmUpService.runWarmUp();
    }
}