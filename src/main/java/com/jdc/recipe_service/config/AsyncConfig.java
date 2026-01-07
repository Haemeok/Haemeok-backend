package com.jdc.recipe_service.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskDecorator;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

@Configuration
@EnableAsync
public class AsyncConfig {

    @Value("${app.async.recipeExtraction.corePool:20}")
    private int corePool;

    @Value("${app.async.recipeExtraction.maxPool:50}")
    private int maxPool;

    @Value("${app.async.recipeExtraction.queueCapacity:100}")
    private int queueCapacity;


    @Bean(name = "recipeExtractionExecutor")
    public Executor recipeExtractionExecutor() {
        ThreadPoolTaskExecutor exec = new ThreadPoolTaskExecutor();

        exec.setCorePoolSize(corePool);
        exec.setMaxPoolSize(maxPool);
        exec.setQueueCapacity(queueCapacity);

        exec.setThreadNamePrefix("recipe-extract-");
        exec.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());

        exec.setTaskDecorator(new ContextCopyingDecorator());

        exec.initialize();
        return exec;
    }

    @Bean(name = "taskExecutor")
    public Executor taskExecutor() {
        ThreadPoolTaskExecutor exec = new ThreadPoolTaskExecutor();
        exec.setCorePoolSize(5);
        exec.setMaxPoolSize(20);
        exec.setQueueCapacity(50);

        exec.setThreadNamePrefix("Async-General-");
        exec.initialize();
        return exec;
    }


    static class ContextCopyingDecorator implements TaskDecorator {
        @Override
        public Runnable decorate(Runnable runnable) {
            SecurityContext context = SecurityContextHolder.getContext();
            return () -> {
                try {
                    SecurityContextHolder.setContext(context);
                    runnable.run();
                } finally {
                    SecurityContextHolder.clearContext();
                }
            };
        }
    }
}