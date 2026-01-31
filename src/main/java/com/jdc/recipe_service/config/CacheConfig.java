package com.jdc.recipe_service.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.cache.support.SimpleCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;
import java.util.Arrays;
import java.util.List;

@Configuration
@EnableCaching
public class CacheConfig {

    @Bean
    public CacheManager cacheManager() {
        SimpleCacheManager cacheManager = new SimpleCacheManager();
        List<CaffeineCache> caches = Arrays.asList(
                buildCache("ingredientSearch", 10, 500),
                buildCache("recipeRecommendations", 30, 1000),
                buildCache("popularRecipes", 60, 100),
                buildCache("budgetRecipes", 60, 100)
        );

        cacheManager.setCaches(caches);
        return cacheManager;
    }

    /**
     * 캐시 생성 헬퍼 메서드
     * @param name 캐시 이름
     * @param minutes 만료 시간(분)
     * @param maxSize 최대 저장 개수
     */
    private CaffeineCache buildCache(String name, int minutes, int maxSize) {
        return new CaffeineCache(name, Caffeine.newBuilder()
                .expireAfterWrite(Duration.ofMinutes(minutes))
                .maximumSize(maxSize)
                .recordStats()
                .build());
    }
}