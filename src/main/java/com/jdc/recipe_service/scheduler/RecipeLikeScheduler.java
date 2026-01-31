package com.jdc.recipe_service.scheduler;

import com.jdc.recipe_service.domain.repository.RecipeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
@Slf4j
public class RecipeLikeScheduler {

    private final RecipeRepository recipeRepository;

    @EventListener(ApplicationReadyEvent.class)
    @Transactional
    public void initDataOnStartup() {
        log.info("[Scheduler] 서버 시작 감지: 초기 인기 레시피 집계 시작");
        refreshPopularRecipes();
    }

    /**
     * 매 시간 정각(0분 0초)마다 실행
     * (서비스 규모에 따라 10분, 30분 등으로 조정 가능)
     */
    @Scheduled(cron = "0 0 * * * *")
    @Transactional
    public void refreshPopularRecipes() {
        log.info("[Scheduler] 주간 인기 레시피 집계 시작");
        long start = System.currentTimeMillis();

        LocalDateTime oneWeekAgo = LocalDateTime.now().minusDays(7);

        recipeRepository.updateAllWeeklyLikeCounts(oneWeekAgo);

        recipeRepository.updateAllWeeklyFavoriteCounts(oneWeekAgo);

        long end = System.currentTimeMillis();
        log.info("[Scheduler] 집계 완료 (소요시간: {}ms)", end - start);
    }
}