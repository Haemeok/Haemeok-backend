package com.jdc.recipe_service.scheduler;

import com.jdc.recipe_service.service.IngredientStatisticsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;


@Component
@RequiredArgsConstructor
@Slf4j
public class IngredientStatisticsScheduler {

    private final IngredientStatisticsService ingredientStatisticsService;

    @Scheduled(cron = "0 30 3 * * *")
    public void updateDailyUsageCounts() {
        log.info(">>>> [Stats Scheduler] Daily Ingredient Usage Count Update Started.");

        try {
            int updatedCount = ingredientStatisticsService.updateAllIngredientPopularity();
            log.info(">>>> [Stats Scheduler] Finished. (Updated: {} items)", updatedCount);
        } catch (Exception e) {
            log.error(">>>> [Stats Scheduler] Failed to update usage counts.", e);
        }
    }

    public int updateUsageCountsManually() {
        log.info(">>>> [Manual Stats] Usage Count Update Started.");
        return ingredientStatisticsService.updateAllIngredientPopularity();
    }
}