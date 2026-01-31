package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.repository.RecipeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.LocalDateTime;


@Slf4j
@Service
@RequiredArgsConstructor
public class RecipeWarmUpService {

    private final RecipeSearchServiceV2 recipeSearchService;
    private final RecipeExtractionService recipeExtractionService;
    private final RecipeRepository recipeRepository;

    private final TransactionTemplate transactionTemplate;

    /**
     * 배포 직후 데이터가 즉시 보이도록 강제 호출
     * [변경 2] 메서드 레벨의 @Transactional 제거 -> DB 커넥션을 오래 잡지 않게 함
     */
    public void runWarmUp() {
        log.info("=== 🚀 Starting Backend Data Warm-up ===");

        try {
            transactionTemplate.executeWithoutResult(status -> {
                LocalDateTime sevenDaysAgo = LocalDateTime.now().minusDays(7);
                recipeRepository.updateAllWeeklyLikeCounts(sevenDaysAgo);
            });
            log.info("✅ 1. Weekly Like Counts updated");

            recipeSearchService.checkOpenSearchHealth();
            log.info("✅ 2. OpenSearch Health Check performed");

            Pageable firstTen = PageRequest.of(0, 10);

            recipeSearchService.getPopularRecipesStaticV2("weekly", firstTen);
            recipeSearchService.getBudgetRecipesStaticV2(10000, firstTen);
            log.info("✅ 3. Popular & Budget Recipes cached");

            var recommended = recipeExtractionService.getRecommendedRecipes();
            if (recommended.isEmpty()) {
                log.info("⚠️ YouTube Recommendation DB is empty. Triggering manual refresh...");
                recipeExtractionService.refreshRecommendedRecipes();
            }
            log.info("✅ 4. YouTube Recommendation Warm-up completed");

            log.info("=== ✨ Warm-up Sequence Finished Successfully ===");

        } catch (Exception e) {
            log.error("❌ Warm-up failed: {}", e.getMessage(), e);
        }
    }
}