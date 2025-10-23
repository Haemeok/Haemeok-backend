package com.jdc.recipe_service.opensearch.indexingfailure;

import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class FailedIndexingRecoveryService {

    private final IndexingFailureLogService failureLogService;
    private final RecipeIndexingService indexingService;

    @Scheduled(cron = "0 0 3 * * *")
    @Transactional(readOnly = true)
    public void runFailedIndexingRecovery() {
        log.info("=== [배치 시작] 누락된 OpenSearch 인덱싱 복구 ===");

        List<Long> failedIds = failureLogService.getAllFailedRecipeIds();
        log.info("복구 대상 레시피 수: {}", failedIds.size());

        if (failedIds.isEmpty()) {
            log.info("복구 대상 누락 레시피가 없습니다. 배치 종료.");
            return;
        }

        for (Long recipeId : failedIds) {
            log.info("ID {} 레시피 복구 재시도 요청 시작.", recipeId);
            indexingService.indexRecipeSafelyWithRetry(recipeId);
        }

        log.info("=== [배치 종료] 누락된 OpenSearch 인덱싱 복구 요청 완료 ===");
    }
}