package com.jdc.recipe_service.event;

import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
@RequiredArgsConstructor
@Slf4j
public class UserRecipeEventListener {

    private final RecipeIndexingService recipeIndexingService;

    @Async
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onUserRecipeCreated(UserRecipeCreatedEvent event) {
        log.info("유저 레시피 등록 이벤트 수신: ID {}", event.getRecipeId());

        recipeIndexingService.indexRecipeSafelyWithRetry(event.getRecipeId());
    }
}