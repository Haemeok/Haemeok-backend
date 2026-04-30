package com.jdc.recipe_service.service.ingredient.backfill;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.core.annotation.Order;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Backfill runner 실행 순서 잠금.
 *
 * <p>둘 다 동시에 enable 했을 때 정규화(row 단)가 먼저 끝나야 aggregate(Recipe 단) 합산이 의미가 있다.
 * Spring에서 unordered ApplicationRunner는 LOWEST_PRECEDENCE라 명시 @Order 없으면 우연히 순서가
 * 맞을 수도 있고 어긋날 수도 있다 — 양쪽 다 명시해서 회귀 잠금.
 */
class BackfillRunnerOrderTest {

    @Test
    @DisplayName("**MUST 회귀 차단**: normalization runner @Order < aggregate runner @Order")
    void normalizationRunsBeforeAggregate() {
        Order normalizationOrder = RecipeIngredientBackfillRunner.class.getAnnotation(Order.class);
        Order aggregateOrder = RecipeAggregateBackfillRunner.class.getAnnotation(Order.class);

        assertThat(normalizationOrder)
                .as("정규화 runner는 명시 @Order 필요 — 미명시 시 LOWEST_PRECEDENCE로 aggregate 뒤에 돌 위험")
                .isNotNull();
        assertThat(aggregateOrder)
                .as("aggregate runner도 명시 @Order 필요")
                .isNotNull();
        assertThat(normalizationOrder.value())
                .as("정규화 runner @Order 값이 aggregate runner보다 작아야 한다 (먼저 실행)")
                .isLessThan(aggregateOrder.value());
    }
}
