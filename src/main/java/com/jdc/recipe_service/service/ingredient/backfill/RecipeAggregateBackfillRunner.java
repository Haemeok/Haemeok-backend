package com.jdc.recipe_service.service.ingredient.backfill;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * Recipe aggregate 백필 자동 실행 진입점. 기본 OFF.
 *
 * <p>스타트업 시 한 번 실행되며, {@code recipe.aggregate-backfill.enabled=true}일 때만 chunk loop
 * 진입. 명시적으로 enable해야 작동 — 이중 안전장치 (코드 + config).
 *
 * <p><b>실행 순서</b>: {@link RecipeIngredientBackfillRunner}(정규화 row 백필)가 끝난 후 실행되어야
 * 의미 있다. {@link Order} 값을 정규화 runner보다 큰 값으로 둬서 같은 startup에서 동시 enable했을 때
 * 정규화가 먼저 끝나도록 한다 (실제 운영은 보통 두 번에 나눠 실행 권장).
 *
 * <p><b>chunk-level 실패 처리</b>: 정규화 runner와 동일 — keyset 위치를 알 수 없어 즉시 종료.
 * 운영자가 startId를 명시 지정해 재개.
 */
@Component
@RequiredArgsConstructor
@Slf4j
@Order(20)  // RecipeIngredientBackfillRunner(@Order(10))보다 뒤 — 정규화 후 aggregate 합산
public class RecipeAggregateBackfillRunner implements ApplicationRunner {

    private final RecipeAggregateBackfillProperties properties;
    private final RecipeAggregateBackfillService service;

    @Override
    public void run(ApplicationArguments args) {
        if (!properties.isEnabled()) {
            log.debug("[RecipeAggregateBackfill] disabled — skip");
            return;
        }

        long lastId = properties.getStartId();
        long processed = 0;
        long maxRows = properties.getMaxRows();
        int batchSize = properties.getBatchSize();
        boolean dryRun = properties.isDryRun();
        long sleepMs = properties.getSleepMs();
        boolean stopOnError = properties.isStopOnError();
        RecipeAggregateBackfillProperties.MarketPriceMode marketPriceMode = properties.getMarketPriceMode();
        boolean includePending = properties.isIncludePending();

        log.info("[RecipeAggregateBackfill] start — dryRun={}, batchSize={}, startId={}, maxRows={}, sleepMs={}, stopOnError={}, marketPriceMode={}, includePending={}",
                dryRun, batchSize, lastId, maxRows, sleepMs, stopOnError, marketPriceMode, includePending);

        RecipeAggregateBackfillResult total = RecipeAggregateBackfillResult.empty();

        while (true) {
            RecipeAggregateBackfillResult chunk;
            try {
                chunk = service.backfillChunk(lastId, batchSize, dryRun, stopOnError, marketPriceMode, includePending);
            } catch (RuntimeException e) {
                log.error("[RecipeAggregateBackfill] chunk failed at lastId={} — terminating loop (cannot advance keyset safely): {}",
                        lastId, e.getMessage(), e);
                break;
            }

            if (chunk.scanned() == 0) {
                log.info("[RecipeAggregateBackfill] no more targets — terminating");
                break;
            }

            total = total.plus(chunk);
            lastId = chunk.lastProcessedId();
            processed += chunk.scanned();

            log.info("[RecipeAggregateBackfill] dryRun={} scanned={} recalculated={} skipped={} failed={} lastId={}",
                    dryRun, chunk.scanned(), chunk.recalculated(), chunk.skipped(), chunk.failed(), chunk.lastProcessedId());

            if (maxRows > 0 && processed >= maxRows) {
                log.info("[RecipeAggregateBackfill] reached maxRows={} — terminating", maxRows);
                break;
            }

            sleepIfConfigured(sleepMs);
        }

        log.info("[RecipeAggregateBackfill] complete — totalScanned={} recalculated={} skipped={} failed={}",
                total.scanned(), total.recalculated(), total.skipped(), total.failed());
    }

    private void sleepIfConfigured(long sleepMs) {
        if (sleepMs <= 0) return;
        try {
            Thread.sleep(sleepMs);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
