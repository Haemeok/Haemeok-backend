package com.jdc.recipe_service.service.ingredient.backfill;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * 정규화 백필 자동 실행 진입점. 기본 OFF.
 *
 * <p>스타트업 시 한 번 실행되며, {@code ingredient.normalization-backfill.enabled=true}일 때만
 * chunk loop 진입. 명시적으로 enable해야 작동 — 이중 안전장치 (코드 + config).
 *
 * <p><b>실행 흐름</b>
 * <ol>
 *   <li>enabled 체크 — false면 즉시 return</li>
 *   <li>chunk loop: 매 iteration마다 backfillChunk 호출 → 결과 누적</li>
 *   <li>scanned=0 → loop 종료 (대상 없음)</li>
 *   <li>maxRows 도달 → loop 종료</li>
 *   <li>chunk-level 예외 → 즉시 종료 (keyset 위치 모름)</li>
 *   <li>row-level 예외는 service가 stopOnError 정책에 따라 처리</li>
 *   <li>chunk 간 sleep-ms로 DB 부하 조절</li>
 * </ol>
 *
 * <p>Runner 자체는 트랜잭션 없음 — chunk 단위 commit이 backfillChunk의 {@code @Transactional} 책임.
 */
@Component
@RequiredArgsConstructor
@Slf4j
@Order(10)  // aggregate runner(@Order(20))보다 먼저 실행 — 정규화가 끝나야 aggregate 합산이 의미 있음
public class RecipeIngredientBackfillRunner implements ApplicationRunner {

    private final RecipeIngredientBackfillProperties properties;
    private final RecipeIngredientBackfillService service;

    @Override
    public void run(ApplicationArguments args) {
        if (!properties.isEnabled()) {
            log.debug("[IngredientNormalizationBackfill] disabled — skip");
            return;
        }

        long lastId = properties.getStartId();
        long processed = 0;
        long maxRows = properties.getMaxRows();
        int batchSize = properties.getBatchSize();
        boolean dryRun = properties.isDryRun();
        long sleepMs = properties.getSleepMs();
        boolean stopOnError = properties.isStopOnError();

        log.info("[IngredientNormalizationBackfill] start — dryRun={}, batchSize={}, startId={}, maxRows={}, sleepMs={}, stopOnError={}",
                dryRun, batchSize, lastId, maxRows, sleepMs, stopOnError);

        RecipeIngredientBackfillResult total = RecipeIngredientBackfillResult.empty();

        while (true) {
            RecipeIngredientBackfillResult chunk;
            try {
                chunk = service.backfillChunk(lastId, batchSize, dryRun, stopOnError);
            } catch (RuntimeException e) {
                // chunk-level 실패는 진행 위치(lastId)를 알 수 없다 — keyset의 lastId는 row id이고
                // batchSize는 개수라서 임의 advance는 dense ID에선 미처리 row 누락, sparse ID에선
                // 같은 실패 구간 무한 반복 위험. row-level 실패는 service가 stopOnError 정책에 따라
                // 처리하고, chunk-level 실패는 stopOnError와 무관하게 즉시 종료. 운영자가 로그 보고
                // 원인 파악 후 startId를 명시 지정해 재개.
                log.error("[IngredientNormalizationBackfill] chunk failed at lastId={} — terminating loop (cannot advance keyset safely): {}",
                        lastId, e.getMessage(), e);
                break;
            }

            if (chunk.scanned() == 0) {
                log.info("[IngredientNormalizationBackfill] no more targets — terminating");
                break;
            }

            total = total.plus(chunk);
            lastId = chunk.lastProcessedId();
            processed += chunk.scanned();

            log.info("[IngredientNormalizationBackfill] dryRun={} scanned={} mapped={} partial={} unresolved={} customSkipped={} failed={} lastId={}",
                    dryRun, chunk.scanned(), chunk.mapped(), chunk.partial(), chunk.unresolved(),
                    chunk.customSkipped(), chunk.failed(), chunk.lastProcessedId());

            if (maxRows > 0 && processed >= maxRows) {
                log.info("[IngredientNormalizationBackfill] reached maxRows={} — terminating", maxRows);
                break;
            }

            sleepIfConfigured(sleepMs);
        }

        log.info("[IngredientNormalizationBackfill] complete — totalScanned={} mapped={} partial={} unresolved={} customSkipped={} failed={}",
                total.scanned(), total.mapped(), total.partial(), total.unresolved(),
                total.customSkipped(), total.failed());
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
