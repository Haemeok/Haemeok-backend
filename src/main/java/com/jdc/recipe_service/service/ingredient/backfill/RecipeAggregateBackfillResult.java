package com.jdc.recipe_service.service.ingredient.backfill;

/**
 * Recipe aggregate 백필 chunk 결과 + 누적 통계.
 *
 * <ul>
 *   <li>{@code scanned}: 조회된 Recipe 수</li>
 *   <li>{@code recalculated}: 새 calculator로 재계산된 Recipe 수 (dryRun이어도 카운트)</li>
 *   <li>{@code skipped}: ingredient가 없거나 모두 pending이라 의미 있는 변화 없는 Recipe</li>
 *   <li>{@code failed}: row 처리 중 예외 (catch 후 통계로만 카운트)</li>
 *   <li>{@code lastProcessedId}: 다음 chunk의 lastId 인자. scanned=0이면 -1 → loop 종료 신호</li>
 * </ul>
 */
public record RecipeAggregateBackfillResult(
        int scanned,
        int recalculated,
        int skipped,
        int failed,
        long lastProcessedId
) {

    public static RecipeAggregateBackfillResult empty() {
        return new RecipeAggregateBackfillResult(0, 0, 0, 0, -1L);
    }

    public RecipeAggregateBackfillResult plus(RecipeAggregateBackfillResult other) {
        return new RecipeAggregateBackfillResult(
                scanned + other.scanned,
                recalculated + other.recalculated,
                skipped + other.skipped,
                failed + other.failed,
                Math.max(lastProcessedId, other.lastProcessedId)
        );
    }

    public static final class Accumulator {
        int scanned;
        int recalculated;
        int skipped;
        int failed;
        long lastProcessedId = -1L;

        public void incrementScanned() { scanned++; }
        public void incrementRecalculated() { recalculated++; }
        public void incrementSkipped() { skipped++; }
        public void incrementFailed() { failed++; }

        public void recordLastId(long id) {
            if (id > lastProcessedId) lastProcessedId = id;
        }

        public RecipeAggregateBackfillResult build() {
            return new RecipeAggregateBackfillResult(scanned, recalculated, skipped, failed, lastProcessedId);
        }
    }
}
