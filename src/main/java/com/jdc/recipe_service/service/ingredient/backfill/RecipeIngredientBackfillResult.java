package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.service.ingredient.normalize.IngredientResolutionStatus;

/**
 * Chunk 처리 결과 + 누적 통계.
 *
 * <ul>
 *   <li>{@code scanned}: 조회된 row 수 (target query 결과)</li>
 *   <li>{@code mapped}/{@code partial}/{@code unresolved}: 결정된 status 분포</li>
 *   <li>{@code customSkipped}: target query에서 안 잡혔지만 안전망으로 service에서 추가 skip된 CUSTOM</li>
 *   <li>{@code failed}: row 처리 중 예외 (catch 후 통계로만 카운트)</li>
 *   <li>{@code lastProcessedId}: 다음 chunk의 lastId 인자. scanned=0이면 -1 → loop 종료 신호</li>
 * </ul>
 */
public record RecipeIngredientBackfillResult(
        int scanned,
        int mapped,
        int partial,
        int unresolved,
        int customSkipped,
        int failed,
        long lastProcessedId
) {

    public static RecipeIngredientBackfillResult empty() {
        return new RecipeIngredientBackfillResult(0, 0, 0, 0, 0, 0, -1L);
    }

    public RecipeIngredientBackfillResult plus(RecipeIngredientBackfillResult other) {
        return new RecipeIngredientBackfillResult(
                scanned + other.scanned,
                mapped + other.mapped,
                partial + other.partial,
                unresolved + other.unresolved,
                customSkipped + other.customSkipped,
                failed + other.failed,
                Math.max(lastProcessedId, other.lastProcessedId)
        );
    }

    /**
     * mutable builder for chunk processing — 한 chunk 안에서 row별로 increment.
     */
    public static final class Accumulator {
        int scanned;
        int mapped;
        int partial;
        int unresolved;
        int customSkipped;
        int failed;
        long lastProcessedId = -1L;

        public void incrementScanned() { scanned++; }
        public void incrementFailed() { failed++; }
        public void incrementCustomSkipped() { customSkipped++; }

        public void incrementByStatus(IngredientResolutionStatus status) {
            if (status == null) return;
            switch (status) {
                case MAPPED -> mapped++;
                case PARTIAL -> partial++;
                case UNRESOLVED -> unresolved++;
                case CUSTOM -> customSkipped++;
            }
        }

        public void recordLastId(long id) {
            if (id > lastProcessedId) lastProcessedId = id;
        }

        public RecipeIngredientBackfillResult build() {
            return new RecipeIngredientBackfillResult(
                    scanned, mapped, partial, unresolved, customSkipped, failed, lastProcessedId);
        }
    }
}
