package com.jdc.recipe_service.service.ingredient.backfill;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Recipe aggregate 백필 설정.
 *
 * <p>{@link RecipeIngredientBackfillService}는 row 단의 정규화 4개 필드만 채운다 — Recipe entity의
 * aggregate(totalIngredientCost / totalCalories / totalIngredientCount / nutrition / marketPrice)는
 * 별도로 재계산해야 한다. 이게 안 되면 검색/예산/영양 필터(목록)는 stale인 채로 detail만 새 값을 보임.
 *
 * <p>정규화 백필이 끝난 후 실행하는 게 정상 — 정규화가 미완이면 calculator가 path 1/2를 못 타서
 * 합산이 더 적게 나옴. 운영 절차: 정규화 backfill 완료 → aggregate backfill 실행.
 *
 * <p><b>운영 안전 기본값</b>: enabled=false, dryRun=true.
 *
 * <pre>
 * RECIPE_AGGREGATE_BACKFILL_ENABLED=true
 * RECIPE_AGGREGATE_BACKFILL_DRY_RUN=false
 * RECIPE_AGGREGATE_BACKFILL_BATCH_SIZE=200
 * RECIPE_AGGREGATE_BACKFILL_MAX_ROWS=0
 * RECIPE_AGGREGATE_BACKFILL_START_ID=0
 * RECIPE_AGGREGATE_BACKFILL_SLEEP_MS=200
 * RECIPE_AGGREGATE_BACKFILL_STOP_ON_ERROR=false
 * </pre>
 */
@Component
@ConfigurationProperties(prefix = "recipe.aggregate-backfill")
@Getter
@Setter
public class RecipeAggregateBackfillProperties {

    /** 백필 자동 실행 여부. 기본 false — ApplicationRunner가 즉시 return. */
    private boolean enabled = false;

    /** dry-run 모드. true면 DB write 없이 통계 로그만 남김. */
    private boolean dryRun = true;

    /**
     * 한 chunk에서 처리할 Recipe 수. ingredient는 batch 내에서 IN 절로 fetch — recipe당 row가 많은
     * 도메인 특성상 200 정도가 무난.
     */
    private int batchSize = 200;

    /** 시작 id (id > startId). 중단 후 재개 시 마지막 처리 id 지정. */
    private long startId = 0;

    /** 한 실행에서 처리할 최대 Recipe 수. 0이면 무제한. */
    private long maxRows = 0;

    /** chunk 간 sleep (ms) — DB 부하 조절. */
    private long sleepMs = 200;

    /** chunk 처리 중 예외 발생 시 즉시 중단 여부. false면 다음 chunk로 진행. */
    private boolean stopOnError = false;

    /**
     * pending line이 있는 recipe도 aggregate를 덮어쓸지 여부.
     *
     * <p>Calculator가 pending으로 둔 라인은 "0이 아니라 미반영"이라는 정책 — 합산이 불완전하다.
     * 그런데 Recipe aggregate에는 pending 정보가 없어서 운영 검색/필터에서는 불완전 합계가
     * "완성값"처럼 보인다. 기본은 false(skip)이고, 운영자가 "어쨌든 stale인 채로라도 새 calculator
     * 결과로 갱신하고 싶다"고 명시할 때만 true.
     */
    private boolean includePending = false;

    /**
     * marketPrice 갱신 정책.
     *
     * <ul>
     *   <li><b>SKIP</b>: marketPrice 손대지 않음 (사용자 명시 입력 보존). 기본값.</li>
     *   <li><b>RECOMPUTE_IF_ZERO</b>: 0 또는 null인 경우만 totalCost 기반으로 재계산.</li>
     *   <li><b>FORCE</b>: totalCost 기반으로 무조건 재계산 (사용자 입력 덮어씀 — 매우 신중).</li>
     * </ul>
     */
    private MarketPriceMode marketPriceMode = MarketPriceMode.SKIP;

    public enum MarketPriceMode { SKIP, RECOMPUTE_IF_ZERO, FORCE }
}
