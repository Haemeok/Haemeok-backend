package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.service.ingredient.backfill.RecipeAggregateBackfillProperties.MarketPriceMode;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalcInputMapper;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationSummary;
import com.jdc.recipe_service.util.PricingUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Recipe aggregate 백필 — 기존 24만 row의 totalIngredientCost / totalCalories /
 * totalIngredientCount / nutrition / (정책에 따라) marketPrice를 새 calculator 기준으로 재계산.
 *
 * <h3>왜 필요한가</h3>
 * 정규화 백필({@link RecipeIngredientBackfillService})은 row 단의 4개 필드(amount_value /
 * ingredient_unit_id / normalized_grams / resolution_status)만 채운다. 그러나 검색/예산/영양 필터는
 * Recipe entity의 aggregate 컬럼을 본다 — row 단만 채워서는 검색/필터가 새 계산값을 반영하지 못한다.
 *
 * <h3>실행 순서</h3>
 * <ol>
 *   <li>먼저 정규화 backfill을 끝낸다 (row 단 정규화).</li>
 *   <li>그 다음 이 aggregate backfill을 실행한다 (Recipe 단 합산 재계산).</li>
 * </ol>
 *
 * <h3>chunk 처리 흐름</h3>
 * <ol>
 *   <li>Recipe id keyset chunk 조회 (lastId &gt; ?, ORDER BY id ASC, LIMIT batchSize)</li>
 *   <li>그 Recipe들의 RecipeIngredient batch fetch ({@code findByRecipeIdIn})</li>
 *   <li>ingredient_unit_id를 모아 IngredientUnit batch fetch (LAZY 차단)</li>
 *   <li>Recipe별로 RecipeIngredient → CalculationLineInput 변환 + summarize</li>
 *   <li>{@link Recipe} entity의 update*(...) 호출 — JPA dirty checking으로 commit 시 update</li>
 * </ol>
 *
 * <h3>변환 정책</h3>
 * {@link RecipeIngredientCalcInputMapper}를 사용해 read 단(DevRecipeDetailService)과 동일한 결정 —
 * candidate id != null인 row의 0을 신뢰하는 분기까지 포함. 두 곳이 mapper를 공유해야 detail 합산값과
 * recipe aggregate가 어긋나지 않는다.
 *
 * <h3>marketPrice 정책</h3>
 * 기본 SKIP — 사용자 명시 입력을 보존. 운영자가 RECOMPUTE_IF_ZERO 또는 FORCE를 명시 선택했을 때만 갱신.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeAggregateBackfillService {

    /** DevRecipeIngredientPersistService와 동일 — 신규 marketPrice 산출 시 기본 마진. */
    private static final int DEFAULT_MARGIN_PERCENT = 30;

    private final RecipeAggregateBackfillRepository recipeRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final IngredientUnitRepository ingredientUnitRepository;
    private final RecipeIngredientCalculator calculator;

    @Transactional
    public RecipeAggregateBackfillResult backfillChunk(long lastId, int batchSize,
                                                       boolean dryRun, boolean stopOnError,
                                                       MarketPriceMode marketPriceMode,
                                                       boolean includePending) {
        List<Recipe> recipes = recipeRepository.findAggregateBackfillTargets(
                lastId, PageRequest.of(0, batchSize));
        if (recipes.isEmpty()) return RecipeAggregateBackfillResult.empty();

        // 1) RecipeIngredient batch fetch
        List<Long> recipeIds = recipes.stream().map(Recipe::getId).toList();
        List<RecipeIngredient> allIngredients = recipeIngredientRepository.findByRecipeIdIn(recipeIds);

        // 2) ingredient_unit_id batch prefetch (LAZY 차단)
        Set<Long> unitIds = allIngredients.stream()
                .map(RecipeIngredient::getIngredientUnitId)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<Long, IngredientUnit> unitsById = unitIds.isEmpty()
                ? Map.of()
                : ingredientUnitRepository.findAllByIdIn(unitIds).stream()
                        .collect(Collectors.toMap(IngredientUnit::getId, u -> u, (a, b) -> a));

        // 3) recipe별 grouping (recipe.id 기준 — recipe 객체 reference로 그룹핑하면 detached/proxy
        //    혼합 시 위험. id로 안전하게.)
        Map<Long, List<RecipeIngredient>> ingredientsByRecipeId = allIngredients.stream()
                .filter(ri -> ri.getRecipe() != null)
                .collect(Collectors.groupingBy(ri -> ri.getRecipe().getId()));

        RecipeAggregateBackfillResult.Accumulator acc = new RecipeAggregateBackfillResult.Accumulator();

        for (Recipe recipe : recipes) {
            acc.incrementScanned();
            acc.recordLastId(recipe.getId());

            List<RecipeIngredient> ris = ingredientsByRecipeId.getOrDefault(recipe.getId(), List.of());

            try {
                if (ris.isEmpty()) {
                    // ingredient 없는 레시피 — totalIngredientCost/Count/nutrition 0으로 정규화 (idempotent)
                    if (!dryRun) {
                        applyZeroAggregate(recipe, marketPriceMode);
                    }
                    acc.incrementSkipped();
                    continue;
                }

                List<CalculationLineInput> calcInputs = ris.stream()
                        .map(ri -> RecipeIngredientCalcInputMapper.toInput(ri, unitsById))
                        .toList();
                CalculationSummary summary = calculator.summarize(calcInputs);
                int realCount = countRealIngredients(ris);

                // pending line이 있으면 합산이 불완전 — Recipe aggregate에는 pending 정보가 없어서
                // 검색/필터에서 stale 합계가 완성값처럼 보임. 기본 skip, includePending=true일 때만 강제.
                if (summary.pendingCount() > 0 && !includePending) {
                    log.debug("[RecipeAggregateBackfill] skip recipe id={} due to pending lines (pending={}, calculated={})",
                            recipe.getId(), summary.pendingCount(), summary.calculatedCount());
                    acc.incrementSkipped();
                    continue;
                }

                if (!dryRun) {
                    applySummaryToRecipe(recipe, summary, realCount, marketPriceMode);
                }
                acc.incrementRecalculated();
            } catch (RuntimeException e) {
                if (stopOnError) {
                    log.error("[RecipeAggregateBackfill] recipe processing failed (stopOnError=true): id={}, error={}",
                            recipe.getId(), e.getMessage(), e);
                    throw e;
                }
                log.warn("[RecipeAggregateBackfill] recipe processing failed (continue): id={}, error={}",
                        recipe.getId(), e.getMessage());
                acc.incrementFailed();
            }
        }

        return acc.build();
    }

    /**
     * Calculator summary + realCount를 Recipe entity에 적용.
     *
     * <p>JPA dirty checking으로 commit 시 update — explicit save 불필요.
     */
    private static void applySummaryToRecipe(Recipe recipe, CalculationSummary summary, int realCount,
                                              MarketPriceMode marketPriceMode) {
        long totalCost = summary.totalIngredientCost();
        // Recipe.totalIngredientCost는 Integer 타입. 24만 row × 평균값 기준 overflow 가능성 낮지만 방어적 cap.
        int totalCostInt = (int) Math.min(totalCost, Integer.MAX_VALUE);

        recipe.updateTotalIngredientCost(totalCostInt);
        recipe.updateTotalIngredientCount(realCount);
        recipe.updateNutrition(
                summary.totalProtein(),
                summary.totalCarbohydrate(),
                summary.totalFat(),
                summary.totalSugar(),
                summary.totalSodium(),
                summary.totalCalories()
        );
        applyMarketPrice(recipe, totalCostInt, marketPriceMode);
    }

    /** ingredient 없는 레시피 — 모든 aggregate를 0으로 idempotent하게. */
    private static void applyZeroAggregate(Recipe recipe, MarketPriceMode marketPriceMode) {
        recipe.updateTotalIngredientCost(0);
        recipe.updateTotalIngredientCount(0);
        recipe.updateNutrition(
                java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO,
                java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO
        );
        applyMarketPrice(recipe, 0, marketPriceMode);
    }

    private static void applyMarketPrice(Recipe recipe, int totalCost, MarketPriceMode mode) {
        switch (mode) {
            case SKIP -> {
                /* 사용자 명시 입력 보존 — 손대지 않음 */
            }
            case RECOMPUTE_IF_ZERO -> {
                Integer current = recipe.getMarketPrice();
                if (current == null || current <= 0) {
                    recipe.updateMarketPrice(deriveMarketPrice(totalCost));
                }
            }
            case FORCE -> recipe.updateMarketPrice(deriveMarketPrice(totalCost));
        }
    }

    private static int deriveMarketPrice(int totalCost) {
        if (totalCost <= 0) return 0;
        return PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT));
    }

    /**
     * DevRecipeIngredientPersistService.countRealIngredients와 동일 정책 — pantry(기본 양념) 마스터
     * 매칭은 카운트에서 제외. customName 라인은 모두 카운트 (pantry 식별 불가).
     */
    private static int countRealIngredients(List<RecipeIngredient> entities) {
        if (entities == null || entities.isEmpty()) return 0;
        return (int) entities.stream()
                .filter(ri -> ri.getIngredient() == null || !ri.getIngredient().isPantry())
                .count();
    }
}
