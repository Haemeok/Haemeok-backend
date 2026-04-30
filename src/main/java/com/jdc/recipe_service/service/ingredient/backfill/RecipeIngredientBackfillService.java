package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientResolutionStatus;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientUnitResolver;
import com.jdc.recipe_service.service.ingredient.normalize.ParsedQuantity;
import com.jdc.recipe_service.service.ingredient.normalize.QuantityParser;
import com.jdc.recipe_service.service.ingredient.normalize.UnitNormalizer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 정규화 백필 — recipe_ingredients의 정규화 4개 필드만 채운다.
 *
 * <h3>채움 대상</h3>
 * <ul>
 *   <li>{@code amount_value}</li>
 *   <li>{@code ingredient_unit_id}</li>
 *   <li>{@code normalized_grams}</li>
 *   <li>{@code resolution_status}</li>
 * </ul>
 *
 * <h3>건드리지 않는 필드</h3>
 * raw_*, quantity, unit, customName, customUnit, custom*(가격/영양), ingredient_candidate_id —
 * raw 보존 정책을 코드 차원에서 강제 ({@link RecipeIngredient#applyNormalizationBackfill}이 이 4개만 받음).
 *
 * <h3>row별 결정 로직</h3>
 * <ol>
 *   <li>rawQuantityText 파싱 실패 또는 정성 수량("약간"/"적당량") → UNRESOLVED, 새 필드 null</li>
 *   <li>unit 매칭 실패 → PARTIAL, amount_value만 채움</li>
 *   <li>unit 매칭 성공 + edible_grams_per_unit 또는 grams_per_unit 사용 가능 → MAPPED, 4개 모두 채움</li>
 *   <li>unit 매칭은 됐으나 grams 정보 없음 → PARTIAL (amount + unit_id, grams=null)</li>
 * </ol>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeIngredientBackfillService {

    private final RecipeIngredientBackfillRepository backfillRepository;
    private final IngredientUnitRepository ingredientUnitRepository;
    private final QuantityParser quantityParser;
    private final UnitNormalizer unitNormalizer;
    private final IngredientUnitResolver unitResolver;

    /**
     * 한 chunk 처리. {@code @Transactional}로 chunk 단위 commit.
     *
     * @param stopOnError true면 row 처리 중 예외 발생 시 즉시 throw → chunk tx 전체 rollback →
     *                    호출자(Runner)가 loop 중단. false면 per-row catch 후 다음 row로 진행 (failed 카운트만 증가).
     */
    @Transactional
    public RecipeIngredientBackfillResult backfillChunk(long lastId, int batchSize,
                                                         boolean dryRun, boolean stopOnError) {
        List<RecipeIngredient> rows = backfillRepository.findNormalizationBackfillTargets(
                lastId, PageRequest.of(0, batchSize));
        if (rows.isEmpty()) return RecipeIngredientBackfillResult.empty();

        // ingredient_id별 IngredientUnit batch prefetch (LAZY 차단)
        Set<Long> ingredientIds = rows.stream()
                .map(r -> r.getIngredient().getId())
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<Long, List<IngredientUnit>> unitsByIngredientId = ingredientIds.isEmpty()
                ? Map.of()
                : ingredientUnitRepository.findAllByIngredientIdIn(ingredientIds).stream()
                        .filter(u -> u.getIngredient() != null)
                        .collect(Collectors.groupingBy(u -> u.getIngredient().getId()));

        RecipeIngredientBackfillResult.Accumulator acc = new RecipeIngredientBackfillResult.Accumulator();

        for (RecipeIngredient row : rows) {
            acc.incrementScanned();
            acc.recordLastId(row.getId());

            // CUSTOM은 target query에서 제외되지만 race/스키마 변경 대비 안전망
            if ("CUSTOM".equals(row.getResolutionStatus())) {
                acc.incrementCustomSkipped();
                continue;
            }

            BackfillDecision decision;
            try {
                decision = decide(row, unitsByIngredientId);
            } catch (RuntimeException e) {
                if (stopOnError) {
                    // chunk tx 전체 rollback 후 runner가 loop 중단 → 운영 안전 모드.
                    log.error("[IngredientNormalizationBackfill] row processing failed (stopOnError=true): id={}, error={}",
                            row.getId(), e.getMessage(), e);
                    throw e;
                }
                log.warn("[IngredientNormalizationBackfill] row processing failed (continue): id={}, error={}",
                        row.getId(), e.getMessage());
                acc.incrementFailed();
                continue;
            }

            acc.incrementByStatus(decision.status());

            if (!dryRun) {
                row.applyNormalizationBackfill(
                        decision.amountValue(),
                        decision.ingredientUnitId(),
                        decision.normalizedGrams(),
                        decision.status().name()
                );
                // JPA dirty checking으로 commit 시 update — explicit save 불필요
            }
        }

        return acc.build();
    }

    /**
     * 단일 row → BackfillDecision. service 내부 helper — 테스트에서 직접 검증 가능.
     */
    BackfillDecision decide(RecipeIngredient row, Map<Long, List<IngredientUnit>> unitsByIngredientId) {
        ParsedQuantity pq = quantityParser.parse(row.getRawQuantityText());

        // 정성 수량("약간") 또는 파싱 실패 → UNRESOLVED, 새 필드 모두 null
        if (!pq.hasAmount()) {
            return BackfillDecision.unresolved();
        }

        Ingredient ingredient = row.getIngredient();
        if (ingredient == null) {
            // target query가 ingredient_id NOT NULL을 보장하지만 race 방어
            return BackfillDecision.unresolved();
        }

        String normalizedUnit = unitNormalizer.normalize(row.getRawUnitText());
        List<IngredientUnit> candidates = unitsByIngredientId.getOrDefault(ingredient.getId(), List.of());
        IngredientUnit matched = unitResolver.resolve(normalizedUnit, candidates).orElse(null);

        // 단위 매칭 실패 → PARTIAL, amount_value만 채움
        if (matched == null) {
            return BackfillDecision.partial(pq.amount(), null, null);
        }

        // unit 매칭 성공 — grams 계산. edible_grams_per_unit 우선, 없으면 grams_per_unit fallback.
        BigDecimal perUnitGrams = matched.getEdibleGramsPerUnit() != null
                ? matched.getEdibleGramsPerUnit()
                : matched.getGramsPerUnit();
        if (perUnitGrams == null) {
            // unit row가 매칭됐지만 grams 데이터 없음 (이론상 NOT NULL이라 일어나지 않음, 방어)
            return BackfillDecision.partial(pq.amount(), matched.getId(), null);
        }

        BigDecimal grams = pq.amount().multiply(perUnitGrams).setScale(3, RoundingMode.HALF_UP);
        return BackfillDecision.mapped(pq.amount(), matched.getId(), grams);
    }

    /**
     * 한 row의 결정 결과. {@link RecipeIngredient#applyNormalizationBackfill}이 받는 4개 값.
     */
    record BackfillDecision(BigDecimal amountValue,
                            Long ingredientUnitId,
                            BigDecimal normalizedGrams,
                            IngredientResolutionStatus status) {

        static BackfillDecision unresolved() {
            return new BackfillDecision(null, null, null, IngredientResolutionStatus.UNRESOLVED);
        }

        static BackfillDecision partial(BigDecimal amountValue, Long unitId, BigDecimal grams) {
            return new BackfillDecision(amountValue, unitId, grams, IngredientResolutionStatus.PARTIAL);
        }

        static BackfillDecision mapped(BigDecimal amountValue, Long unitId, BigDecimal grams) {
            return new BackfillDecision(amountValue, unitId, grams, IngredientResolutionStatus.MAPPED);
        }
    }
}
