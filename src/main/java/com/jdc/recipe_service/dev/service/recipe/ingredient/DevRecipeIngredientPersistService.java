package com.jdc.recipe_service.dev.service.recipe.ingredient;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientCandidate;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import com.jdc.recipe_service.domain.repository.IngredientCandidateRepository;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepRepository;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientResolutionStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.service.ingredient.normalize.NormalizedLine;
import com.jdc.recipe_service.service.ingredient.normalize.RawIngredientInput;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationSummary;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.LineCalculation;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientNormalizer;
import com.jdc.recipe_service.service.ingredient.normalize.UnitNormalizer;
import com.jdc.recipe_service.util.PricingUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 ingredient persist 단일 진입점.
 *
 * <p>운영 {@code RecipeIngredientService.saveAll}을 대체하는 dev 전용 path. 1.4의 YouTube/AI
 * 추출 path도 같은 service를 재사용한다.
 *
 * <p><b>흐름</b>
 * <ol>
 *   <li>입력 DTO → RawIngredientInput 변환 (customByUser 플래그 포함)</li>
 *   <li>이름 lookup map(canonicalize → Ingredient) + ingredient_id별 IngredientUnit batch prefetch</li>
 *   <li>{@link RecipeIngredientNormalizer}로 normalize → {@link NormalizedLine} 리스트
 *       (MAPPED / PARTIAL / UNRESOLVED / CUSTOM 분류 + dedupe + C' bypass)</li>
 *   <li>각 NormalizedLine을 {@link CalculationLineInput}으로 변환 후 line별 calc + summary 산출</li>
 *   <li>{@link RecipeIngredient} entity로 dual-write 저장 (legacy + 신규 필드 모두 채움)</li>
 *   <li>Recipe entity aggregate 갱신: totalIngredientCost / totalIngredientCount /
 *       totalCalories / nutrition / marketPrice</li>
 * </ol>
 *
 * <p><b>호출자 책임</b>: 같은 트랜잭션 내에서 호출. 호출자는 보통 dev create/update에서 운영
 * createRecipeAndGenerateUrls/updateUserRecipe를 ingredients=[]로 부른 직후 이 service를 호출한다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeIngredientPersistService {

    /** 운영 RecipeService와 동일 — dev 자체 marketPrice 산출의 기본 마진. */
    private static final int DEFAULT_MARGIN_PERCENT = 30;

    private final IngredientRepository ingredientRepository;
    private final IngredientUnitRepository ingredientUnitRepository;
    private final IngredientCandidateRepository ingredientCandidateRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final RecipeIngredientNormalizer normalizer;
    private final RecipeIngredientCalculator calculator;
    private final UnitNormalizer unitNormalizer;

    /**
     * 신규 생성용 — 빈 상태에서 dual-write 저장.
     *
     * @param suppliedMarketPrice 사용자 제공 marketPrice (있으면 우선, 없으면 totalCost+margin으로 산정)
     */
    @Transactional
    public PersistResult persistAll(Recipe recipe,
                                    List<RecipeIngredientRequestDto> dtos,
                                    Integer suppliedMarketPrice) {
        return doPersist(recipe, dtos, suppliedMarketPrice);
    }

    /**
     * AI/YouTube 추출 path 전용 진입점 — 모든 입력에 customByUser=false 강제 후 persist.
     *
     * <p><b>이유</b>: AI/YouTube가 못 푼 재료는 시스템적 매칭 실패이지 사용자 의도 CUSTOM이 아니다.
     * customByUser=true가 잘못 박히면 정상 매칭 가능했던 재료가 영구 CUSTOM으로 빠져 후속
     * candidates 큐잉 batch에서도 후보화되지 않는다(spec). 그래서 진입점에서 명시 강제.
     *
     * <p>사용자 직접 입력 path(`POST /api/dev/recipes`)만 customByUser=true를 받아들인다.
     */
    @Transactional
    public PersistResult persistAllSystemSourced(Recipe recipe,
                                                  List<RecipeIngredientRequestDto> dtos,
                                                  Integer suppliedMarketPrice) {
        return doPersist(recipe, forceCustomByUserFalse(dtos), suppliedMarketPrice);
    }

    /**
     * AI/YouTube path용 — customByUser=true 또는 null이 박혀 있어도 false로 덮어쓴 신규 list 반환.
     * 원본 dto는 mutate하지 않는다 (호출자가 같은 dto를 다른 path에서 재사용 가능성).
     */
    private static List<RecipeIngredientRequestDto> forceCustomByUserFalse(List<RecipeIngredientRequestDto> dtos) {
        if (dtos == null) return null;
        return dtos.stream()
                .map(d -> {
                    if (d == null) return null;
                    if (Boolean.FALSE.equals(d.getCustomByUser()) || d.getCustomByUser() == null) return d;
                    // toBuilder 없으면 manual copy 필요. RecipeIngredientRequestDto는 @Builder 있고 toBuilder 없음.
                    return RecipeIngredientRequestDto.builder()
                            .name(d.getName())
                            .quantity(d.getQuantity())
                            .customPrice(d.getCustomPrice())
                            .customUnit(d.getCustomUnit())
                            .customCalories(d.getCustomCalories())
                            .customCarbohydrate(d.getCustomCarbohydrate())
                            .customProtein(d.getCustomProtein())
                            .customFat(d.getCustomFat())
                            .customSugar(d.getCustomSugar())
                            .customSodium(d.getCustomSodium())
                            .isEstimated(d.getIsEstimated())
                            .customByUser(false)
                            .build();
                })
                .toList();
    }

    /**
     * 수정용 — 기존 RecipeIngredient + step_ingredient FK 정리 후 재저장.
     *
     * <p>운영 update와 동일하게 step_ingredient → recipe_ingredient 순서로 cascade delete.
     */
    @Transactional
    public PersistResult replaceAll(Recipe recipe,
                                    List<RecipeIngredientRequestDto> dtos,
                                    Integer suppliedMarketPrice) {
        List<RecipeIngredient> existing = recipeIngredientRepository.findByRecipeId(recipe.getId());
        for (RecipeIngredient ri : existing) {
            recipeStepIngredientRepository.deleteByRecipeIngredientId(ri.getId());
        }
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush();
        return doPersist(recipe, dtos, suppliedMarketPrice);
    }

    private PersistResult doPersist(Recipe recipe,
                                    List<RecipeIngredientRequestDto> dtos,
                                    Integer suppliedMarketPrice) {
        if (dtos == null) dtos = List.of();

        // 1. 입력 → RawIngredientInput
        List<RawIngredientInput> rawInputs = dtos.stream()
                .filter(d -> d != null && d.getName() != null && !d.getName().isBlank())
                .map(DevRecipeIngredientPersistService::toRawInput)
                .toList();

        // 2. lookup maps
        Map<String, Ingredient> ingredientByLookupName = buildIngredientLookup(rawInputs);
        Map<Long, List<IngredientUnit>> unitsByIngredientId = buildUnitsLookup(ingredientByLookupName.values());

        // 3. normalize
        List<NormalizedLine> normalizedLines = normalizer.normalize(rawInputs, ingredientByLookupName, unitsByIngredientId);

        // 4. row별 처리: candidate 생성 (필요 시) → calc input 빌드 (candidate_id 포함) → entity build/save.
        // candidate를 entity 저장보다 먼저 INSERT 해야 RecipeIngredient.ingredient_candidate_id FK 연결 가능.
        List<RecipeIngredient> saved = new ArrayList<>(normalizedLines.size());
        List<CalculationLineInput> calcInputs = new ArrayList<>(normalizedLines.size());
        for (NormalizedLine line : normalizedLines) {
            Long candidateId = maybeCreateCandidate(line, recipe);
            CalculationLineInput calcInput = toCalculationInput(line, candidateId);
            calcInputs.add(calcInput);
            LineCalculation calc = calculator.calculate(calcInput);

            RecipeIngredient entity = buildEntity(recipe, line, candidateId, calc);
            saved.add(recipeIngredientRepository.save(entity));
        }
        CalculationSummary summary = calculator.summarize(calcInputs);

        // 6. Recipe aggregate 갱신
        int totalCost = (int) summary.totalIngredientCost();
        int realCount = countRealIngredients(saved);
        recipe.updateTotalIngredientCost(totalCost);
        recipe.updateTotalIngredientCount(realCount);
        recipe.updateNutrition(
                summary.totalProtein(),
                summary.totalCarbohydrate(),
                summary.totalFat(),
                summary.totalSugar(),
                summary.totalSodium(),
                summary.totalCalories()
        );

        // marketPrice — 사용자 제공값 우선, 없으면 totalCost + 기본 마진
        int marketPrice;
        if (suppliedMarketPrice != null && suppliedMarketPrice > 0) {
            marketPrice = suppliedMarketPrice;
        } else if (totalCost > 0) {
            marketPrice = PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT));
        } else {
            marketPrice = 0;
        }
        // marketPrice가 totalCost보다 낮으면 강제 조정 (운영 V1/V2 정책 동등)
        if (marketPrice > 0 && marketPrice < totalCost) {
            marketPrice = PricingUtil.applyMargin(totalCost, DEFAULT_MARGIN_PERCENT);
        }
        recipe.updateMarketPrice(marketPrice);

        return new PersistResult(saved, summary, totalCost, realCount, marketPrice);
    }

    /**
     * Step ingredient FK 재연결 (dev empty-then-save 후처리).
     *
     * <p>운영 호출 시 step.ingredients=[]로 빈 리스트를 넘기므로 운영의
     * {@link com.jdc.recipe_service.service.RecipeStepService#saveAll}/{@code updateStepsFromUser}
     * 가 step ingredient를 만들지 않는다. 이 메서드가 dev persist 직후에 원본 step 데이터로
     * 다시 RecipeStepIngredient를 만들어 step→recipe_ingredient FK를 연결한다.
     *
     * <p><b>3-tier 매칭 정책</b>: dev normalizer는 같은 (name, unit) 조합도 두 row로 보존하는
     * 케이스가 있다 (예: "마늘 약간 쪽" MAPPED + "마늘 3쪽" PARTIAL bypass — 같은 name+unit이지만
     * amount 합산 불가). 그래서 unit만으로는 disambiguate 못 함:
     * <ol>
     *   <li>1차: {@code (canonicalize(name), normalize(unit), normalize(quantity))} 정확 매칭 — 최우선</li>
     *   <li>2차: {@code (canonicalize(name), normalize(unit))} candidates. <b>후보가 정확히 1개일 때만</b>
     *       그 row 사용 (운영의 단일 row 시대 호환).</li>
     *   <li>3차: name으로만 후보 검색. <b>후보가 정확히 1개일 때만</b> 사용 (legacy 호환).</li>
     *   <li>매칭 실패: {@link ErrorCode#INGREDIENT_NOT_FOUND} throw — 잘못된 row에 묶이느니 fail-fast.</li>
     * </ol>
     *
     * <p>이름은 {@link RecipeIngredientNormalizer#canonicalizeName}, 단위는 {@link UnitNormalizer},
     * 수량은 trim+lowercase 정규화 — 1.1 helper와 동일 키 정책 사용.
     *
     * <p>{@code @Transactional}: 1.4에서 단독 호출 시에도 read/write가 한 트랜잭션에 묶이도록.
     */
    @Transactional
    public void linkStepIngredients(Recipe recipe, List<RecipeStepRequestDto> originalSteps) {
        if (originalSteps == null || originalSteps.isEmpty()) return;

        List<RecipeIngredient> savedIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        if (savedIngredients == null) savedIngredients = List.of();

        // 3개 인덱스: (name+unit+qty), (name+unit), (name)
        Map<NameUnitQtyKey, List<RecipeIngredient>> byNameUnitQty = new HashMap<>();
        Map<NameUnitKey, List<RecipeIngredient>> byNameUnit = new HashMap<>();
        Map<String, List<RecipeIngredient>> byName = new HashMap<>();

        for (RecipeIngredient ri : savedIngredients) {
            for (String nameKey : nameKeys(ri)) {
                addUnique(byName, nameKey, ri);
                for (String unitKey : unitKeys(ri)) {
                    addUnique(byNameUnit, new NameUnitKey(nameKey, unitKey), ri);
                    for (String qtyKey : quantityKeys(ri)) {
                        addUnique(byNameUnitQty, new NameUnitQtyKey(nameKey, unitKey, qtyKey), ri);
                    }
                }
            }
        }

        Map<Integer, RecipeStep> stepByNumber = recipeStepRepository
                .findByRecipeIdOrderByStepNumber(recipe.getId()).stream()
                .collect(Collectors.toMap(RecipeStep::getStepNumber, java.util.function.Function.identity(),
                        (a, b) -> a));

        for (RecipeStepRequestDto stepDto : originalSteps) {
            if (stepDto == null || stepDto.getIngredients() == null || stepDto.getIngredients().isEmpty()) {
                continue;
            }
            RecipeStep step = stepByNumber.get(stepDto.getStepNumber());
            if (step == null) continue;

            for (RecipeStepIngredientRequestDto stepIngDto : stepDto.getIngredients()) {
                if (stepIngDto == null || stepIngDto.getName() == null || stepIngDto.getName().isBlank()) {
                    continue;
                }

                RecipeIngredient ri = resolveStepIngredient(stepIngDto, byNameUnitQty, byNameUnit, byName);
                if (ri == null) {
                    throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, stepIngDto.getName());
                }

                RecipeStepIngredient rsi = StepIngredientMapper.toEntity(stepIngDto, step, ri);
                recipeStepIngredientRepository.save(rsi);
            }
        }
    }

    /**
     * 3-tier resolution: name+unit+qty (정확) → name+unit (1 candidate) → name (1 candidate).
     */
    private RecipeIngredient resolveStepIngredient(RecipeStepIngredientRequestDto dto,
                                                   Map<NameUnitQtyKey, List<RecipeIngredient>> byNameUnitQty,
                                                   Map<NameUnitKey, List<RecipeIngredient>> byNameUnit,
                                                   Map<String, List<RecipeIngredient>> byName) {
        String canonicalName = RecipeIngredientNormalizer.canonicalizeName(dto.getName());
        if (canonicalName.isEmpty()) return null;

        String normalizedUnit = unitNormalizer.normalize(dto.getCustomUnit());
        String normalizedQty = canonicalizeQuantity(dto.getQuantity());

        // Tier 1: name + unit + quantity (가장 구체적). 후보 2개 이상이면 normalizer/dedupe가 비정상
        // — fail-fast 일관성을 위해 1개일 때만 받아들이고 그 외엔 다음 tier로 흘려 결국 throw.
        if (!normalizedUnit.isEmpty() && !normalizedQty.isEmpty()) {
            List<RecipeIngredient> hits = byNameUnitQty.getOrDefault(
                    new NameUnitQtyKey(canonicalName, normalizedUnit, normalizedQty), List.of());
            if (hits.size() == 1) return hits.get(0);
        }

        // Tier 2: name + unit (1 candidate only)
        if (!normalizedUnit.isEmpty()) {
            List<RecipeIngredient> candidates = byNameUnit.getOrDefault(
                    new NameUnitKey(canonicalName, normalizedUnit), List.of());
            if (candidates.size() == 1) return candidates.get(0);
        }

        // Tier 3: name only (1 candidate only)
        List<RecipeIngredient> candidates = byName.getOrDefault(canonicalName, List.of());
        if (candidates.size() == 1) return candidates.get(0);

        return null;  // ambiguous or not found — 호출자가 throw
    }

    private static <K> void addUnique(Map<K, List<RecipeIngredient>> index, K key, RecipeIngredient ri) {
        List<RecipeIngredient> bucket = index.computeIfAbsent(key, k -> new ArrayList<>());
        if (!bucket.contains(ri)) bucket.add(ri);
    }

    /**
     * 수량 정규화 — trim + lowercase. 입력 측("3", "약간")과 저장 측 rawQuantityText/quantity가
     * 동일 함수로 정규화되어야 정확 매칭이 작동한다.
     */
    private static String canonicalizeQuantity(String raw) {
        if (raw == null) return "";
        return raw.trim().toLowerCase(java.util.Locale.ROOT);
    }

    /**
     * RecipeIngredient의 매칭용 이름 candidates (canonicalize 적용).
     *
     * <p>raw 보존 시대에는 rawName이 가장 안정적이지만 frontend가 V2 호환으로 master.name을 보낼 수도
     * 있어 customName / ingredient.name도 등록.
     */
    private static List<String> nameKeys(RecipeIngredient ri) {
        List<String> keys = new ArrayList<>(3);
        addCanonicalNameIfPresent(keys, ri.getRawName());
        addCanonicalNameIfPresent(keys, ri.getCustomName());
        if (ri.getIngredient() != null) {
            addCanonicalNameIfPresent(keys, ri.getIngredient().getName());
        }
        return keys;
    }

    private static void addCanonicalNameIfPresent(List<String> keys, String name) {
        if (name == null || name.isBlank()) return;
        String key = RecipeIngredientNormalizer.canonicalizeName(name);
        if (!key.isEmpty() && !keys.contains(key)) keys.add(key);
    }

    /**
     * RecipeIngredient의 매칭용 unit candidates (UnitNormalizer 적용).
     */
    private List<String> unitKeys(RecipeIngredient ri) {
        List<String> keys = new ArrayList<>(3);
        addNormalizedUnitIfPresent(keys, ri.getRawUnitText());
        addNormalizedUnitIfPresent(keys, ri.getCustomUnit());
        addNormalizedUnitIfPresent(keys, ri.getUnit());
        return keys;
    }

    private void addNormalizedUnitIfPresent(List<String> keys, String unit) {
        if (unit == null || unit.isBlank()) return;
        String key = unitNormalizer.normalize(unit);
        if (!key.isEmpty() && !keys.contains(key)) keys.add(key);
    }

    /**
     * RecipeIngredient의 매칭용 quantity candidates.
     *
     * <p>raw 보존 시대에는 rawQuantityText가 가장 정확한 매칭 키 — normalizer가 합산 시
     * rawQuantityText를 합계로 갱신하므로 이 값이 사용자가 보는 표기와 일치. legacy quantity는
     * fallback (대부분 rawQuantityText와 동일하지만 dual-write 시점 구조상 분리되어 있음).
     */
    private static List<String> quantityKeys(RecipeIngredient ri) {
        List<String> keys = new ArrayList<>(2);
        addCanonicalQtyIfPresent(keys, ri.getRawQuantityText());
        addCanonicalQtyIfPresent(keys, ri.getQuantity());
        return keys;
    }

    private static void addCanonicalQtyIfPresent(List<String> keys, String qty) {
        if (qty == null || qty.isBlank()) return;
        String key = canonicalizeQuantity(qty);
        if (!key.isEmpty() && !keys.contains(key)) keys.add(key);
    }

    private record NameUnitKey(String name, String unit) {}
    private record NameUnitQtyKey(String name, String unit, String quantity) {}

    // ─── 변환 helpers ────────────────────────────────────────────────────────

    private static RawIngredientInput toRawInput(RecipeIngredientRequestDto dto) {
        boolean customByUser = Boolean.TRUE.equals(dto.getCustomByUser());
        return new RawIngredientInput(
                dto.getName(),
                dto.getQuantity(),
                dto.getCustomUnit(),
                customByUser,
                dto.getCustomCalories(),
                dto.getCustomPrice() == null ? null : dto.getCustomPrice().intValue(),
                dto.getCustomCarbohydrate(),
                dto.getCustomProtein(),
                dto.getCustomFat(),
                dto.getCustomSugar(),
                dto.getCustomSodium(),
                null  // customLink — operational 입력 path에서는 사용 안 함 (AI/YouTube path가 1.4에서 채울 가능)
        );
    }

    /**
     * NormalizedLine + candidateId → CalculationLineInput.
     * candidateId가 path 3 진입 fallback signal로 작동.
     */
    private static CalculationLineInput toCalculationInput(NormalizedLine line, Long candidateId) {
        return new CalculationLineInput(
                line.status(),
                line.normalizedGrams(),
                line.resolvedIngredient(),
                line.unitOwnerIngredient(),
                candidateId,
                line.customCalorie(),
                line.customPrice(),
                line.customCarbohydrate(),
                line.customProtein(),
                line.customFat(),
                line.customSugar(),
                line.customSodium()
        );
    }

    /**
     * NormalizedLine + LineCalculation → RecipeIngredient entity (dual-write).
     *
     * <p><b>매핑 규칙</b>
     * <ul>
     *   <li>raw_*, amount_value, normalized_grams, ingredient_unit_id, resolution_status — 모든 status 공통</li>
     *   <li>quantity/unit (legacy 표시) — rawQuantityText/rawUnitText 그대로</li>
     *   <li>MAPPED: ingredient = resolved, customName/customUnit/custom* 비움</li>
     *   <li>PARTIAL/UNRESOLVED/CUSTOM (ingredient_id=null): customName=rawName, customUnit=rawUnitText,
     *       custom* = calc 결과(included) 또는 ZERO(pending)</li>
     *   <li>price (Integer line 가격) — calc included면 그 값, 아니면 0</li>
     * </ul>
     */
    /**
     * Row 종류별 custom* 정책 (Phase 3 dev V3 override):
     *
     * <ul>
     *   <li><b>MAPPED (canonical path 1)</b>: ingredient hit + unit hit. custom*는 모두 ZERO/null.
     *       AI가 custom*를 줘도 무시 (canonical 우선). V2 read는 ingredient.* 사용 (isCustom=false).</li>
     *   <li><b>PARTIAL bypass (canonical path 2)</b>: ingredient_id=null + unit_id 보존. unit→ingredient
     *       경로로 calc 가능. customName=raw, custom*=calc 결과 (V2 read는 customCalorie를 봄 → 호환).</li>
     *   <li><b>PARTIAL ingredient-hit unit-miss (AI fallback)</b>: ingredient_id 있고 unit miss.
     *       candidate(UNIT) 생성됨. customName/customUnit=raw, custom*=NormalizedLine 입력값 line-total
     *       (AI가 line-total로 보낸 값 그대로 보존). 후속 batch가 unit seed로 path 1으로 승격하면
     *       custom*는 그대로 남아도 path 1 우선이라 무시됨.</li>
     *   <li><b>UNRESOLVED (AI fallback)</b>: ingredient miss. candidate(INGREDIENT) 생성됨.
     *       custom*=AI 입력값. 후속 batch가 ingredient seed로 path 1/2 승격 가능.</li>
     *   <li><b>CUSTOM</b>: customByUser=true. candidate 없음. custom*=사용자 입력. status signal로 path 3.</li>
     * </ul>
     *
     * <p><b>핵심 규칙</b>: canonical row(MAPPED + bypass)에서 AI가 보낸 custom*는 무시. fallback row
     * (ingredient hit + unit miss / UNRESOLVED / CUSTOM)에서만 line-total custom* 보존. fallback signal
     * (candidate_id 또는 status=CUSTOM)이 calculator path 3 진입 가드.
     */
    private static RecipeIngredient buildEntity(Recipe recipe, NormalizedLine line, Long candidateId, LineCalculation calc) {
        int linePrice = calc.included() ? (int) Math.min(calc.price(), Integer.MAX_VALUE) : 0;

        RecipeIngredient.RecipeIngredientBuilder builder = RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(line.resolvedIngredient())
                .quantity(line.rawQuantityText())
                .unit(line.rawUnitText())
                .ingredientUnitId(line.ingredientUnitId())
                .ingredientCandidateId(candidateId)
                .rawName(line.rawName())
                .rawQuantityText(line.rawQuantityText())
                .rawUnitText(line.rawUnitText())
                .amountValue(line.amountValue())
                .normalizedGrams(line.normalizedGrams())
                .resolutionStatus(line.status().name())
                .price(linePrice);

        if (line.status() == IngredientResolutionStatus.MAPPED) {
            // canonical path 1 — V2 read는 ingredient.* 사용. AI custom*는 무시.
            builder.customName(null)
                    .customUnit(null)
                    .customPrice(0)
                    .customCalorie(BigDecimal.ZERO)
                    .customCarbohydrate(BigDecimal.ZERO)
                    .customProtein(BigDecimal.ZERO)
                    .customFat(BigDecimal.ZERO)
                    .customSugar(BigDecimal.ZERO)
                    .customSodium(BigDecimal.ZERO);
        } else if (line.isBypassRow()) {
            // canonical path 2 (C' bypass). V2 read는 customName-path 사용 → calc 결과를 박아 호환.
            // AI custom*는 canonical 우선이라 무시되지만, V2 isCustom path가 customCalorie를 보므로
            // path 2 calc 결과를 채워 V2 read도 정확히 표시.
            builder.customName(line.rawName())
                    .customUnit(line.rawUnitText())
                    .customPrice(calc.included() ? linePrice : 0)
                    .customCalorie(calc.included() ? calc.calories() : BigDecimal.ZERO)
                    .customCarbohydrate(calc.included() ? calc.carbohydrate() : BigDecimal.ZERO)
                    .customProtein(calc.included() ? calc.protein() : BigDecimal.ZERO)
                    .customFat(calc.included() ? calc.fat() : BigDecimal.ZERO)
                    .customSugar(calc.included() ? calc.sugar() : BigDecimal.ZERO)
                    .customSodium(calc.included() ? calc.sodium() : BigDecimal.ZERO);
        } else {
            // PARTIAL ingredient-hit-unit-miss / UNRESOLVED / CUSTOM — AI/사용자 입력 line-total 보존.
            // candidate_id 또는 status=CUSTOM이 calculator path 3 진입 signal — 의미 있는 fallback임을 코드로 잠금.
            //
            // **누락값은 null 그대로 저장** (ZERO/0으로 채우지 않음).
            // - "AI가 customFat=0을 정상 line-total로 명시" vs "필드를 빠뜨려 누락"을 read 경로에서 구분 가능해야
            //   detail 합산이 부분 누락을 0으로 흡수하지 않고 pending으로 정확히 떨어진다.
            // - read 단(DevRecipeDetailService)이 candidate id를 trust signal로 보고, 0을 신뢰할지/legacy default로
            //   보호할지 분기한다. write가 누락을 0으로 박으면 그 분기가 무력화됨.
            // - Builder의 @Builder.Default(BigDecimal.ZERO/0)는 setter 호출 안 했을 때만 적용되므로,
            //   명시적으로 .customXxx(null) 호출하면 null이 저장된다.
            builder.customName(line.rawName())
                    .customUnit(line.rawUnitText())
                    .customPrice(line.customPrice())
                    .customCalorie(line.customCalorie())
                    .customCarbohydrate(line.customCarbohydrate())
                    .customProtein(line.customProtein())
                    .customFat(line.customFat())
                    .customSugar(line.customSugar())
                    .customSodium(line.customSodium());
        }

        return builder.build();
    }

    // ─── candidate 생성 ──────────────────────────────────────────────────────

    /**
     * Row 종류에 따라 IngredientCandidate INSERT 후 id 반환. candidate 불필요 row는 null 반환.
     *
     * <ul>
     *   <li>UNRESOLVED (이름 매칭 실패) → candidate_type=INGREDIENT</li>
     *   <li>PARTIAL with ingredient hit + unit miss (ingredient_id 있고 unit_id null) → candidate_type=UNIT.
     *       검수자가 unit seed를 추가하면 후속 batch에서 path 1으로 승격.</li>
     *   <li>PARTIAL true canonical bypass (ingredient_id=null + unit_id 보존) → null. unit→ingredient 경로로
     *       canonical 즉시 계산 가능 (calculator path 2). UNIT seed 추가로 풀릴 게 없음.</li>
     *   <li>PARTIAL duplicate unit-miss bypass (ingredient_id=null + unit_id=null + unitOwnerIngredient!=null)
     *       → candidate_type=UNIT. 같은 ingredient의 또다른 unit miss라 unit seed 추가로 path 1 승격 가능.
     *       (normalizer dedupe가 같은 ingredient 두 row 중 한쪽 unit miss를 ingredient_id=null bypass로 두는 케이스)</li>
     *   <li>MAPPED → null (이미 완전 해결)</li>
     *   <li>CUSTOM (customByUser=true) → null. 사용자 의도 final state.</li>
     * </ul>
     */
    private Long maybeCreateCandidate(NormalizedLine line, Recipe recipe) {
        if (line.customByUser()) return null;
        if (line.status() == IngredientResolutionStatus.MAPPED) return null;

        if (line.status() == IngredientResolutionStatus.UNRESOLVED) {
            return createIngredientCandidate(line, recipe);
        }

        if (line.status() == IngredientResolutionStatus.PARTIAL) {
            // True canonical bypass(unit_id 보존)는 path 2로 즉시 계산 가능 — candidate 불필요.
            if (line.isBypassRow()) return null;

            // 나머지 PARTIAL: ingredient hit + unit miss / duplicate unit-miss bypass.
            // resolvedIngredient(직접 hit) 또는 unitOwnerIngredient(같은 이름 dedupe로 unit owner 복원) 중 가능한 것 사용.
            Ingredient ingredient = line.resolvedIngredient() != null
                    ? line.resolvedIngredient()
                    : line.unitOwnerIngredient();
            if (ingredient == null) return null;  // 방어적 — 이론상 도달 불가
            return createUnitCandidate(line, recipe, ingredient);
        }

        return null;
    }

    private Long createUnitCandidate(NormalizedLine line, Recipe recipe, Ingredient ingredient) {
        IngredientCandidate cand = IngredientCandidate.builder()
                .candidateType("UNIT")
                .ingredient(ingredient)
                .rawName(line.rawName())
                .rawUnitText(line.rawUnitText())
                .proposedUnitLabelKo(line.rawUnitText())  // raw 그대로 제안. 검수자가 정규화.
                .sourceRecipeId(recipe.getId())
                .status("PENDING")
                .build();
        return ingredientCandidateRepository.save(cand).getId();
    }

    private Long createIngredientCandidate(NormalizedLine line, Recipe recipe) {
        IngredientCandidate cand = IngredientCandidate.builder()
                .candidateType("INGREDIENT")
                .ingredient(null)
                .rawName(line.rawName())
                .rawUnitText(line.rawUnitText())
                .proposedNameKo(line.rawName())          // raw 그대로 제안
                .proposedUnitLabelKo(line.rawUnitText())
                .sourceRecipeId(recipe.getId())
                .status("PENDING")
                .build();
        return ingredientCandidateRepository.save(cand).getId();
    }

    /**
     * 운영 calculateRealIngredientCount 동등 — pantry(기본 양념) 마스터 매칭은 카운트에서 제외.
     * customName 라인은 모두 카운트 (pantry 식별 불가).
     */
    private static int countRealIngredients(List<RecipeIngredient> entities) {
        if (entities == null || entities.isEmpty()) return 0;
        return (int) entities.stream()
                .filter(ri -> ri.getIngredient() == null || !ri.getIngredient().isPantry())
                .count();
    }

    // ─── lookup map 빌드 (1.1의 IngredientUnitResolver invariant: caller가 prefetch) ─────

    /**
     * 입력의 모든 raw_name에 대해 ingredient lookup map 구성.
     *
     * <p>이름 매칭 키는 {@link RecipeIngredientNormalizer#canonicalizeName(String)} 결과.
     * "청 양 고추" / "청양고추" 등 공백 변형이 같은 키로 흡수됨.
     *
     * <p>{@code findAllByIsActiveTrue()}로 비활성 마스터를 제외 — 비활성 ingredient가 매칭되어
     * dual-write 됐다가 운영 read에서 누락되면 데이터 inconsistency가 생긴다.
     */
    private Map<String, Ingredient> buildIngredientLookup(List<RawIngredientInput> rawInputs) {
        Set<String> canonicalNames = rawInputs.stream()
                .filter(r -> !Boolean.TRUE.equals(r.customByUser()))  // CUSTOM은 lookup 불필요
                .map(RawIngredientInput::rawName)
                .filter(n -> n != null && !n.isBlank())
                .map(RecipeIngredientNormalizer::canonicalizeName)
                .filter(s -> !s.isEmpty())
                .collect(Collectors.toSet());

        if (canonicalNames.isEmpty()) return Map.of();

        // alias는 v1에서 사용 안 함 — name 기준 일대일. is_active=true 만 가져옴.
        Map<String, Ingredient> result = new HashMap<>();
        for (Ingredient ing : ingredientRepository.findAllByIsActiveTrue()) {
            String key = RecipeIngredientNormalizer.canonicalizeName(ing.getName());
            if (canonicalNames.contains(key)) {
                result.putIfAbsent(key, ing);
            }
        }
        return result;
    }

    /**
     * resolved ingredient들의 IngredientUnit batch prefetch (LAZY 차단).
     *
     * <p>{@code findAllByIngredientIdIn(...)} + {@code @EntityGraph(ingredient)}로 필요한 unit만
     * 가져와 grouping. findAll() 대비 write path 누적 비용 절감.
     */
    private Map<Long, List<IngredientUnit>> buildUnitsLookup(java.util.Collection<Ingredient> ingredients) {
        Set<Long> ingredientIds = ingredients.stream()
                .map(Ingredient::getId)
                .filter(java.util.Objects::nonNull)
                .collect(Collectors.toSet());
        if (ingredientIds.isEmpty()) return Map.of();

        return ingredientUnitRepository.findAllByIngredientIdIn(ingredientIds).stream()
                .filter(u -> u.getIngredient() != null)
                .collect(Collectors.groupingBy(u -> u.getIngredient().getId()));
    }

    // ─── 결과 record ─────────────────────────────────────────────────────────

    /**
     * persist 결과. 호출자가 응답 DTO에 summary를 노출하거나 후속 처리에 사용.
     */
    public record PersistResult(
            List<RecipeIngredient> savedEntities,
            CalculationSummary summary,
            int totalIngredientCost,
            int totalIngredientCount,
            int marketPrice
    ) {}
}
