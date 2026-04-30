package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientResolutionStatus;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientUnitResolver;
import com.jdc.recipe_service.service.ingredient.normalize.QuantityParser;
import com.jdc.recipe_service.service.ingredient.normalize.UnitNormalizer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import com.jdc.recipe_service.service.ingredient.normalize.ParsedQuantity;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.BDDMockito.given;

/**
 * 2차 백필 결정 로직 단위 테스트.
 *
 * <p>각 row가 어떤 status로 결정되는지, 어떤 필드가 채워지는지 잠금. dry-run 동작도 검증.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RecipeIngredientBackfillServiceTest {

    @Mock RecipeIngredientBackfillRepository backfillRepository;
    @Mock IngredientUnitRepository ingredientUnitRepository;

    RecipeIngredientBackfillService service;
    Recipe recipe;

    @BeforeEach
    void setUp() {
        QuantityParser quantityParser = new QuantityParser();
        UnitNormalizer unitNormalizer = new UnitNormalizer();
        IngredientUnitResolver unitResolver = new IngredientUnitResolver();
        service = new RecipeIngredientBackfillService(
                backfillRepository, ingredientUnitRepository,
                quantityParser, unitNormalizer, unitResolver);
        recipe = Recipe.builder().id(1L).title("Test").build();
    }

    private static Ingredient garlic() {
        return Ingredient.builder().id(1L).name("마늘").build();
    }

    private static IngredientUnit unit(Long id, Ingredient ing, String label, String edibleGrams) {
        return IngredientUnit.builder()
                .id(id).ingredient(ing)
                .unitLabelKo(label).normalizedUnitLabel(label)
                .gramsPerUnit(new BigDecimal(edibleGrams))
                .edibleGramsPerUnit(new BigDecimal(edibleGrams))
                .isDefault(false)
                .build();
    }

    private static RecipeIngredient row(Long id, Ingredient ing, String rawName, String rawQty, String rawUnit,
                                          String currentStatus) {
        return RecipeIngredient.builder()
                .id(id).recipe(Recipe.builder().id(1L).build())
                .ingredient(ing)
                .quantity(rawQty).unit(rawUnit)
                .rawName(rawName).rawQuantityText(rawQty).rawUnitText(rawUnit)
                .resolutionStatus(currentStatus)
                .build();
    }

    @Test
    @DisplayName("MAPPED 결정: 정상 수량 + 단위 매칭 + grams 계산 → 4개 필드 모두 채움")
    void backfill_mapped_fillsAllFour() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r = row(101L, garlic, "마늘", "3", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, false, false);

        assertThat(result.scanned()).isEqualTo(1);
        assertThat(result.mapped()).isEqualTo(1);
        assertThat(r.getAmountValue()).isEqualByComparingTo("3");
        assertThat(r.getIngredientUnitId()).isEqualTo(10L);
        assertThat(r.getNormalizedGrams()).isEqualByComparingTo("15.000");
        assertThat(r.getResolutionStatus()).isEqualTo("MAPPED");
    }

    @Test
    @DisplayName("PARTIAL 결정: 수량 파싱 OK + 단위 매칭 실패 → amount_value만 채움, unit_id/grams=null")
    void backfill_partial_unitMissOnly_amountFilled() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        // raw unit "봉지" — master에 없음
        RecipeIngredient r = row(102L, garlic, "마늘", "1", "봉지", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        service.backfillChunk(0L, 100, false, false);

        assertThat(r.getAmountValue()).isEqualByComparingTo("1");
        assertThat(r.getIngredientUnitId()).isNull();
        assertThat(r.getNormalizedGrams()).isNull();
        assertThat(r.getResolutionStatus()).isEqualTo("PARTIAL");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: UNRESOLVED 결정 — 정성 수량 \"약간\" → 4개 필드 모두 null/UNRESOLVED")
    void backfill_unresolved_specialQuantity_allNull() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r = row(103L, garlic, "마늘", "약간", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        service.backfillChunk(0L, 100, false, false);

        assertThat(r.getAmountValue())
                .as("\"약간\"은 amount=null — 0이 아니라 의도적 \"수량 미정\"")
                .isNull();
        assertThat(r.getIngredientUnitId()).isNull();
        assertThat(r.getNormalizedGrams()).isNull();
        assertThat(r.getResolutionStatus()).isEqualTo("UNRESOLVED");
    }

    @Test
    @DisplayName("UNRESOLVED 결정: 수량 파싱 실패 (이상한 문자열) → UNRESOLVED")
    void backfill_unresolved_unparseable() {
        Ingredient garlic = garlic();
        RecipeIngredient r = row(104L, garlic, "마늘", "abc!@#", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        service.backfillChunk(0L, 100, false, false);

        assertThat(r.getResolutionStatus()).isEqualTo("UNRESOLVED");
        assertThat(r.getAmountValue()).isNull();
    }

    @Test
    @DisplayName("CUSTOM은 service 안전망에서 skip — entity 필드 변경 안 됨")
    void backfill_custom_skipped() {
        Ingredient garlic = garlic();
        // CUSTOM은 보통 target query에서 제외되지만 race 대비 service 안전망 체크
        RecipeIngredient r = row(105L, garlic, "마늘", "3", "쪽", "CUSTOM");
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, false, false);

        assertThat(result.customSkipped()).isEqualTo(1);
        assertThat(result.mapped()).isZero();
        // entity 변경 없음
        assertThat(r.getResolutionStatus()).isEqualTo("CUSTOM");
        assertThat(r.getAmountValue()).isNull();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: dry-run=true → entity 필드 변경 없음, 통계만 누적")
    void backfill_dryRun_doesNotMutateEntity() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r = row(101L, garlic, "마늘", "3", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, /* dryRun= */ true, false);

        // 통계는 계산됨
        assertThat(result.mapped()).isEqualTo(1);
        // entity는 변경 안 됨
        assertThat(r.getAmountValue()).as("dry-run에서는 entity write 없음").isNull();
        assertThat(r.getIngredientUnitId()).isNull();
        assertThat(r.getNormalizedGrams()).isNull();
        assertThat(r.getResolutionStatus()).isNull();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: applyNormalizationBackfill은 raw_*/quantity/unit/customName/customUnit/custom* 필드 건드리지 않음")
    void backfill_doesNotTouchPreservedFields() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽")
                .rawName("마늘 (큰 알)").rawQuantityText("3").rawUnitText("쪽")
                .customName("preserved-custom").customUnit("preserved-unit")
                .customPrice(7777).customCalorie(new BigDecimal("999"))
                .resolutionStatus(null)
                .build();
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        service.backfillChunk(0L, 100, false, false);

        // 4개 정규화 필드는 갱신
        assertThat(r.getAmountValue()).isEqualByComparingTo("3");
        assertThat(r.getResolutionStatus()).isEqualTo("MAPPED");
        // 그 외 모든 필드는 보존
        assertThat(r.getQuantity()).isEqualTo("3");
        assertThat(r.getUnit()).isEqualTo("쪽");
        assertThat(r.getRawName()).isEqualTo("마늘 (큰 알)");
        assertThat(r.getRawQuantityText()).isEqualTo("3");
        assertThat(r.getRawUnitText()).isEqualTo("쪽");
        assertThat(r.getCustomName()).isEqualTo("preserved-custom");
        assertThat(r.getCustomUnit()).isEqualTo("preserved-unit");
        assertThat(r.getCustomPrice()).isEqualTo(7777);
        assertThat(r.getCustomCalorie()).isEqualByComparingTo("999");
    }

    @Test
    @DisplayName("Result.lastProcessedId는 chunk 내 최대 id로 설정됨 (다음 chunk의 startId)")
    void backfill_lastProcessedId_isMaxIdInChunk() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r1 = row(101L, garlic, "마늘", "3", "쪽", null);
        RecipeIngredient r2 = row(150L, garlic, "마늘", "5", "쪽", null);
        RecipeIngredient r3 = row(200L, garlic, "마늘", "1", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r1, r2, r3));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, false, false);

        assertThat(result.scanned()).isEqualTo(3);
        assertThat(result.lastProcessedId())
                .as("다음 chunk의 lastId로 사용 — keyset 페이징 진행")
                .isEqualTo(200L);
    }

    @Test
    @DisplayName("빈 chunk: scanned=0, lastProcessedId=-1 (Runner의 종료 신호)")
    void backfill_emptyChunk_signalsTermination() {
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of());

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, false, false);

        assertThat(result.scanned()).isZero();
        assertThat(result.lastProcessedId()).isEqualTo(-1L);
    }

    @Test
    @DisplayName("PARTIAL: unit 매칭 OK이지만 edible_grams_per_unit이 null인 비정상 unit row → PARTIAL (amount+unit_id, grams=null)")
    void backfill_partial_unitMatchedButNoGrams() {
        Ingredient garlic = garlic();
        IngredientUnit brokenUnit = IngredientUnit.builder()
                .id(10L).ingredient(garlic)
                .unitLabelKo("쪽").normalizedUnitLabel("쪽")
                .gramsPerUnit(null).edibleGramsPerUnit(null)  // 이론상 NOT NULL이지만 방어
                .isDefault(false).build();
        RecipeIngredient r = row(101L, garlic, "마늘", "3", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(brokenUnit));

        service.backfillChunk(0L, 100, false, false);

        assertThat(r.getResolutionStatus()).isEqualTo("PARTIAL");
        assertThat(r.getAmountValue()).isEqualByComparingTo("3");
        assertThat(r.getIngredientUnitId())
                .as("unit row는 매칭됐으니 unit_id는 보존")
                .isEqualTo(10L);
        assertThat(r.getNormalizedGrams())
                .as("grams 정보 없으면 null")
                .isNull();
    }

    @Test
    @DisplayName("edible_grams_per_unit이 null이면 grams_per_unit fallback")
    void backfill_mapped_fallbackToGramsPerUnit() {
        Ingredient garlic = garlic();
        IngredientUnit unitWithFallback = IngredientUnit.builder()
                .id(10L).ingredient(garlic)
                .unitLabelKo("쪽").normalizedUnitLabel("쪽")
                .gramsPerUnit(new BigDecimal("6"))
                .edibleGramsPerUnit(null)  // null → grams_per_unit 사용
                .isDefault(false).build();
        RecipeIngredient r = row(101L, garlic, "마늘", "2", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(unitWithFallback));

        service.backfillChunk(0L, 100, false, false);

        assertThat(r.getResolutionStatus()).isEqualTo("MAPPED");
        assertThat(r.getNormalizedGrams())
                .as("edible_grams 없으면 grams_per_unit fallback (2 × 6 = 12)")
                .isEqualByComparingTo("12.000");
    }

    @Test
    @DisplayName("Accumulator status 분포 카운트 — MAPPED/PARTIAL/UNRESOLVED 동시 발생")
    void backfill_mixedStatuses_correctCounts() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        // r1: MAPPED, r2: PARTIAL (unit miss), r3: UNRESOLVED (정성)
        RecipeIngredient r1 = row(101L, garlic, "마늘", "3", "쪽", null);
        RecipeIngredient r2 = row(102L, garlic, "마늘", "1", "봉지", null);
        RecipeIngredient r3 = row(103L, garlic, "마늘", "약간", "쪽", null);
        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r1, r2, r3));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillResult result = service.backfillChunk(0L, 100, false, false);

        assertThat(result.scanned()).isEqualTo(3);
        assertThat(result.mapped()).isEqualTo(1);
        assertThat(result.partial()).isEqualTo(1);
        assertThat(result.unresolved()).isEqualTo(1);
        assertThat(result.customSkipped()).isZero();
    }

    /**
     * Row-level stopOnError 테스트는 row 처리 도중에 RuntimeException이 나야 한다.
     * QuantityParser를 mock으로 주입해 parse() 호출에서 throw 시켜 service.decide() 안의
     * row-level catch 분기를 직접 잠근다 (chunk-level prefetch 실패와 명확히 구분).
     */
    private RecipeIngredientBackfillService serviceWithBadParser(QuantityParser badParser) {
        return new RecipeIngredientBackfillService(
                backfillRepository, ingredientUnitRepository,
                badParser, new UnitNormalizer(), new IngredientUnitResolver());
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: stopOnError=false + row 처리 중 RuntimeException → catch + 다음 row 진행, failed 카운트 증가, 다른 row는 commit")
    void backfill_stopOnErrorFalse_rowFailure_continuesWithOtherRows() {
        QuantityParser badParser = org.mockito.Mockito.mock(QuantityParser.class);
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r1 = row(101L, garlic, "마늘", "3", "쪽", null);
        RecipeIngredient r2 = row(102L, garlic, "마늘", "5", "쪽", null);

        // r1.rawQuantityText="3"은 정상 파싱, r2.rawQuantityText="5"는 throw
        given(badParser.parse("3")).willReturn(new ParsedQuantity(new BigDecimal("3"), false, "3"));
        given(badParser.parse("5")).willThrow(new RuntimeException("simulated row failure"));

        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r1, r2));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillService localService = serviceWithBadParser(badParser);
        RecipeIngredientBackfillResult result = localService.backfillChunk(0L, 100, false, /* stopOnError= */ false);

        assertThat(result.scanned()).isEqualTo(2);
        assertThat(result.mapped())
                .as("r1은 정상 처리 → MAPPED")
                .isEqualTo(1);
        assertThat(result.failed())
                .as("r2의 RuntimeException은 catch + failed 카운트 증가")
                .isEqualTo(1);

        // r1은 정상 갱신
        assertThat(r1.getResolutionStatus()).isEqualTo("MAPPED");
        // r2는 entity 미갱신 (decide() 실패 → applyNormalizationBackfill 호출 안 됨)
        assertThat(r2.getResolutionStatus()).isNull();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: stopOnError=true + row 처리 중 RuntimeException → 즉시 throw (chunk tx rollback)")
    void backfill_stopOnErrorTrue_rowFailureThrows() {
        QuantityParser badParser = org.mockito.Mockito.mock(QuantityParser.class);
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        RecipeIngredient r1 = row(101L, garlic, "마늘", "3", "쪽", null);
        RecipeIngredient r2 = row(102L, garlic, "마늘", "5", "쪽", null);

        given(badParser.parse("3")).willReturn(new ParsedQuantity(new BigDecimal("3"), false, "3"));
        given(badParser.parse("5")).willThrow(new RuntimeException("simulated row failure"));

        given(backfillRepository.findNormalizationBackfillTargets(anyLong(), any(Pageable.class)))
                .willReturn(List.of(r1, r2));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientBackfillService localService = serviceWithBadParser(badParser);

        org.assertj.core.api.Assertions
                .assertThatThrownBy(() -> localService.backfillChunk(0L, 100, false, /* stopOnError= */ true))
                .as("row-level RuntimeException은 stopOnError=true 시 즉시 전파")
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("simulated row failure");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: decide()는 status 결정만 — IngredientResolutionStatus 4-state 모두 정확히 분류")
    void decide_directInvocation_coversAllStatuses() {
        Ingredient garlic = garlic();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        java.util.Map<Long, java.util.List<IngredientUnit>> unitsByIng =
                java.util.Map.of(1L, java.util.List.of(garlicPiece));

        // MAPPED
        RecipeIngredientBackfillService.BackfillDecision mapped =
                service.decide(row(1L, garlic, "마늘", "3", "쪽", null), unitsByIng);
        assertThat(mapped.status()).isEqualTo(IngredientResolutionStatus.MAPPED);

        // PARTIAL (unit miss)
        RecipeIngredientBackfillService.BackfillDecision partial =
                service.decide(row(2L, garlic, "마늘", "1", "봉지", null), unitsByIng);
        assertThat(partial.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);

        // UNRESOLVED (정성)
        RecipeIngredientBackfillService.BackfillDecision unresolved =
                service.decide(row(3L, garlic, "마늘", "약간", "쪽", null), unitsByIng);
        assertThat(unresolved.status()).isEqualTo(IngredientResolutionStatus.UNRESOLVED);
    }
}
