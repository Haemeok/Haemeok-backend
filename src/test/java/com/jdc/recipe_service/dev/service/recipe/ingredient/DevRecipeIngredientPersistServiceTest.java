package com.jdc.recipe_service.dev.service.recipe.ingredient;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepRepository;
import com.jdc.recipe_service.service.ingredient.normalize.IngredientUnitResolver;
import com.jdc.recipe_service.service.ingredient.normalize.QuantityParser;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientNormalizer;
import com.jdc.recipe_service.service.ingredient.normalize.UnitNormalizer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeIngredientPersistService unit tests.
 *
 * <p>Locks invariants for the dev ingredient persist path (also reused by 1.4 YouTube/AI):
 * <ul>
 *   <li>MAPPED line dual-writes ingredient + ingredient_unit_id + raw fields + normalized_grams + status</li>
 *   <li>C-prime bypass line keeps ingredient_id null but preserves ingredient_unit_id, customName, customUnit</li>
 *   <li>UNRESOLVED line keeps raw fields only, calculation pending - price=0, custom* = ZERO</li>
 *   <li>CUSTOM line requires both customCalorie and customPrice for inclusion</li>
 *   <li>recipe aggregate (totalIngredientCost / Count / nutrition / marketPrice) updated by new policy</li>
 *   <li>replaceAll deletes step_ingredient FKs first, then recipe_ingredient, then re-saves</li>
 * </ul>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DevRecipeIngredientPersistServiceTest {

    @Mock IngredientRepository ingredientRepository;
    @Mock IngredientUnitRepository ingredientUnitRepository;
    @Mock com.jdc.recipe_service.domain.repository.IngredientCandidateRepository ingredientCandidateRepository;
    @Mock RecipeIngredientRepository recipeIngredientRepository;
    @Mock RecipeStepRepository recipeStepRepository;
    @Mock RecipeStepIngredientRepository recipeStepIngredientRepository;

    DevRecipeIngredientPersistService service;
    Recipe recipe;
    final java.util.concurrent.atomic.AtomicLong nextCandId = new java.util.concurrent.atomic.AtomicLong(0L);

    private static final Long RECIPE_ID = 100L;

    @BeforeEach
    void setUp() {
        // 1.1 helper들은 pure component - real instance 사용
        QuantityParser quantityParser = new QuantityParser();
        UnitNormalizer unitNormalizer = new UnitNormalizer();
        IngredientUnitResolver unitResolver = new IngredientUnitResolver();
        RecipeIngredientNormalizer normalizer = new RecipeIngredientNormalizer(quantityParser, unitNormalizer, unitResolver);
        RecipeIngredientCalculator calculator = new RecipeIngredientCalculator();

        service = new DevRecipeIngredientPersistService(
                ingredientRepository, ingredientUnitRepository, ingredientCandidateRepository,
                recipeIngredientRepository, recipeStepRepository, recipeStepIngredientRepository,
                normalizer, calculator, unitNormalizer);

        // candidate save → echo with id assigned (id sequence 1000+)
        given(ingredientCandidateRepository.save(any(com.jdc.recipe_service.domain.entity.IngredientCandidate.class)))
                .willAnswer(inv -> {
                    com.jdc.recipe_service.domain.entity.IngredientCandidate c = inv.getArgument(0);
                    org.springframework.test.util.ReflectionTestUtils.setField(c, "id", 1000L + nextCandId.getAndIncrement());
                    return c;
                });

        // recipe entity는 real (aggregate setter 검증 필요)
        recipe = Recipe.builder().id(RECIPE_ID).title("Test").build();

        // save 호출 시 그대로 echo (실제 entity 검증 위해)
        given(recipeIngredientRepository.save(any(RecipeIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));
    }

    /** per-g 7개 모두 채워진 마스터. */
    private static Ingredient garlicMaster() {
        return Ingredient.builder()
                .id(1L).name("마늘")
                .price(100)                              // legacy (unused in dev calc)
                .unit("개")                              // legacy
                .calorie(new BigDecimal("3"))            // legacy
                .carbohydrate(new BigDecimal("0.6"))
                .protein(new BigDecimal("0.12"))
                .fat(new BigDecimal("0.01"))
                .sugar(new BigDecimal("0.02"))
                .sodium(new BigDecimal("0.04"))
                .kcalPerG(new BigDecimal("1.5"))
                .pricePerG(new BigDecimal("10"))
                .carbohydrateGPerG(new BigDecimal("0.3"))
                .proteinGPerG(new BigDecimal("0.06"))
                .fatGPerG(new BigDecimal("0.005"))
                .sugarGPerG(new BigDecimal("0.01"))
                .sodiumMgPerG(new BigDecimal("0.02"))
                .build();
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

    private RecipeIngredientRequestDto raw(String name, String qty, String unit) {
        return RecipeIngredientRequestDto.builder().name(name).quantity(qty).customUnit(unit).build();
    }

    @Test
    @DisplayName("MAPPED: ingredient + ingredient_unit_id + raw_*/normalized_grams 모두 채워지고 line price도 per-g 기반")
    void persistAll_mapped_dualWriteAllFields() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        service.persistAll(recipe, List.of(raw("마늘", "3", "쪽")), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getIngredient()).isSameAs(garlic);
        assertThat(saved.getIngredientUnitId()).isEqualTo(10L);
        assertThat(saved.getRawName()).isEqualTo("마늘");
        assertThat(saved.getRawQuantityText()).isEqualTo("3");
        assertThat(saved.getRawUnitText()).isEqualTo("쪽");
        assertThat(saved.getAmountValue()).isEqualByComparingTo("3");
        assertThat(saved.getNormalizedGrams()).isEqualByComparingTo("15.000");
        assertThat(saved.getResolutionStatus()).isEqualTo("MAPPED");
        // 15g × 10원/g = 150
        assertThat(saved.getPrice()).isEqualTo(150);
        // MAPPED - customName/custom* 비움
        assertThat(saved.getCustomName()).isNull();
        assertThat(saved.getCustomUnit()).isNull();
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("0");

        // recipe aggregate
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(150);
        assertThat(recipe.getTotalIngredientCount()).isEqualTo(1);
        // 15g × 1.5 kcal/g = 22.5
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("22.500");
        // marketPrice - totalCost > 0 -> margin 적용 (강제 조정 회피 위해 0보다 큼)
        assertThat(recipe.getMarketPrice()).isGreaterThan(0);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: C-prime bypass - 같은 ingredient + 다른 unit, 두 번째 row는 ingredient_id=null + unit_id 보존 + customName=raw 채움")
    void persistAll_bypassRow_dualWriteCorrectly() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        IngredientUnit garlicSpoon = unit(11L, garlic, "큰술", "8");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece, garlicSpoon));

        service.persistAll(recipe, List.of(
                raw("마늘", "3", "쪽"),
                raw("마늘", "1", "큰술")
        ), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(2)).save(captor.capture());
        List<RecipeIngredient> savedList = captor.getAllValues();

        // First row - MAPPED
        RecipeIngredient first = savedList.get(0);
        assertThat(first.getIngredient()).isSameAs(garlic);
        assertThat(first.getIngredientUnitId()).isEqualTo(10L);
        assertThat(first.getResolutionStatus()).isEqualTo("MAPPED");

        // Second row - C-prime bypass
        RecipeIngredient bypass = savedList.get(1);
        assertThat(bypass.getIngredient())
                .as("bypass: UNIQUE 회피로 ingredient_id=null")
                .isNull();
        assertThat(bypass.getIngredientUnitId())
                .as("unit_id는 보존되어야 calc가 unit->ingredient로 복원 가능")
                .isEqualTo(11L);
        assertThat(bypass.getResolutionStatus()).isEqualTo("PARTIAL");
        assertThat(bypass.getRawName()).isEqualTo("마늘");
        assertThat(bypass.getCustomName())
                .as("V2 read path가 customName을 보므로 raw_name으로 채움")
                .isEqualTo("마늘");
        assertThat(bypass.getCustomUnit()).isEqualTo("큰술");
        // 8g × 10원/g = 80원
        assertThat(bypass.getPrice()).isEqualTo(80);
        assertThat(bypass.getCustomPrice())
                .as("V2 read 일관: 같은 라인 가격을 customPrice에도 박는다 (V2 mapper가 isCustom path 사용)")
                .isEqualTo(80);
        // 8g × 1.5 = 12 kcal
        assertThat(bypass.getCustomCalorie()).isEqualByComparingTo("12.000");

        // recipe aggregate: 15g+8g = 23g -> 23×10=230원, 23×1.5=34.5kcal
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(230);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("34.500");
        assertThat(recipe.getTotalIngredientCount()).isEqualTo(2);
    }

    @Test
    @DisplayName("UNRESOLVED: 이름 매칭 실패 -> ingredient=null + customName=raw + calc 보류 (price=0, custom*=ZERO)")
    void persistAll_unresolved_savesAsRawCustom() {
        // master에 "황태머리" 없음
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlicMaster()));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        service.persistAll(recipe, List.of(raw("황태머리", "1", "마리")), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getIngredient()).isNull();
        assertThat(saved.getIngredientUnitId()).isNull();
        assertThat(saved.getResolutionStatus()).isEqualTo("UNRESOLVED");
        assertThat(saved.getCustomName()).isEqualTo("황태머리");
        assertThat(saved.getCustomUnit()).isEqualTo("마리");
        assertThat(saved.getRawName()).isEqualTo("황태머리");
        // calc 보류 -> price=0 (line price는 calculator output)
        assertThat(saved.getPrice()).isEqualTo(0);
        // **AI/사용자 입력 누락은 null 그대로 저장** — ZERO/0 default 안 박힘.
        // "AI가 0을 정상 명시" vs "필드 누락"을 read 단에서 구분 가능해야 합산이 정확.
        assertThat(saved.getCustomCalorie())
                .as("입력 누락은 null로 저장 (legacy default ZERO와 명시 0을 구분)")
                .isNull();
        assertThat(saved.getCustomPrice()).isNull();
        // recipe aggregate: 0
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(0);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("0");
    }

    @Test
    @DisplayName("CUSTOM by user: 명시 customCalorie+customPrice 둘 다 있을 때 calc 포함")
    void persistAll_customByUser_explicitValues() {
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        RecipeIngredientRequestDto customLine = RecipeIngredientRequestDto.builder()
                .name("엄마표양념")
                .quantity("1")
                .customUnit("티스푼")
                .customByUser(true)
                .customPrice(new BigDecimal("500"))
                .customCalories(new BigDecimal("80"))
                .build();

        service.persistAll(recipe, List.of(customLine), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getResolutionStatus()).isEqualTo("CUSTOM");
        assertThat(saved.getIngredient()).isNull();
        assertThat(saved.getCustomName()).isEqualTo("엄마표양념");
        assertThat(saved.getPrice()).isEqualTo(500);
        assertThat(saved.getCustomPrice()).isEqualTo(500);
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("80");

        // recipe aggregate
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(500);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("80.000");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: customByUser=true는 마스터 이름이 같아도 매핑 안 함 (CUSTOM 강제)")
    void persistAll_customByUser_bypassesMasterMatch() {
        Ingredient garlic = garlicMaster();
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(unit(10L, garlic, "쪽", "5")));

        // 이름이 master와 같지만 customByUser=true -> CUSTOM
        RecipeIngredientRequestDto line = RecipeIngredientRequestDto.builder()
                .name("마늘")
                .quantity("3")
                .customUnit("쪽")
                .customByUser(true)
                .customPrice(new BigDecimal("999"))
                .customCalories(new BigDecimal("123"))
                .build();

        service.persistAll(recipe, List.of(line), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getResolutionStatus()).isEqualTo("CUSTOM");
        assertThat(saved.getIngredient())
                .as("customByUser=true는 master matching을 건너뛴다")
                .isNull();
        assertThat(saved.getCustomName()).isEqualTo("마늘");
        assertThat(saved.getCustomPrice()).isEqualTo(999);
    }

    @Test
    @DisplayName("빈 입력: aggregate는 0으로 갱신, save 미호출")
    void persistAll_emptyInput_zeroAggregates() {
        // 빈 list 처리 시 ingredientRepository.findAllByIsActiveTrue() / ingredientUnitRepository.findAllByIngredientIdIn(any()) 호출 안 됨
        service.persistAll(recipe, List.of(), null);

        verify(recipeIngredientRepository, never()).save(any());
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(0);
        assertThat(recipe.getTotalIngredientCount()).isEqualTo(0);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("0");
        assertThat(recipe.getMarketPrice()).isEqualTo(0);
    }

    @Test
    @DisplayName("null 입력: 빈 입력과 동일 처리")
    void persistAll_nullInput_zeroAggregates() {
        service.persistAll(recipe, null, null);

        verify(recipeIngredientRepository, never()).save(any());
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(0);
    }

    @Test
    @DisplayName("suppliedMarketPrice 있으면 그대로 사용 (0보다 클 때)")
    void persistAll_suppliedMarketPrice_used() {
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        service.persistAll(recipe, List.of(), 9999);

        assertThat(recipe.getMarketPrice()).isEqualTo(9999);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: replaceAll은 step_ingredient FK 정리 -> recipe_ingredient 삭제 -> 재저장 순서")
    void replaceAll_cascadeDeleteOrder() {
        RecipeIngredient existing1 = RecipeIngredient.builder().id(101L).recipe(recipe).build();
        RecipeIngredient existing2 = RecipeIngredient.builder().id(102L).recipe(recipe).build();
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID))
                .willReturn(List.of(existing1, existing2));
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        service.replaceAll(recipe, List.of(), null);

        // step_ingredient FK 먼저 정리 (각 existing별로)
        verify(recipeStepIngredientRepository).deleteByRecipeIngredientId(101L);
        verify(recipeStepIngredientRepository).deleteByRecipeIngredientId(102L);
        // recipe_ingredient 일괄 삭제
        verify(recipeIngredientRepository).deleteByRecipeId(RECIPE_ID);
        verify(recipeIngredientRepository).flush();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 같은 ingredient + 같은 unit은 한 row로 merge되고 amount가 합산")
    void persistAll_sameIngredientSameUnit_mergedToOneRow() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        service.persistAll(recipe, new ArrayList<>(List.of(
                raw("마늘", "3", "쪽"),
                raw("마늘", "5", "쪽")
        )), null);

        // 한 row만 저장됨 (merge)
        verify(recipeIngredientRepository, times(1)).save(any(RecipeIngredient.class));

        // recipe aggregate: 8쪽 × 5g = 40g -> 40 × 10 = 400원, 40 × 1.5 = 60kcal
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(400);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("60.000");
        assertThat(recipe.getTotalIngredientCount()).isEqualTo(1);
    }

    @Test
    @DisplayName("pantry 마스터(기본 양념)는 totalIngredientCount에서 제외")
    void persistAll_pantryIngredient_notCountedInRealCount() {
        Ingredient salt = Ingredient.builder()
                .id(2L).name("소금")
                .isPantry(true)
                .kcalPerG(new BigDecimal("0"))
                .pricePerG(new BigDecimal("0"))
                .carbohydrateGPerG(new BigDecimal("0"))
                .proteinGPerG(new BigDecimal("0"))
                .fatGPerG(new BigDecimal("0"))
                .sugarGPerG(new BigDecimal("0"))
                .sodiumMgPerG(new BigDecimal("39"))
                .build();
        IngredientUnit saltGram = unit(20L, salt, "g", "1");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(salt));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(saltGram));

        service.persistAll(recipe, List.of(raw("소금", "5", "g")), null);

        // 1 row 저장되지만 realCount는 0 (pantry 제외)
        verify(recipeIngredientRepository, times(1)).save(any(RecipeIngredient.class));
        assertThat(recipe.getTotalIngredientCount()).isEqualTo(0);
    }

    // ─── linkStepIngredients (MUST 1: empty-then-save 후처리) ─────────────────

    @Test
    @DisplayName("**MUST 회귀 차단**: linkStepIngredients - 저장된 RecipeIngredient를 step의 ingredient ref와 매칭 (rawName 우선)")
    void linkStepIngredients_matchesByRawNameAndSavesStepIngredients() {
        Ingredient garlic = garlicMaster();
        RecipeIngredient savedRi = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .resolutionStatus("MAPPED")
                .build();
        RecipeStep savedStep = RecipeStep.builder()
                .id(201L).recipe(recipe).stepNumber(0).instruction("끓인다").build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(savedRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(savedStep));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .instruction("끓인다")
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("3").customUnit("쪽").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository).save(captor.capture());
        RecipeStepIngredient saved = captor.getValue();
        assertThat(saved.getStep()).isSameAs(savedStep);
        assertThat(saved.getRecipeIngredient()).isSameAs(savedRi);
        assertThat(saved.getIngredient()).isSameAs(garlic);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: linkStepIngredients - bypass row(customName=raw)도 step ref와 매칭됨")
    void linkStepIngredients_matchesBypassRowByCustomName() {
        // bypass row: ingredient_id=null, customName=rawName="마늘", customUnit="큰술"
        RecipeIngredient bypassRi = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(null)
                .quantity("1").unit("큰술")
                .rawName("마늘").rawQuantityText("1").rawUnitText("큰술")
                .customName("마늘").customUnit("큰술")
                .resolutionStatus("PARTIAL")
                .build();
        RecipeStep savedStep = RecipeStep.builder()
                .id(201L).recipe(recipe).stepNumber(0).instruction("볶는다").build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(bypassRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(savedStep));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("1").customUnit("큰술").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getRecipeIngredient()).isSameAs(bypassRi);
        // bypass row의 ingredient_id=null이므로 stepIngredient.ingredient도 null (StepIngredientMapper isCustom path)
        assertThat(captor.getValue().getIngredient()).isNull();
        assertThat(captor.getValue().getCustomName()).isEqualTo("마늘");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: linkStepIngredients - 매칭 실패 시 INGREDIENT_NOT_FOUND throw (운영 동등)")
    void linkStepIngredients_unknownIngredient_throwsIngredientNotFound() {
        // 저장된 ingredient 없음
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of());
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID))
                .willReturn(List.of(RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build()));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("황태머리").build()))
                .build();

        org.assertj.core.api.Assertions
                .assertThatThrownBy(() -> service.linkStepIngredients(recipe, List.of(stepDto)))
                .isInstanceOf(com.jdc.recipe_service.exception.CustomException.class);
    }

    @Test
    @DisplayName("linkStepIngredients - 빈/null step 리스트는 no-op (DB hit 없음)")
    void linkStepIngredients_emptySteps_noOp() {
        service.linkStepIngredients(recipe, null);
        service.linkStepIngredients(recipe, List.of());

        verify(recipeIngredientRepository, never()).findByRecipeId(any());
        verify(recipeStepIngredientRepository, never()).save(any());
    }

    @Test
    @DisplayName("linkStepIngredients - step에 ingredients 없으면 그 step은 skip")
    void linkStepIngredients_stepWithoutIngredients_skipped() {
        RecipeStepRequestDto stepNoIng = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .instruction("씻는다")
                .ingredients(null)
                .build();

        service.linkStepIngredients(recipe, List.of(stepNoIng));

        verify(recipeStepIngredientRepository, never()).save(any());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: C' bypass — 같은 이름 + 다른 unit 두 row일 때 step의 unit 으로 정확히 구별된다 (이름만 매칭하면 잘못된 row에 link됨)")
    void linkStepIngredients_bypassRows_disambiguateByUnit() {
        // 저장된 ingredient: 마늘 3쪽 (MAPPED, id=101) + 마늘 1큰술 (PARTIAL bypass, id=102)
        Ingredient garlic = garlicMaster();
        RecipeIngredient mappedRi = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .resolutionStatus("MAPPED")
                .build();
        RecipeIngredient bypassRi = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(null)
                .quantity("1").unit("큰술")
                .customName("마늘").customUnit("큰술")
                .rawName("마늘").rawQuantityText("1").rawUnitText("큰술")
                .ingredientUnitId(11L)
                .resolutionStatus("PARTIAL")
                .build();

        RecipeStep step1 = RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build();
        RecipeStep step2 = RecipeStep.builder().id(202L).recipe(recipe).stepNumber(1).build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mappedRi, bypassRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(step1, step2));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        // step 0: 마늘 3쪽 의도 → MAPPED row(101)에 link되어야 함
        // step 1: 마늘 1큰술 의도 → bypass row(102)에 link되어야 함 (이전엔 putIfAbsent로 마늘 3쪽으로 잘못 묶임)
        RecipeStepRequestDto stepDto1 = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("3").customUnit("쪽").build()))
                .build();
        RecipeStepRequestDto stepDto2 = RecipeStepRequestDto.builder()
                .stepNumber(1)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("1").customUnit("큰술").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto1, stepDto2));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository, times(2)).save(captor.capture());

        // step 0의 RecipeStepIngredient → mappedRi(101)에 link
        RecipeStepIngredient step1Saved = captor.getAllValues().get(0);
        assertThat(step1Saved.getStep()).isSameAs(step1);
        assertThat(step1Saved.getRecipeIngredient())
                .as("step 0의 \"마늘 3쪽\"은 MAPPED row(101)에 정확히 link되어야 한다")
                .isSameAs(mappedRi);

        // step 1의 RecipeStepIngredient → bypassRi(102)에 link
        RecipeStepIngredient step2Saved = captor.getAllValues().get(1);
        assertThat(step2Saved.getStep()).isSameAs(step2);
        assertThat(step2Saved.getRecipeIngredient())
                .as("step 1의 \"마늘 1큰술\"은 bypass row(102)에 link되어야 한다 — putIfAbsent로 첫 row에 잘못 묶이면 회귀")
                .isSameAs(bypassRi);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: 같은 이름 두 row + step이 unit 안 줌 → ambiguous → INGREDIENT_NOT_FOUND throw")
    void linkStepIngredients_ambiguousNameWithoutUnit_throws() {
        Ingredient garlic = garlicMaster();
        RecipeIngredient mappedRi = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();
        RecipeIngredient bypassRi = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(null)
                .quantity("1").unit("큰술").customName("마늘").customUnit("큰술")
                .rawName("마늘").rawUnitText("큰술")
                .ingredientUnitId(11L).resolutionStatus("PARTIAL").build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mappedRi, bypassRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID))
                .willReturn(List.of(RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build()));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘")  // unit 안 줌 — 어느 row를 의도했는지 모름
                        .build()))
                .build();

        org.assertj.core.api.Assertions
                .assertThatThrownBy(() -> service.linkStepIngredients(recipe, List.of(stepDto)))
                .as("같은 이름 다른 unit row가 둘이고 step이 unit 미명시면 잘못된 row에 묶이지 않게 throw")
                .isInstanceOf(com.jdc.recipe_service.exception.CustomException.class);

        verify(recipeStepIngredientRepository, never()).save(any());
    }

    @Test
    @DisplayName("같은 이름 단 1개 row + step이 unit 안 줌 → name-only fallback으로 link 성공")
    void linkStepIngredients_singleCandidateNameOnly_fallbackOk() {
        Ingredient garlic = garlicMaster();
        RecipeIngredient onlyRi = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();
        RecipeStep step = RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(onlyRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(step));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        // step에서 unit 미명시
        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getRecipeIngredient()).isSameAs(onlyRi);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: 같은 name+unit 두 row(약간 vs 3) — quantity로만 disambiguate되는 케이스가 정확히 매칭됨")
    void linkStepIngredients_sameNameSameUnitDifferentQty_disambiguatedByQuantity() {
        // normalizer가 "마늘 약간 쪽" + "마늘 3쪽"을 한쪽 amount=null이라 합산 불가로 두 row 보존:
        // - row1: MAPPED, rawQty="약간", unit="쪽"
        // - row2: PARTIAL bypass, rawQty="3", unit="쪽"
        // 두 row의 (name, unit)은 동일하므로 tier 2(name+unit)로는 disambiguate 불가 — tier 1(qty 추가)이 필요.
        Ingredient garlic = garlicMaster();
        RecipeIngredient mappedSpecial = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("약간").unit("쪽")
                .rawName("마늘").rawQuantityText("약간").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();
        RecipeIngredient bypassWithQty = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(null)
                .quantity("3").unit("쪽")
                .customName("마늘").customUnit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("PARTIAL").build();

        RecipeStep step1 = RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build();
        RecipeStep step2 = RecipeStep.builder().id(202L).recipe(recipe).stepNumber(1).build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mappedSpecial, bypassWithQty));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(step1, step2));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        // step 0: 약간 → row 1 (MAPPED)
        // step 1: 3 → row 2 (bypass) — tier 2(name+unit)만 있으면 둘 다 동일 키라 putIfAbsent로 row 1로만 묶임
        RecipeStepRequestDto stepDto1 = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("약간").customUnit("쪽").build()))
                .build();
        RecipeStepRequestDto stepDto2 = RecipeStepRequestDto.builder()
                .stepNumber(1)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("3").customUnit("쪽").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto1, stepDto2));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository, times(2)).save(captor.capture());

        assertThat(captor.getAllValues().get(0).getRecipeIngredient())
                .as("step 0의 \"마늘 약간 쪽\" → row 1 (rawQty=약간) — tier 1 정확 매칭")
                .isSameAs(mappedSpecial);
        assertThat(captor.getAllValues().get(1).getRecipeIngredient())
                .as("step 1의 \"마늘 3쪽\" → row 2 (rawQty=3) — name+unit만으로는 둘 다 후보지만 quantity가 disambiguate")
                .isSameAs(bypassWithQty);
    }

    @Test
    @DisplayName("merge된 row의 rawQuantityText가 합산값(8)이라도, step quantity가 원본(3)이면 tier 2 1-candidate fallback으로 매칭 (운영 호환)")
    void linkStepIngredients_mergedRowQuantityMismatch_tier2FallbackOk() {
        // 마늘 3쪽 + 마늘 5쪽 → merge 후 rawQty="8". step은 원본 "3"을 보낼 수 있음.
        Ingredient garlic = garlicMaster();
        RecipeIngredient mergedRi = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("8").unit("쪽")
                .rawName("마늘").rawQuantityText("8").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();
        RecipeStep step = RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mergedRi));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(step));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        // step's quantity는 "3" (원본 입력 시점), saved rawQty는 "8" (merge 결과) → tier 1 miss
        // tier 2: name+unit candidates 1개 → fallback success
        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("3").customUnit("쪽").build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getRecipeIngredient()).isSameAs(mergedRi);
    }

    @Test
    @DisplayName("**NIT 회귀 차단**: tier 1에서 같은 name+unit+qty가 2 row 이상이면 (normalizer 비정상) 1개일 때만 받고 그 외엔 fail-fast")
    void linkStepIngredients_tier1AmbiguousMultipleHits_failsFastConsistentWithOtherTiers() {
        // 비정상 상태 시뮬레이션: normalizer가 (name+unit+qty) 동일한 두 row를 만든 경우.
        // 실제로는 normalizer dedupe로 발생하지 않지만, 데이터 손상이나 직접 INSERT 시나리오 방어.
        Ingredient garlic = garlicMaster();
        RecipeIngredient duplicate1 = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();
        RecipeIngredient duplicate2 = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L).resolutionStatus("MAPPED").build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(duplicate1, duplicate2));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID))
                .willReturn(List.of(RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build()));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").quantity("3").customUnit("쪽").build()))
                .build();

        // tier 1에서 hits=2 → 안 받아 흘림 → tier 2도 candidates=2 → 안 받음 → tier 3도 2 → throw
        org.assertj.core.api.Assertions
                .assertThatThrownBy(() -> service.linkStepIngredients(recipe, List.of(stepDto)))
                .as("tier 1에서 첫 row 임의 선택하면 잘못된 link 가능 — 2+ 후보면 fail-fast로 throw")
                .isInstanceOf(com.jdc.recipe_service.exception.CustomException.class);

        verify(recipeStepIngredientRepository, never()).save(any());
    }

    // ─── 1.4: persistAllSystemSourced (AI/YouTube path 진입점) ───────────────────

    @Test
    @DisplayName("**MUST 회귀 차단**: persistAllSystemSourced는 customByUser=true가 박혀와도 false로 강제 → CUSTOM 분기 안 탐")
    void persistAllSystemSourced_forcesCustomByUserFalse_evenIfTrueProvided() {
        // AI/YouTube path가 어쩌다 customByUser=true를 보내도 시스템 매칭이 작동해야 함.
        // 이름이 master와 일치하면 MAPPED로 가야 — CUSTOM이면 후속 candidates 큐잉도 안 되고 영구 미해결.
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        // 입력에 customByUser=true가 박혀있음 (AI/YouTube가 잘못 보낸 경우 시뮬레이션)
        RecipeIngredientRequestDto badInput = RecipeIngredientRequestDto.builder()
                .name("마늘")
                .quantity("3")
                .customUnit("쪽")
                .customByUser(true)  // ← 시스템 path에서 절대 박히면 안 됨
                .build();

        service.persistAllSystemSourced(recipe, List.of(badInput), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getResolutionStatus())
                .as("AI/YouTube path는 customByUser=true가 와도 무시 → master 매칭으로 MAPPED")
                .isEqualTo("MAPPED");
        assertThat(saved.getIngredient())
                .as("CUSTOM이 강제되면 ingredient=null이 됨 — MAPPED여야 master link됨")
                .isSameAs(garlic);
    }

    @Test
    @DisplayName("persistAllSystemSourced - 원본 dto는 mutate 안 됨 (false 강제는 새 list에)")
    void persistAllSystemSourced_doesNotMutateOriginalDto() {
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        RecipeIngredientRequestDto original = RecipeIngredientRequestDto.builder()
                .name("황태머리").quantity("1").customUnit("마리")
                .customByUser(true)  // 원본 의도
                .build();
        List<RecipeIngredientRequestDto> originals = List.of(original);

        service.persistAllSystemSourced(recipe, originals, null);

        // 원본 dto는 그대로 — true 유지
        assertThat(original.getCustomByUser())
                .as("원본 dto는 mutate되면 호출자가 다른 path에서 같은 dto를 재사용할 때 의도가 손상됨")
                .isTrue();
    }

    @Test
    @DisplayName("persistAllSystemSourced - customByUser=null/false인 정상 입력은 원본 그대로 흘려보냄")
    void persistAllSystemSourced_passThroughForNormalInputs() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        // customByUser=null (기본값) — AI/YouTube의 정상 입력
        RecipeIngredientRequestDto input = RecipeIngredientRequestDto.builder()
                .name("마늘").quantity("3").customUnit("쪽")
                // customByUser 미설정 (null)
                .build();

        service.persistAllSystemSourced(recipe, List.of(input), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getResolutionStatus()).isEqualTo("MAPPED");
    }

    @Test
    @DisplayName("persistAllSystemSourced - null/empty 입력 안전 처리 (NPE 없음)")
    void persistAllSystemSourced_nullEmptySafe() {
        service.persistAllSystemSourced(recipe, null, null);
        service.persistAllSystemSourced(recipe, List.of(), null);

        verify(recipeIngredientRepository, never()).save(any());
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(0);
    }

    @Test
    @DisplayName("step의 unit이 영문 alias여도 매칭됨 (UnitNormalizer 통합: 1Tbsp ↔ 큰술)")
    void linkStepIngredients_englishUnitAlias_resolvesViaNormalizer() {
        Ingredient garlic = garlicMaster();
        RecipeIngredient mappedSpoon = RecipeIngredient.builder()
                .id(102L).recipe(recipe).ingredient(garlic)
                .quantity("1").unit("큰술").rawName("마늘").rawUnitText("큰술")
                .ingredientUnitId(11L).resolutionStatus("MAPPED").build();
        RecipeStep step = RecipeStep.builder().id(201L).recipe(recipe).stepNumber(0).build();

        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mappedSpoon));
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(RECIPE_ID)).willReturn(List.of(step));
        given(recipeStepIngredientRepository.save(any(RecipeStepIngredient.class)))
                .willAnswer(inv -> inv.getArgument(0));

        RecipeStepRequestDto stepDto = RecipeStepRequestDto.builder()
                .stepNumber(0)
                .ingredients(List.of(RecipeStepIngredientRequestDto.builder()
                        .name("마늘").customUnit("Tbsp")  // 영문 alias
                        .build()))
                .build();

        service.linkStepIngredients(recipe, List.of(stepDto));

        ArgumentCaptor<RecipeStepIngredient> captor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getRecipeIngredient()).isSameAs(mappedSpoon);
    }

    // ─── Phase 3: candidate 생성 + custom* 보존 정책 ───────────────────────────

    /** AI fallback path 3 included를 위해 7개 custom 필드를 모두 채운 dto helper. */
    private static RecipeIngredientRequestDto.RecipeIngredientRequestDtoBuilder fullCustomDto(
            String name, String qty, String unit,
            String price, String kcal,
            String carb, String protein, String fat, String sugar, String sodium) {
        return RecipeIngredientRequestDto.builder()
                .name(name).quantity(qty).customUnit(unit)
                .customPrice(new BigDecimal(price))
                .customCalories(new BigDecimal(kcal))
                .customCarbohydrate(new BigDecimal(carb))
                .customProtein(new BigDecimal(protein))
                .customFat(new BigDecimal(fat))
                .customSugar(new BigDecimal(sugar))
                .customSodium(new BigDecimal(sodium));
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: PARTIAL ingredient-hit + unit miss → UNIT candidate 생성 + 7개 custom 필드 line-total 보존 + path 3 included")
    void persist_partialUnitMiss_createsUnitCandidate_andPreservesCustomFallback() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        // 마늘 + 봉지(unit miss) + AI 7개 custom 필드 모두 동반
        RecipeIngredientRequestDto dto = fullCustomDto(
                "마늘", "1", "봉지",
                "3000", "450", "60", "20", "5", "10", "300"
        ).build();

        service.persistAll(recipe, List.of(dto), null);

        // candidate type=UNIT, ingredient=garlic 으로 한 번 INSERT
        ArgumentCaptor<com.jdc.recipe_service.domain.entity.IngredientCandidate> candCaptor =
                ArgumentCaptor.forClass(com.jdc.recipe_service.domain.entity.IngredientCandidate.class);
        verify(ingredientCandidateRepository).save(candCaptor.capture());
        com.jdc.recipe_service.domain.entity.IngredientCandidate candidate = candCaptor.getValue();
        assertThat(candidate.getCandidateType()).isEqualTo("UNIT");
        assertThat(candidate.getIngredient()).isSameAs(garlic);
        assertThat(candidate.getRawUnitText()).isEqualTo("봉지");
        assertThat(candidate.getStatus()).isEqualTo("PENDING");

        // RecipeIngredient는 candidate id로 연결, custom* line-total 보존
        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(riCaptor.capture());
        RecipeIngredient saved = riCaptor.getValue();
        assertThat(saved.getResolutionStatus()).isEqualTo("PARTIAL");
        assertThat(saved.getIngredient()).isSameAs(garlic);
        assertThat(saved.getIngredientUnitId()).isNull();
        assertThat(saved.getIngredientCandidateId())
                .as("candidate id가 fallback signal로 연결되어야 — calculator path 3 진입 가드")
                .isEqualTo(candidate.getId());
        assertThat(saved.getCustomPrice())
                .as("AI line-total 그대로 보존")
                .isEqualTo(3000);
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("450");

        // recipe aggregate: path 3 included (candidate signal + 7개 custom* 모두 있음)
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(3000);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("450.000");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: UNRESOLVED → INGREDIENT candidate 생성 + 7개 custom 필드 보존 + path 3 included")
    void persist_unresolved_createsIngredientCandidate_andPreservesCustomFallback() {
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        RecipeIngredientRequestDto dto = fullCustomDto(
                "황태머리", "1", "개",
                "500", "80", "10", "15", "2", "3", "200"
        ).build();

        service.persistAll(recipe, List.of(dto), null);

        ArgumentCaptor<com.jdc.recipe_service.domain.entity.IngredientCandidate> candCaptor =
                ArgumentCaptor.forClass(com.jdc.recipe_service.domain.entity.IngredientCandidate.class);
        verify(ingredientCandidateRepository).save(candCaptor.capture());
        com.jdc.recipe_service.domain.entity.IngredientCandidate candidate = candCaptor.getValue();
        assertThat(candidate.getCandidateType()).isEqualTo("INGREDIENT");
        assertThat(candidate.getIngredient()).isNull();  // 미인식 재료
        assertThat(candidate.getRawName()).isEqualTo("황태머리");
        assertThat(candidate.getProposedNameKo()).isEqualTo("황태머리");
        assertThat(candidate.getStatus()).isEqualTo("PENDING");

        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(riCaptor.capture());
        RecipeIngredient saved = riCaptor.getValue();
        assertThat(saved.getResolutionStatus()).isEqualTo("UNRESOLVED");
        assertThat(saved.getIngredientCandidateId()).isEqualTo(candidate.getId());
        assertThat(saved.getCustomPrice()).isEqualTo(500);
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("80");

        // path 3 included
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(500);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("80.000");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: 같은 ingredient + 둘 다 unknown unit → 두 row 모두 UNIT candidate 생성 + 합산 included")
    void persist_duplicateUnitMissBypass_bothRowsGetCandidates_andIncluded() {
        // 시나리오: "마늘 1봉지" + "마늘 2덩이" 둘 다 unit miss.
        // - 첫 row: PARTIAL with ingredient_id=garlic, unit_id=null (ingredient hit + unit miss)
        // - 둘째 row: 같은 ingredient_id 이미 점유됨 → bypass branch + matchedUnit=null →
        //   ingredient_id=null + unit_id=null + unitOwnerIngredient=garlic. isBypassRow()=false (unit_id 없으므로).
        // 둘 다 UNIT candidate가 만들어져 path 3 fallback이 동작해야 합산이 한쪽으로만 되는 회귀를 방어.
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");  // 봉지/덩이는 master에 없음
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        RecipeIngredientRequestDto first = fullCustomDto(
                "마늘", "1", "봉지",
                "3000", "450", "60", "20", "5", "10", "300"
        ).build();
        RecipeIngredientRequestDto second = fullCustomDto(
                "마늘", "2", "덩이",
                "5000", "800", "100", "30", "10", "20", "500"
        ).build();

        service.persistAll(recipe, List.of(first, second), null);

        // 두 row 모두 UNIT candidate 생성, 둘 다 garlic 연결 (resolved 또는 unitOwner)
        ArgumentCaptor<com.jdc.recipe_service.domain.entity.IngredientCandidate> candCaptor =
                ArgumentCaptor.forClass(com.jdc.recipe_service.domain.entity.IngredientCandidate.class);
        verify(ingredientCandidateRepository, times(2)).save(candCaptor.capture());
        List<com.jdc.recipe_service.domain.entity.IngredientCandidate> candidates = candCaptor.getAllValues();
        assertThat(candidates).allSatisfy(c -> {
            assertThat(c.getCandidateType()).isEqualTo("UNIT");
            assertThat(c.getIngredient())
                    .as("duplicate unit-miss bypass row도 unitOwnerIngredient로 garlic 연결되어야")
                    .isSameAs(garlic);
            assertThat(c.getStatus()).isEqualTo("PENDING");
        });
        assertThat(candidates).extracting(c -> c.getRawUnitText())
                .containsExactlyInAnyOrder("봉지", "덩이");

        // 두 RecipeIngredient row 모두 candidate id 연결 (signal 박힘)
        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(2)).save(riCaptor.capture());
        List<RecipeIngredient> savedRows = riCaptor.getAllValues();

        RecipeIngredient firstSaved = savedRows.get(0);
        assertThat(firstSaved.getResolutionStatus()).isEqualTo("PARTIAL");
        assertThat(firstSaved.getIngredient()).isSameAs(garlic);
        assertThat(firstSaved.getIngredientUnitId()).isNull();
        assertThat(firstSaved.getIngredientCandidateId())
                .as("첫 row도 unit miss이므로 UNIT candidate id 박혀야 함")
                .isNotNull();
        assertThat(firstSaved.getCustomPrice()).isEqualTo(3000);

        RecipeIngredient secondSaved = savedRows.get(1);
        assertThat(secondSaved.getResolutionStatus()).isEqualTo("PARTIAL");
        assertThat(secondSaved.getIngredient())
                .as("UNIQUE 회피로 ingredient_id=null")
                .isNull();
        assertThat(secondSaved.getIngredientUnitId())
                .as("unit miss이므로 unit_id도 null — duplicate unit-miss bypass 형태")
                .isNull();
        assertThat(secondSaved.getIngredientCandidateId())
                .as("**핵심**: unit_id=null이라도 unitOwnerIngredient로 UNIT candidate 만들어 signal 박혀야")
                .isNotNull();
        assertThat(secondSaved.getCustomPrice()).isEqualTo(5000);
        assertThat(secondSaved.getCustomCalorie()).isEqualByComparingTo("800");

        assertThat(firstSaved.getIngredientCandidateId())
                .as("두 candidate id는 서로 달라야 — 각 row별 별도 INSERT")
                .isNotEqualTo(secondSaved.getIngredientCandidateId());

        // 합산: 두 row 모두 path 3 included → 3000 + 5000 = 8000원, 450 + 800 = 1250 kcal
        assertThat(recipe.getTotalIngredientCost())
                .as("**핵심**: 둘째 row가 candidate 없으면 pending되어 합산이 3000이 되는 회귀 방어")
                .isEqualTo(8000);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("1250.000");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: AI fallback 누락 macros → null로 저장 (ZERO default 안 박힘) — read 단에서 0/누락 구분 가능")
    void persist_fallbackRow_missingCustomFields_savedAsNull() {
        // UNRESOLVED + customPrice/customCalorie만 명시, macros 누락.
        // 이전엔 builder @Builder.Default(ZERO)로 0 박혔지만 새 정책은 명시적 null 호출로 null 저장.
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("황태머리").quantity("1").customUnit("개")
                .customPrice(new BigDecimal("500"))
                .customCalories(new BigDecimal("80"))
                // macros 5개 누락
                .build();

        service.persistAll(recipe, List.of(dto), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getCustomPrice()).isEqualTo(500);
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("80");
        assertThat(saved.getCustomCarbohydrate())
                .as("**핵심**: 누락 macros는 null로 저장 — 'AI가 0을 명시' vs '필드 누락'을 DB 차원에서 구분")
                .isNull();
        assertThat(saved.getCustomProtein()).isNull();
        assertThat(saved.getCustomFat()).isNull();
        assertThat(saved.getCustomSugar()).isNull();
        assertThat(saved.getCustomSodium()).isNull();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: AI fallback 입력에 customFat=0 명시 → 0 그대로 저장 (null로 변환 안 됨)")
    void persist_fallbackRow_explicitZeroPreserved() {
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of());
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        // AI가 line-total로 "지방 0g, 당 0g"을 정상 명시한 케이스 (예: 지방 거의 없는 재료).
        RecipeIngredientRequestDto dto = fullCustomDto(
                "황태머리", "1", "개",
                "500", "80",
                "10",   // 탄
                "15",   // 단
                "0",    // 지 — AI가 "0g" 정상 명시
                "0",    // 당 — AI가 "0g" 정상 명시
                "200"   // 나
        ).build();

        service.persistAll(recipe, List.of(dto), null);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(captor.capture());
        RecipeIngredient saved = captor.getValue();

        assertThat(saved.getCustomFat())
                .as("AI가 0을 정상 line-total로 줬으면 0 그대로 저장 — null 변환되면 read 단에서 누락으로 오해")
                .isEqualByComparingTo("0");
        assertThat(saved.getCustomSugar()).isEqualByComparingTo("0");
        assertThat(saved.getCustomCarbohydrate()).isEqualByComparingTo("10");

        // path 3 included — 0도 명시값이므로 7개 필드 모두 non-null 조건 충족
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(500);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("80.000");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: PARTIAL true canonical bypass(unit_id 보존)는 candidate 없음 — path 2로 즉시 계산 가능")
    void persist_trueCanonicalBypass_noCandidate() {
        // 시나리오: "마늘 3쪽" + "마늘 1큰술" — 둘 다 unit hit, 같은 ingredient.
        // - 첫 row: MAPPED (ingredient_id + unit_id=쪽)
        // - 둘째 row: bypass branch + matchedUnit=큰술 → ingredient_id=null + unit_id=큰술.id 보존.
        //   isBypassRow()=true. unit→ingredient 경로로 path 2 즉시 계산 가능 → candidate 불필요.
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        IngredientUnit garlicSpoon = unit(11L, garlic, "큰술", "8");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece, garlicSpoon));

        service.persistAll(recipe, List.of(
                raw("마늘", "3", "쪽"),
                raw("마늘", "1", "큰술")
        ), null);

        // candidate는 한 개도 안 만들어져야 — 둘 다 canonical 경로로 처리됨
        verify(ingredientCandidateRepository, never())
                .save(any(com.jdc.recipe_service.domain.entity.IngredientCandidate.class));

        // 두 row 모두 candidate id 없음
        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(2)).save(riCaptor.capture());
        for (RecipeIngredient saved : riCaptor.getAllValues()) {
            assertThat(saved.getIngredientCandidateId())
                    .as("true canonical bypass는 candidate 불필요")
                    .isNull();
        }
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: MAPPED row는 candidate 안 만듦 + AI custom*는 무시 (canonical 우선)")
    void persist_mapped_noCandidateAndCustomIgnored() {
        Ingredient garlic = garlicMaster();
        IngredientUnit garlicPiece = unit(10L, garlic, "쪽", "5");
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of(garlicPiece));

        // AI가 잘못 custom*를 줬어도 MAPPED canonical에서는 무시
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("마늘").quantity("3").customUnit("쪽")
                .customPrice(new BigDecimal("9999"))   // AI 추정 (잘못된 가짜값)
                .customCalories(new BigDecimal("9999"))
                .build();

        service.persistAll(recipe, List.of(dto), null);

        verify(ingredientCandidateRepository, never())
                .save(any(com.jdc.recipe_service.domain.entity.IngredientCandidate.class));

        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(riCaptor.capture());
        RecipeIngredient saved = riCaptor.getValue();
        assertThat(saved.getResolutionStatus()).isEqualTo("MAPPED");
        assertThat(saved.getIngredientCandidateId()).isNull();
        assertThat(saved.getCustomPrice())
                .as("MAPPED canonical row는 AI custom* 무시 → 0/ZERO")
                .isEqualTo(0);
        assertThat(saved.getCustomCalorie()).isEqualByComparingTo("0");

        // canonical 계산: 3쪽 × 5g × 10원/g = 150원, 3쪽 × 5g × 1.5kcal/g = 22.5
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(150);
        assertThat(recipe.getTotalCalories()).isEqualByComparingTo("22.500");
    }

    @Test
    @DisplayName("CUSTOM (customByUser=true)은 candidate 안 만들고 custom* 보존 (status signal만으로 path 3)")
    void persist_customByUser_noCandidateButCustomPreserved() {
        Ingredient garlic = garlicMaster();
        given(ingredientRepository.findAllByIsActiveTrue()).willReturn(List.of(garlic));
        given(ingredientUnitRepository.findAllByIngredientIdIn(any())).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("마늘").quantity("1").customUnit("티스푼")
                .customByUser(true)
                .customPrice(new BigDecimal("500"))
                .customCalories(new BigDecimal("80"))
                .build();

        service.persistAll(recipe, List.of(dto), null);

        verify(ingredientCandidateRepository, never())
                .save(any(com.jdc.recipe_service.domain.entity.IngredientCandidate.class));

        ArgumentCaptor<RecipeIngredient> riCaptor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository).save(riCaptor.capture());
        RecipeIngredient saved = riCaptor.getValue();
        assertThat(saved.getResolutionStatus()).isEqualTo("CUSTOM");
        assertThat(saved.getIngredientCandidateId()).isNull();
        assertThat(saved.getCustomPrice()).isEqualTo(500);

        // status=CUSTOM signal → path 3 included
        assertThat(recipe.getTotalIngredientCost()).isEqualTo(500);
    }
}
