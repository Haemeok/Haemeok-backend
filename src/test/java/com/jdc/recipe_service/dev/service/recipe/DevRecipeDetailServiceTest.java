package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeExtractionInfo;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeExtractionInfoRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeInfoRepository;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto.IngredientCalculationSummary;
import com.jdc.recipe_service.service.RecipeSearchServiceV2;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeDetailService 단위 테스트.
 *
 * 검증 포인트:
 *  1. V2 base 호출 후 dev 신규 필드(model/visibility/lifecycle/listing/source) 추가
 *  2. RecipeYoutubeInfo 있으면 youtubeInfo 포함, 없으면 null (frontend가 base.youtube_* fallback)
 *  3. RecipeYoutubeExtractionInfo 있으면 extractionInfo 포함, 없으면 null
 *  4. Recipe 없으면 RECIPE_NOT_FOUND (V2 base 호출 후라도 dev 단계에서 거부)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeDetailServiceTest {

    @Mock RecipeSearchServiceV2 recipeSearchServiceV2;
    @Mock RecipeRepository recipeRepository;
    @Mock RecipeIngredientRepository recipeIngredientRepository;
    @Mock IngredientUnitRepository ingredientUnitRepository;
    @Mock RecipeYoutubeInfoRepository youtubeInfoRepository;
    @Mock RecipeYoutubeExtractionInfoRepository extractionInfoRepository;
    @Spy RecipeIngredientDisplayResolver displayResolver = new RecipeIngredientDisplayResolver();
    @Spy RecipeIngredientCalculator calculator = new RecipeIngredientCalculator();

    @InjectMocks DevRecipeDetailService service;

    private static final Long RECIPE_ID = 100L;
    private static final Long USER_ID = 1L;

    private RecipeDetailStaticDto baseDto;
    private Recipe recipe;

    @BeforeEach
    void setUp() {
        baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID)
                .title("Test Recipe")
                .youtubeUrl("https://www.youtube.com/watch?v=legacy")
                .youtubeChannelName("legacy channel")
                .build();

        recipe = Recipe.builder()
                .id(RECIPE_ID)
                .imageGenerationModel("gemini-2.5-flash-image")
                .visibility(RecipeVisibility.PUBLIC)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.YOUTUBE)
                .build();
    }

    @Test
    @DisplayName("YouTube 분리 테이블 + 추출 info 모두 있을 때: 모든 dev 필드 포함")
    void happyPath_allDevFieldsIncluded() {
        // given
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        RecipeYoutubeInfo info = RecipeYoutubeInfo.builder()
                .videoId("dQw4w9WgXcQ")
                .youtubeUrl("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
                .channelName("new channel")
                .channelId("UC_xxx")
                .videoTitle("video title")
                .thumbnailUrl("https://thumb")
                .channelProfileUrl("https://profile")
                .subscriberCount(1000L)
                .videoViewCount(5000L)
                .durationSeconds(180L)
                .extractorId(USER_ID)
                .build();
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.of(info));

        RecipeYoutubeExtractionInfo extractionInfo = RecipeYoutubeExtractionInfo.builder()
                .hasSubtitle(true)
                .hasDescriptionIngredient(true)
                .hasCommentIngredient(false)
                .usedGeminiAnalysis(false)
                .evidenceLevel(EvidenceLevel.HIGH)
                .tokenCost(2)
                .build();
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.of(extractionInfo));

        // when
        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        // then - V2 base는 그대로
        assertThat(result.getBase()).isSameAs(baseDto);
        // dev 신규 (Recipe entity)
        assertThat(result.getImageGenerationModel()).isEqualTo("gemini-2.5-flash-image");
        assertThat(result.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(result.getLifecycleStatus()).isEqualTo(RecipeLifecycleStatus.ACTIVE);
        assertThat(result.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(result.getSource()).isEqualTo(RecipeSourceType.YOUTUBE);
        // youtubeInfo (분리 테이블)
        assertThat(result.getYoutubeInfo()).isNotNull();
        assertThat(result.getYoutubeInfo().getVideoId()).isEqualTo("dQw4w9WgXcQ");
        assertThat(result.getYoutubeInfo().getDurationSeconds()).isEqualTo(180L);
        assertThat(result.getYoutubeInfo().getChannelName()).isEqualTo("new channel");
        // extractionInfo
        assertThat(result.getExtractionInfo()).isNotNull();
        assertThat(result.getExtractionInfo().getEvidenceLevel()).isEqualTo(EvidenceLevel.HIGH);
        assertThat(result.getExtractionInfo().getTokenCost()).isEqualTo(2);
        assertThat(result.getExtractionInfo().isHasSubtitle()).isTrue();
        assertThat(result.getExtractionInfo().isUsedGeminiAnalysis()).isFalse();
    }

    @Test
    @DisplayName("RecipeYoutubeInfo 없으면 youtubeInfo=null (frontend가 base.youtube_* fallback 사용)")
    void noYoutubeInfo_falsbackToLegacy() {
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        assertThat(result.getYoutubeInfo()).isNull();
        // base의 legacy youtube_* 값은 그대로
        assertThat(result.getBase().getYoutubeUrl()).isEqualTo("https://www.youtube.com/watch?v=legacy");
    }

    @Test
    @DisplayName("USER 출처 (추출 안 됨) → extractionInfo=null")
    void userSource_noExtractionInfo() {
        Recipe userRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .visibility(RecipeVisibility.PUBLIC)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)  // 사용자 직접 작성
                .build();
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(userRecipe));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        assertThat(result.getExtractionInfo()).isNull();
        assertThat(result.getSource()).isEqualTo(RecipeSourceType.USER);
        assertThat(result.getImageGenerationModel()).isNull(); // 사용자 업로드는 모델 정보 없음
    }

    @Test
    @DisplayName("Recipe 없으면 RECIPE_NOT_FOUND (V2 호출 전에 단축)")
    void recipeNotFound() {
        // findById가 먼저 호출되므로 V2 stub은 불필요 (V2 호출 자체가 안 됨)
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> service.getRecipeDetail(RECIPE_ID, USER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        // reorder 의도 잠금: V2 호출 자체가 발생하지 않았음을 명시 검증
        // (만약 코드가 다시 V2 선호출로 회귀하면 이 verify가 실패)
        verify(recipeSearchServiceV2, never()).getRecipeDetail(any(), any());
    }

    @Test
    @DisplayName("비로그인 (currentUserId=null)도 V2 권한 체크 그대로 거쳐 정상 조회 (공개 레시피)")
    void anonymousUser_callsV2WithNullUserId() {
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, null)).willReturn(baseDto);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, null);

        assertThat(result).isNotNull();
        assertThat(result.getBase()).isSameAs(baseDto);
    }

    // ---------- visibility 방어 (isPrivate-visibility 불일치 데이터 차단) ----------

    @Test
    @DisplayName("visibility=PRIVATE + isPrivate=false (불일치 데이터): 비로그인 → RECIPE_PRIVATE_ACCESS_DENIED (V2 호출 전에 차단)")
    void privateVisibility_anonymousAccess_throws() {
        // 비로그인 case: isOwner는 currentUserId=null로 즉시 false 반환 → owner.getId() 호출 안 됨 (stub 불필요)
        // V2도 호출 안 됨 (visibility 가드가 V2 호출 전에 수행)
        Recipe privateRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .visibility(RecipeVisibility.PRIVATE)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.USER)
                .isPrivate(false)  // 잘못된 데이터: V2 legacy로는 통과되는 상태
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(privateRecipe));

        assertThatThrownBy(() -> service.getRecipeDetail(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        // reorder 의도 잠금: V2 호출 자체가 발생하지 않았음을 명시 검증
        verify(recipeSearchServiceV2, never()).getRecipeDetail(any(), any());
    }

    @Test
    @DisplayName("visibility=PRIVATE + 다른 사용자 (로그인) → RECIPE_PRIVATE_ACCESS_DENIED (V2 호출 전에 차단)")
    void privateVisibility_nonOwnerAccess_throws() {
        // V2는 호출되지 않으므로 stub 불필요 (visibility 가드가 V2 호출 전에 수행)
        User owner = mock(User.class);
        given(owner.getId()).willReturn(99L);  // 소유자는 99 (비교용)
        Recipe privateRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PRIVATE)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.USER)
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(privateRecipe));

        // currentUser=1, owner=99 → 비소유자
        assertThatThrownBy(() -> service.getRecipeDetail(RECIPE_ID, USER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        // reorder 의도 잠금: V2 호출 자체가 발생하지 않았음을 명시 검증
        verify(recipeSearchServiceV2, never()).getRecipeDetail(any(), any());
    }

    @Test
    @DisplayName("visibility=PRIVATE + owner 본인 → 정상 조회")
    void privateVisibility_ownerAccess_succeeds() {
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);

        User owner = mock(User.class);
        given(owner.getId()).willReturn(USER_ID);  // 본인 소유
        Recipe privateRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PRIVATE)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.USER)
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(privateRecipe));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        assertThat(result.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
    }

    // ─── 1.2 raw-first 표시 + ingredientCalculationSummary ─────────────────────

    /** per-g 7개 모두 채워진 마스터 — Calculator path 1/2 included 조건 충족. */
    private static com.jdc.recipe_service.domain.entity.Ingredient garlicWithPerGram() {
        return com.jdc.recipe_service.domain.entity.Ingredient.builder()
                .id(1L).name("마늘")
                .kcalPerG(new BigDecimal("1.5"))
                .pricePerG(new BigDecimal("10"))
                .carbohydrateGPerG(new BigDecimal("0.3"))
                .proteinGPerG(new BigDecimal("0.06"))
                .fatGPerG(new BigDecimal("0.005"))
                .sugarGPerG(new BigDecimal("0.01"))
                .sodiumMgPerG(new BigDecimal("0.02"))
                .build();
    }

    private static IngredientUnit unit(Long id, com.jdc.recipe_service.domain.entity.Ingredient ing,
                                        String label, String edibleGrams) {
        return IngredientUnit.builder()
                .id(id).ingredient(ing)
                .unitLabelKo(label).normalizedUnitLabel(label)
                .gramsPerUnit(new BigDecimal(edibleGrams))
                .edibleGramsPerUnit(new BigDecimal(edibleGrams))
                .isDefault(false)
                .build();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 재료 표시는 raw_name/raw_quantity_text/raw_unit_text 우선 (V2 legacy 표시 위에 덮어씀)")
    void rawFirstDisplay_overridesV2Mapping() {
        // V2가 만든 base ingredient: name="마늘"(legacy), quantity="3", unit="쪽"
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .id(1L)
                .name("마늘")
                .quantity("3")
                .unit("쪽")
                .build();
        List<RecipeIngredientDto> v2Ingredients = new ArrayList<>(List.of(v2Dto));
        RecipeDetailStaticDto baseWithIngredients = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test").ingredients(v2Ingredients).build();

        // 엔티티는 raw_*를 다른 값으로 보존 (사용자가 직접 입력한 자연어를 살리는 케이스)
        RecipeIngredient entity = RecipeIngredient.builder()
                .id(101L)
                .ingredient(garlicWithPerGram())
                .quantity("3").unit("쪽")
                .rawName("마늘 (큰 알)")
                .rawQuantityText("3")
                .rawUnitText("쪽")
                .resolutionStatus("MAPPED")
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseWithIngredients);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(entity));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        service.getRecipeDetail(RECIPE_ID, USER_ID);

        // base.ingredients가 raw-first로 in-place 덮어써졌는지
        RecipeIngredientDto mutated = baseWithIngredients.getIngredients().get(0);
        assertThat(mutated.getName()).isEqualTo("마늘 (큰 알)");
    }

    @Test
    @DisplayName("ingredientCalculationSummary: MAPPED 라인 1개에 대해 per-g 계산 결과가 노출된다")
    void summary_containsCalculationForMappedLine() {
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .name("마늘").quantity("3").unit("쪽").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(v2Dto))).build();

        IngredientUnit garlicPiece = unit(10L, garlicWithPerGram(), "쪽", "5");
        RecipeIngredient entity = RecipeIngredient.builder()
                .id(101L)
                .ingredient(garlicWithPerGram())
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .normalizedGrams(new BigDecimal("15"))   // 3쪽 × 5g
                .resolutionStatus("MAPPED")
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(entity));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicPiece));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary).isNotNull();
        assertThat(summary.getMappedCount()).isEqualTo(1);
        assertThat(summary.getCalculatedCount()).isEqualTo(1);
        assertThat(summary.getPendingCount()).isZero();
        // 15g × 1.5 kcal/g = 22.5
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("22.500");
        // 15g × 10원/g = 150
        assertThat(summary.getTotalIngredientCost()).isEqualTo(150L);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: C' bypass row(ingredient_id=null + unit_id 보존)도 unit→ingredient로 calc에 포함된다")
    void summary_bypassRow_calculatedViaUnitOwner() {
        // V2 base: 두 라인 (마늘 3쪽, 마늘 1큰술)
        RecipeIngredientDto dto1 = RecipeIngredientDto.builder().name("마늘").quantity("3").unit("쪽").build();
        RecipeIngredientDto dto2 = RecipeIngredientDto.builder().name("마늘").quantity("1").unit("큰술").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(dto1, dto2))).build();

        IngredientUnit garlicPiece = unit(10L, garlicWithPerGram(), "쪽", "5");
        IngredientUnit garlicSpoon = unit(11L, garlicWithPerGram(), "큰술", "8");

        // 첫 라인: MAPPED (ingredient_id=1)
        RecipeIngredient mapped = RecipeIngredient.builder()
                .id(101L)
                .ingredient(garlicWithPerGram())
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .normalizedGrams(new BigDecimal("15"))
                .resolutionStatus("MAPPED")
                .build();
        // 두 번째 라인: C' bypass (ingredient_id=null, ingredient_unit_id=11 살아있음)
        RecipeIngredient bypass = RecipeIngredient.builder()
                .id(102L)
                .ingredient(null)                         // C' bypass: UNIQUE 회피
                .quantity("1").unit("큰술")
                .customName("마늘").customUnit("큰술")
                .rawName("마늘").rawQuantityText("1").rawUnitText("큰술")
                .ingredientUnitId(11L)                    // unit은 보존
                .normalizedGrams(new BigDecimal("8"))
                .resolutionStatus("PARTIAL")
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mapped, bypass));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicPiece, garlicSpoon));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary.getMappedCount()).isEqualTo(1);
        assertThat(summary.getPartialCount())
                .as("bypass row는 PARTIAL로 카운트")
                .isEqualTo(1);
        assertThat(summary.getCalculatedCount())
                .as("bypass row도 calc에 포함되어야 한다 (unit→ingredient 복원)")
                .isEqualTo(2);
        assertThat(summary.getPendingCount()).isZero();
        // 15g + 8g = 23g × 1.5 = 34.5
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("34.500");
        // 15g + 8g = 23g × 10 = 230
        assertThat(summary.getTotalIngredientCost()).isEqualTo(230L);
    }

    @Test
    @DisplayName("UNRESOLVED 라인은 pending으로 카운트되고 합계에 포함되지 않는다 (0 안 삼킴)")
    void summary_unresolvedIsPendingNotZero() {
        RecipeIngredientDto dto1 = RecipeIngredientDto.builder().name("마늘").quantity("3").unit("쪽").build();
        RecipeIngredientDto dto2 = RecipeIngredientDto.builder().name("황태머리").quantity("1").unit("마리").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(dto1, dto2))).build();

        IngredientUnit garlicPiece = unit(10L, garlicWithPerGram(), "쪽", "5");

        RecipeIngredient mapped = RecipeIngredient.builder()
                .id(101L).ingredient(garlicWithPerGram())
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .normalizedGrams(new BigDecimal("15"))
                .resolutionStatus("MAPPED").build();
        RecipeIngredient unresolved = RecipeIngredient.builder()
                .id(102L)
                .ingredient(null)
                .quantity("1").unit("마리")
                .customName("황태머리")
                .rawName("황태머리").rawQuantityText("1").rawUnitText("마리")
                .resolutionStatus("UNRESOLVED").build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(mapped, unresolved));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicPiece));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary.getMappedCount()).isEqualTo(1);
        assertThat(summary.getUnresolvedCount()).isEqualTo(1);
        assertThat(summary.getCalculatedCount()).isEqualTo(1);
        assertThat(summary.getPendingCount())
                .as("UNRESOLVED 라인은 0이 아니라 pending — UI가 미반영을 알 수 있게")
                .isEqualTo(1);
        // 합계는 mapped만 (15g × 1.5 = 22.5)
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("22.500");
    }

    @Test
    @DisplayName("기존 base.totalCalories는 유지된다 (V2 legacy 계산값 그대로) — dev summary가 별도로 노출")
    void baseTotals_preserved_devSummaryAdded() {
        // V2 base의 totalCalories는 기존 계산 결과
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .totalCalories(999.0)            // V2 legacy 계산
                .totalIngredientCost(8888)
                .ingredients(new ArrayList<>(List.of(
                        RecipeIngredientDto.builder().name("마늘").quantity("3").unit("쪽").build())))
                .build();

        IngredientUnit garlicPiece = unit(10L, garlicWithPerGram(), "쪽", "5");
        RecipeIngredient entity = RecipeIngredient.builder()
                .id(101L).ingredient(garlicWithPerGram())
                .quantity("3").unit("쪽")
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .normalizedGrams(new BigDecimal("15"))
                .resolutionStatus("MAPPED").build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(entity));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicPiece));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        // base.totalCalories는 V2 값 그대로 유지
        assertThat(result.getBase().getTotalCalories()).isEqualTo(999.0);
        assertThat(result.getBase().getTotalIngredientCost()).isEqualTo(8888);
        // 새 summary는 별도로 새 계산값 (22.5)
        assertThat(result.getIngredientCalculationSummary().getTotalCalories()).isEqualByComparingTo("22.500");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 여러 라인을 ID 오름차순으로 받았을 때 raw-first가 올바른 DTO에 매핑된다 (인덱스 misalignment 방지)")
    void rawFirstDisplay_multipleLines_correctIndexAlignment() {
        // V2 base의 ingredient DTO 3개 — id ASC 순으로 정렬되어 들어옴
        RecipeIngredientDto dtoA = RecipeIngredientDto.builder().name("A_legacy").quantity("1").unit("개").build();
        RecipeIngredientDto dtoB = RecipeIngredientDto.builder().name("B_legacy").quantity("2").unit("개").build();
        RecipeIngredientDto dtoC = RecipeIngredientDto.builder().name("C_legacy").quantity("3").unit("개").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(dtoA, dtoB, dtoC))).build();

        // 엔티티도 같은 ID ASC 순서 (findByRecipeId가 ORDER BY ri.id ASC를 보장)
        RecipeIngredient eA = RecipeIngredient.builder()
                .id(101L).ingredient(garlicWithPerGram())
                .quantity("1").unit("개")
                .rawName("RAW_A").rawQuantityText("1").rawUnitText("개")
                .resolutionStatus("MAPPED").build();
        RecipeIngredient eB = RecipeIngredient.builder()
                .id(102L).ingredient(garlicWithPerGram())
                .quantity("2").unit("개")
                .rawName("RAW_B").rawQuantityText("2").rawUnitText("개")
                .resolutionStatus("MAPPED").build();
        RecipeIngredient eC = RecipeIngredient.builder()
                .id(103L).ingredient(garlicWithPerGram())
                .quantity("3").unit("개")
                .rawName("RAW_C").rawQuantityText("3").rawUnitText("개")
                .resolutionStatus("MAPPED").build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(eA, eB, eC));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        service.getRecipeDetail(RECIPE_ID, USER_ID);

        // 각 DTO에 대응되는 entity의 rawName이 정확히 매핑되어야 한다
        // 만약 V2/dev가 다른 순서를 받으면(ORDER BY 누락 시) 이 assertion이 깨지면서 회귀를 알린다
        List<RecipeIngredientDto> result = baseDto.getIngredients();
        assertThat(result.get(0).getName()).isEqualTo("RAW_A");
        assertThat(result.get(1).getName()).isEqualTo("RAW_B");
        assertThat(result.get(2).getName()).isEqualTo("RAW_C");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: resolutionStatus가 null/dirty여도 C' bypass row(unit_id 보존)는 PARTIAL로 분류되어 calc에 포함된다")
    void parseStatus_bypassRow_withDirtyStatus_classifiedAsPartial() {
        // dirty status 시뮬레이션: customName=rawName + ingredient_unit_id 보존된 bypass row인데
        // resolution_status가 null. 이전 fallback은 customName 우선 검사로 CUSTOM 분류 → calc 미작동.
        // 새 fallback은 unit_id 우선 → PARTIAL → unit→ingredient 복원으로 calc 정상 작동.
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .name("마늘").quantity("1").unit("큰술").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(v2Dto))).build();

        IngredientUnit garlicSpoon = unit(11L, garlicWithPerGram(), "큰술", "8");
        RecipeIngredient bypassWithDirtyStatus = RecipeIngredient.builder()
                .id(102L)
                .ingredient(null)                        // C' bypass: UNIQUE 회피
                .quantity("1").unit("큰술")
                .customName("마늘").customUnit("큰술")
                .rawName("마늘").rawQuantityText("1").rawUnitText("큰술")
                .ingredientUnitId(11L)                   // unit 보존 → calc 가능
                .normalizedGrams(new BigDecimal("8"))
                .resolutionStatus(null)                  // ★ dirty: 1차 backfill에서 누락된 케이스 시뮬레이션
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(bypassWithDirtyStatus));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicSpoon));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary.getPartialCount())
                .as("dirty status여도 unit_id 보존된 bypass row는 PARTIAL로 분류되어야 한다")
                .isEqualTo(1);
        assertThat(summary.getCustomCount())
                .as("CUSTOM으로 빨려 들어가면 calc path 2를 놓침 — 회귀 잠금")
                .isZero();
        assertThat(summary.getCalculatedCount())
                .as("PARTIAL → unit→ingredient 복원으로 calc 포함")
                .isEqualTo(1);
        assertThat(summary.getPendingCount()).isZero();
        // 8g × 1.5 = 12 kcal, 8g × 10원 = 80원
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("12.000");
        assertThat(summary.getTotalIngredientCost()).isEqualTo(80L);
    }

    @Test
    @DisplayName("parseStatus: resolutionStatus 케이스/공백 흔들림(\" partial \", \"Partial\") 모두 PARTIAL로 정규화")
    void parseStatus_caseAndWhitespaceTolerant() {
        // 한 라인에 \" partial \" — trim+toUpperCase로 PARTIAL 인식되어야 함
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .name("마늘").quantity("1").unit("큰술").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(v2Dto))).build();

        IngredientUnit garlicSpoon = unit(11L, garlicWithPerGram(), "큰술", "8");
        RecipeIngredient dirty = RecipeIngredient.builder()
                .id(102L).ingredient(null)
                .quantity("1").unit("큰술")
                .customName("마늘").customUnit("큰술")
                .rawName("마늘").rawQuantityText("1").rawUnitText("큰술")
                .ingredientUnitId(11L)
                .normalizedGrams(new BigDecimal("8"))
                .resolutionStatus(" partial ")           // 케이스/공백 흔들림
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(dirty));
        given(ingredientUnitRepository.findAllByIdIn(any())).willReturn(List.of(garlicSpoon));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        assertThat(result.getIngredientCalculationSummary().getPartialCount()).isEqualTo(1);
        assertThat(result.getIngredientCalculationSummary().getCalculatedCount()).isEqualTo(1);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: candidate id 박힌 row + customFat=0/customSugar=0 정상 명시값 → included (read 단이 0을 신뢰)")
    void summary_aiFallbackRow_explicitZeroMacros_included() {
        // AI fallback row가 candidate id로 의미 있는 fallback임을 표시했고, 일부 macro를 0으로 정상 명시.
        // toCalculationInput의 trustExplicitZero 분기가 candidate id != null이면 nonZeroOrNull 변환을 풀어
        // 0 그대로 calculator로 전달 → 7개 필드 모두 non-null 조건 충족 → included.
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .name("황태머리").quantity("1").unit("개").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(v2Dto))).build();

        // AI fallback row: candidate id 박힘 + 7개 custom 필드 명시 (fat=0, sugar=0이 정상값)
        RecipeIngredient aiFallback = RecipeIngredient.builder()
                .id(201L).ingredient(null)
                .quantity("1").unit("개")
                .customName("황태머리").customUnit("개")
                .rawName("황태머리").rawQuantityText("1").rawUnitText("개")
                .resolutionStatus("UNRESOLVED")
                .ingredientCandidateId(5001L)               // ★ AI fallback signal
                .customCalorie(new BigDecimal("80"))
                .customPrice(500)
                .customCarbohydrate(new BigDecimal("12.0"))
                .customProtein(new BigDecimal("3.0"))
                .customFat(BigDecimal.ZERO)                 // ★ AI가 "0g" 정상 명시
                .customSugar(BigDecimal.ZERO)               // ★ AI가 "0g" 정상 명시
                .customSodium(new BigDecimal("200"))
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(aiFallback));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary.getCalculatedCount())
                .as("**핵심**: candidate id 박힌 row의 0은 정상 명시값으로 신뢰 — included")
                .isEqualTo(1);
        assertThat(summary.getPendingCount())
                .as("0이 누락으로 오해되어 pending되면 회귀")
                .isZero();
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("80.000");
        assertThat(summary.getTotalIngredientCost()).isEqualTo(500L);
        assertThat(summary.getTotalFat())
                .as("AI가 명시한 0이 read 단까지 보존")
                .isEqualByComparingTo("0.000");
        assertThat(summary.getTotalSugar()).isEqualByComparingTo("0.000");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: candidate id 없는 legacy row + custom defaults all-zero → pending (0이 누락 가능성이라 보호)")
    void summary_legacyRowAllZeroDefaults_pending() {
        // candidate id 없는 row — legacy 코드가 default ZERO로 저장한 케이스 시뮬레이션.
        // 이 row는 0이 "AI 명시값"이 아니라 "누락"일 가능성이 있어 nonZeroOrNull로 보호 → calculator는 null 받음 →
        // hasFallbackSignal도 false (candidate=null + status=UNRESOLVED)이므로 pending.
        RecipeIngredientDto v2Dto = RecipeIngredientDto.builder()
                .name("legacy재료").quantity("1").unit("개").build();
        RecipeDetailStaticDto baseDto = RecipeDetailStaticDto.builder()
                .id(RECIPE_ID).title("Test")
                .ingredients(new ArrayList<>(List.of(v2Dto))).build();

        RecipeIngredient legacy = RecipeIngredient.builder()
                .id(301L).ingredient(null)
                .quantity("1").unit("개")
                .customName("legacy재료").customUnit("개")
                .rawName("legacy재료").rawQuantityText("1").rawUnitText("개")
                .resolutionStatus("UNRESOLVED")
                // ingredientCandidateId 없음 (null)
                .customCalorie(BigDecimal.ZERO)             // legacy default
                .customPrice(0)                              // legacy default
                .customCarbohydrate(BigDecimal.ZERO)
                .customProtein(BigDecimal.ZERO)
                .customFat(BigDecimal.ZERO)
                .customSugar(BigDecimal.ZERO)
                .customSodium(BigDecimal.ZERO)
                .build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(legacy));
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary.getCalculatedCount())
                .as("legacy row의 default ZERO는 0원/0kcal로 합산되면 안 됨")
                .isZero();
        assertThat(summary.getPendingCount())
                .as("**핵심**: candidate id 없는 0은 누락 가능성으로 보호 → pending")
                .isEqualTo(1);
        assertThat(summary.getTotalIngredientCost()).isEqualTo(0L);
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("0.000");
    }

    @Test
    @DisplayName("ingredients가 비어있으면 summary는 zero/zero (pendingCount=0, calculatedCount=0)")
    void summary_emptyIngredients_returnsZeros() {
        // 기본 setUp의 baseDto는 ingredients 없음
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipeSearchServiceV2.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(baseDto);
        given(youtubeInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());
        given(extractionInfoRepository.findByRecipeId(RECIPE_ID)).willReturn(Optional.empty());

        DevRecipeDetailDto result = service.getRecipeDetail(RECIPE_ID, USER_ID);

        IngredientCalculationSummary summary = result.getIngredientCalculationSummary();
        assertThat(summary).isNotNull();
        assertThat(summary.getMappedCount()).isZero();
        assertThat(summary.getCalculatedCount()).isZero();
        assertThat(summary.getPendingCount()).isZero();
        assertThat(summary.getTotalCalories()).isEqualByComparingTo("0.000");
    }
}
