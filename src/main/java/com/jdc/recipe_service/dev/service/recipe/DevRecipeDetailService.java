package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto.ExtractionInfoDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto.IngredientCalculationSummary;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto.YoutubeInfoDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeExtractionInfo;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeExtractionInfoRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeInfoRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeSearchServiceV2;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalcInputMapper;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationSummary;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver.DisplayLine;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver.LineDisplayInput;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 레시피 상세 조회.
 *
 * 흐름:
 *  1. Recipe entity 조회 + visibility 가드 (V2 호출 전 단축)
 *  2. V2 base 호출 → base 응답 확보 (모든 V2 필드, legacy 계산 totals 포함)
 *  3. RecipeIngredient 엔티티 재로딩 → raw-first 표시 적용 + Calculator summary 산출 (1.2 신규)
 *  4. RecipeYoutubeInfo / RecipeYoutubeExtractionInfo (분리 테이블) 조회
 *  5. 위 모두 합쳐 DevRecipeDetailDto 반환
 *
 * <p>1.2 raw-first 표시: V2의 legacy quantity/unit/customName 중심 매핑 위에 raw_*가 있으면
 * raw 우선으로 in-place 재기록. base.ingredients DTO 인스턴스의 setName/setQuantity/setUnit만
 * 갈아낀다 — 다른 필드(price/calories/coupangLink/custom*)는 V2 그대로.
 *
 * <p>1.2 summary: Calculator를 통해 새 정책(per-g 7-필수, custom kcal+price 둘 다 필수,
 * pending 0 안 삼킴)으로 다시 계산. base.totalCalories/totalIngredientCost는 V2 legacy 계산
 * 그대로 유지하고 새 결과는 별도 summary에 노출.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeDetailService {

    private final RecipeSearchServiceV2 recipeSearchServiceV2;
    private final RecipeRepository recipeRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final IngredientUnitRepository ingredientUnitRepository;
    private final RecipeYoutubeInfoRepository youtubeInfoRepository;
    private final RecipeYoutubeExtractionInfoRepository extractionInfoRepository;
    private final RecipeIngredientDisplayResolver displayResolver;
    private final RecipeIngredientCalculator calculator;

    @Transactional(readOnly = true)
    public DevRecipeDetailDto getRecipeDetail(Long recipeId, Long currentUserId) {
        // 1. Recipe entity 먼저 조회 — visibility 가드를 V2 상세 조립 쿼리 전에 수행 (불필요한 비용 회피)
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        // 1a. Dev 추가 권한 가드 — V2의 legacy isPrivate 판정과 별개로 새 visibility를 source of truth로 사용.
        if (recipe.getVisibility() == RecipeVisibility.PRIVATE && !isOwner(recipe, currentUserId)) {
            throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
        }

        // 2. V2 base 호출 (V2의 권한/공개 판정 + 상세 조립)
        RecipeDetailStaticDto base = recipeSearchServiceV2.getRecipeDetail(recipeId, currentUserId);

        // 3. raw-first 표시 + 재계산 summary
        IngredientCalculationSummary calcSummary = applyRawFirstAndComputeSummary(recipeId, base);

        // 4. 분리 테이블 — 있으면 우선
        YoutubeInfoDto youtubeInfo = youtubeInfoRepository.findByRecipeId(recipeId)
                .map(DevRecipeDetailService::toYoutubeInfoDto)
                .orElse(null);

        ExtractionInfoDto extractionInfo = extractionInfoRepository.findByRecipeId(recipeId)
                .map(DevRecipeDetailService::toExtractionInfoDto)
                .orElse(null);

        return DevRecipeDetailDto.builder()
                .base(base)
                .imageGenerationModel(recipe.getImageGenerationModel())
                .visibility(recipe.getVisibility())
                .lifecycleStatus(recipe.getLifecycleStatus())
                .listingStatus(recipe.getListingStatus())
                .source(recipe.getSource())
                .youtubeInfo(youtubeInfo)
                .extractionInfo(extractionInfo)
                .ingredientCalculationSummary(calcSummary)
                .build();
    }

    /**
     * 1.2 raw-first 표시 + 새 계산 summary.
     *
     * <p>V2 base의 ingredients DTO 인스턴스를 in-place 수정 (RecipeIngredientDto는 @Data → setter 존재).
     * V2 mapper가 customName=null 커스텀 row를 필터링하는 점을 동일 규칙으로 정렬해서 인덱스 매칭.
     *
     * <p>Calculator 입력 빌드 시 ingredient_unit_id를 batch prefetch (LAZY 회피).
     */
    private IngredientCalculationSummary applyRawFirstAndComputeSummary(Long recipeId,
                                                                         RecipeDetailStaticDto base) {
        List<RecipeIngredient> entities = recipeIngredientRepository.findByRecipeId(recipeId);
        if (entities == null) entities = List.of();

        // unit 배치 prefetch — JOIN FETCH ingredient
        Set<Long> unitIds = entities.stream()
                .map(RecipeIngredient::getIngredientUnitId)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<Long, IngredientUnit> unitsById = unitIds.isEmpty()
                ? Collections.emptyMap()
                : ingredientUnitRepository.findAllByIdIn(unitIds).stream()
                        .collect(Collectors.toMap(IngredientUnit::getId, u -> u));

        // raw-first in-place 적용. V2 mapper의 필터 규칙(custom-without-name 스킵)에 맞춰 인덱스 정렬.
        List<RecipeIngredientDto> baseIngredients = base.getIngredients();
        if (baseIngredients != null && !baseIngredients.isEmpty()) {
            int dtoIdx = 0;
            for (RecipeIngredient entity : entities) {
                if (v2WouldFilter(entity)) continue;
                if (dtoIdx >= baseIngredients.size()) break;

                RecipeIngredientDto dto = baseIngredients.get(dtoIdx);
                DisplayLine display = displayResolver.resolve(toDisplayInput(entity));
                if (display.name() != null) dto.setName(display.name());
                if (display.quantity() != null) dto.setQuantity(display.quantity());
                if (display.unit() != null) dto.setUnit(display.unit());

                dtoIdx++;
            }
        }

        // Calculator 호출 — 모든 entity가 대상 (V2 필터된 row도 카운트에 포함)
        List<CalculationLineInput> calcInputs = entities.stream()
                .map(e -> toCalculationInput(e, unitsById))
                .toList();
        CalculationSummary summary = calculator.summarize(calcInputs);
        return toSummaryDto(summary);
    }

    /**
     * V2 mapper 동일 필터: custom row이지만 customName이 없으면 표시 불가 → DTO 생성 안 됨.
     * dev raw-first 표시 인덱스 정렬에 사용.
     */
    private static boolean v2WouldFilter(RecipeIngredient entity) {
        if (entity == null) return true;
        return entity.getIngredient() == null && entity.getCustomName() == null;
    }

    private static LineDisplayInput toDisplayInput(RecipeIngredient entity) {
        Ingredient ing = entity.getIngredient();
        return new LineDisplayInput(
                entity.getRawName(),
                entity.getCustomName(),
                ing != null ? ing.getName() : null,
                entity.getRawQuantityText(),
                entity.getQuantity(),
                entity.getRawUnitText(),
                entity.getCustomUnit(),
                entity.getUnit()
        );
    }

    /**
     * RecipeIngredient → CalculationLineInput.
     *
     * <ul>
     *   <li>resolution_status가 String이라 enum으로 매핑. 미설정/알 수 없는 값은 ingredient_id 유무로
     *       MAPPED/UNRESOLVED 추정 (백필 1차에서 PARTIAL/CUSTOM이 일관 박혀있을 수 있어 추정 fallback).</li>
     *   <li>C' bypass 식별: ingredient_id=null + ingredient_unit_id != null + status=PARTIAL →
     *       unit→ingredient로 unitOwnerIngredient 채움.</li>
     * </ul>
     */
    private static CalculationLineInput toCalculationInput(RecipeIngredient e, Map<Long, IngredientUnit> unitsById) {
        // 변환 정책은 RecipeIngredientCalcInputMapper에 위임 — aggregate 백필도 같은 mapper를 사용해
        // detail 화면 합산값과 recipe aggregate(목록 검색/필터)가 어긋나지 않도록 한다.
        return RecipeIngredientCalcInputMapper.toInput(e, unitsById);
    }

    private static IngredientCalculationSummary toSummaryDto(CalculationSummary s) {
        return IngredientCalculationSummary.builder()
                .totalCalories(s.totalCalories())
                .totalIngredientCost(s.totalIngredientCost())
                .totalCarbohydrate(s.totalCarbohydrate())
                .totalProtein(s.totalProtein())
                .totalFat(s.totalFat())
                .totalSugar(s.totalSugar())
                .totalSodium(s.totalSodium())
                .mappedCount(s.mappedCount())
                .partialCount(s.partialCount())
                .unresolvedCount(s.unresolvedCount())
                .customCount(s.customCount())
                .calculatedCount(s.calculatedCount())
                .pendingCount(s.pendingCount())
                .build();
    }

    /**
     * Recipe 작성자가 currentUser인지 판정. recipe.user는 LAZY라 id만 비교 (불필요한 join 회피).
     */
    private static boolean isOwner(Recipe recipe, Long currentUserId) {
        if (currentUserId == null) return false;
        return recipe.getUser() != null && currentUserId.equals(recipe.getUser().getId());
    }

    private static YoutubeInfoDto toYoutubeInfoDto(RecipeYoutubeInfo info) {
        return YoutubeInfoDto.builder()
                .videoId(info.getVideoId())
                .youtubeUrl(info.getYoutubeUrl())
                .channelName(info.getChannelName())
                .channelId(info.getChannelId())
                .videoTitle(info.getVideoTitle())
                .thumbnailUrl(info.getThumbnailUrl())
                .channelProfileUrl(info.getChannelProfileUrl())
                .subscriberCount(info.getSubscriberCount())
                .videoViewCount(info.getVideoViewCount())
                .durationSeconds(info.getDurationSeconds())
                .extractorId(info.getExtractorId())
                .build();
    }

    private static ExtractionInfoDto toExtractionInfoDto(RecipeYoutubeExtractionInfo info) {
        return ExtractionInfoDto.builder()
                .hasSubtitle(info.isHasSubtitle())
                .hasDescriptionIngredient(info.isHasDescriptionIngredient())
                .hasCommentIngredient(info.isHasCommentIngredient())
                .usedGeminiAnalysis(info.isUsedGeminiAnalysis())
                .evidenceLevel(info.getEvidenceLevel())
                .tokenCost(info.getTokenCost())
                .build();
    }
}
