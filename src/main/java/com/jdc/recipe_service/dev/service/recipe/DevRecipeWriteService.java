package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeService;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

/**
 * Dev V3 recipe write (create / update / delete) dispatcher.
 *
 * <h3>1.3: ingredient 저장 파이프라인 교체 (empty-then-save + step relink)</h3>
 * 운영 {@link RecipeService#createRecipeAndGenerateUrls}과 {@link RecipeService#updateUserRecipe}는
 * ingredient 저장 + dedupe + saveAll + step ingredient FK 매핑 + aggregate 산출이 한데 얽혀 있다.
 * dev는 raw 보존 + per-g 기반 새 정책으로 가야 하므로 이 영역만 분리한다.
 *
 * <p><b>create 흐름</b>
 * <ol>
 *   <li>원본 dto의 {@code ingredients}와 각 {@code step.ingredients}를 따로 잡아둔다</li>
 *   <li>운영용 cloned request 생성 — {@code recipe.ingredients=[]} <b>그리고 각 step의
 *       {@code ingredients=[]}</b>도 비움. step.ingredients가 비지 않으면 운영 RecipeStepService가
 *       INGREDIENT_NOT_FOUND를 던진다 (riMap 비어 있는 상태에서 step ingredient 매칭 시도). 원본 DTO는
 *       {@code dto.toBuilder()} clone으로 보호 — mutate 금지.</li>
 *   <li>운영 {@code createRecipeAndGenerateUrls(clonedReq, ...)} 호출 — recipe entity / step / tag /
 *       image presigned URL은 운영 코드 그대로 사용. ingredients와 step ingredients 모두 무동작.</li>
 *   <li>response.recipeId(raw Long)로 Recipe entity 재조회 (같은 트랜잭션 내 detached 회피)</li>
 *   <li>{@link DevRecipeIngredientPersistService#persistAll}로 dual-write 저장 + aggregate 갱신</li>
 *   <li>{@link DevRecipeIngredientPersistService#linkStepIngredients}로 step→recipe_ingredient
 *       FK 재연결 (3-tier name+unit+qty 매칭으로 C' bypass row까지 정확히 disambiguate)</li>
 * </ol>
 *
 * <p><b>update 흐름</b>: create와 동일 패턴. 단 운영 update는 {@code isIngredientsModified=true}일 때만
 * ingredient를 건드리므로, dev는 사용자가 modify 의사를 표시한 경우(true)에만 empty-then-save +
 * relink를 한다. isIngredientsModified=false면 dev도 ingredient를 건드리지 않고 운영에 그대로 위임.
 */
@Service
@RequiredArgsConstructor
public class DevRecipeWriteService {

    private final DevRecipeAccessValidator accessValidator;
    private final RecipeRepository recipeRepository;
    private final RecipeService recipeService;
    private final DevRecipeIngredientPersistService devIngredientPersist;
    private final Validator validator;

    @Transactional
    public PresignedUrlResponse createRecipe(Long userId, RecipeWithImageUploadRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        RecipeCreateRequestDto dto = request.getRecipe();
        if (dto == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 정보(recipe)가 비어있습니다.");
        }
        Set<ConstraintViolation<RecipeCreateRequestDto>> violations = validator.validate(dto);
        if (!violations.isEmpty()) {
            String firstMessage = violations.iterator().next().getMessage();
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, firstMessage);
        }

        Long originId = dto.getOriginRecipeId();
        if (originId != null) {
            accessValidator.loadAndCheckInteractable(originId, userId);
        }

        // 1) 원본 ingredients/steps 따로 잡아두기 (운영 호출이 원본 dto를 mutate하지 못하도록 별도 request 사용)
        List<RecipeIngredientRequestDto> originalIngredients = dto.getIngredients();
        List<RecipeStepRequestDto> originalSteps = dto.getSteps();
        Integer suppliedMarketPrice = dto.getMarketPrice();

        // 2) 운영용 cloned request — recipe.ingredients=[] + 각 step.ingredients=[] 모두 비움.
        // step.ingredients가 비지 않으면 운영 RecipeStepService가 INGREDIENT_NOT_FOUND를 던진다
        // (riMap이 비어있는 상태에서 step ingredient 매칭 시도). step→recipe_ingredient 링크는
        // dev persist 이후 linkStepIngredients가 다시 연결한다.
        RecipeCreateRequestDto operationalDto = dto.toBuilder()
                .ingredients(List.of())
                .steps(stripStepIngredients(originalSteps))
                .build();
        RecipeWithImageUploadRequest reqForOperational = RecipeWithImageUploadRequest.builder()
                .recipe(operationalDto)
                .files(request.getFiles())
                .aiRequest(request.getAiRequest())
                .build();

        // 3) 운영 호출 — recipe entity + step + tag + image presigned URL 처리 (step ingredient는 만들지 않음)
        PresignedUrlResponse response = recipeService.createRecipeAndGenerateUrls(
                reqForOperational, userId, RecipeSourceType.USER, null);

        // 4) 같은 트랜잭션 내에서 Recipe 재조회 후 dev persist + aggregate 갱신
        Recipe savedRecipe = recipeRepository.findById(response.getRecipeId())
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        devIngredientPersist.persistAll(savedRecipe, originalIngredients, suppliedMarketPrice);

        // 5) step→recipe_ingredient FK 재연결 (운영이 만들지 않은 RecipeStepIngredient 채움)
        devIngredientPersist.linkStepIngredients(savedRecipe, originalSteps);

        return response;
    }

    /**
     * 운영 호출용 step DTO 클론 — 각 step의 ingredients만 빈 리스트로. 다른 필드는 원본 그대로.
     * 원본 step DTO와 그 안의 ingredients 리스트는 mutate 하지 않는다 (dev persist에서 사용).
     */
    private static List<RecipeStepRequestDto> stripStepIngredients(List<RecipeStepRequestDto> originalSteps) {
        if (originalSteps == null) return null;
        return originalSteps.stream()
                .map(s -> s == null ? null : RecipeStepRequestDto.builder()
                        .stepNumber(s.getStepNumber())
                        .instruction(s.getInstruction())
                        .imageKey(s.getImageKey())
                        .action(s.getAction())
                        .timeline(s.getTimeline())
                        .ingredients(List.of())
                        .build())
                .toList();
    }

    @Transactional
    public PresignedUrlResponse updateRecipe(Long userId, Long recipeId, RecipeUpdateWithImageRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        if (request.getRecipe() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 정보(recipe)가 비어있습니다.");
        }

        // dev V3 strict: owner + ACTIVE only
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        if (recipe.getUser() == null || !recipe.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }
        if (recipe.getLifecycleStatus() != RecipeLifecycleStatus.ACTIVE) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }

        RecipeUpdateRequestDto dto = request.getRecipe();
        boolean modifyIngredients = Boolean.TRUE.equals(dto.getIsIngredientsModified());

        if (!modifyIngredients) {
            // 사용자가 ingredient 수정 의사 없음 → 운영에 그대로 위임 (운영도 ingredient 건드리지 않음)
            return recipeService.updateUserRecipe(recipeId, userId, request);
        }

        // empty-then-save: 원본 ingredients/steps는 dev persist에 사용, 운영에는 빈 리스트 전달.
        // isIngredientsModified=true 유지 → 운영이 기존 ingredient를 deleteByRecipeId로 정리(step_ingredient FK 포함).
        // operational의 afterCommit AI 분석 trigger도 그대로 작동.
        List<RecipeIngredientRequestDto> originalIngredients = dto.getIngredients();
        List<RecipeStepRequestDto> originalSteps = dto.getSteps();
        Integer suppliedMarketPrice = dto.getMarketPrice();

        // step.ingredients도 비워서 운영 step ingredient linker가 INGREDIENT_NOT_FOUND를 던지지 않게 한다.
        RecipeUpdateRequestDto operationalDto = dto.toBuilder()
                .ingredients(List.of())
                .steps(stripStepIngredientsForUpdate(originalSteps))
                .build();
        RecipeUpdateWithImageRequest reqForOperational = RecipeUpdateWithImageRequest.builder()
                .recipe(operationalDto)
                .files(request.getFiles())
                .build();

        PresignedUrlResponse response = recipeService.updateUserRecipe(recipeId, userId, reqForOperational);

        Recipe savedRecipe = recipeRepository.findById(response.getRecipeId())
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        // 운영이 이미 기존 ingredient를 비웠지만 step_ingredient FK 정리 + 멱등성 보장을 위해 replaceAll 사용.
        devIngredientPersist.replaceAll(savedRecipe, originalIngredients, suppliedMarketPrice);

        // step→recipe_ingredient FK 재연결
        devIngredientPersist.linkStepIngredients(savedRecipe, originalSteps);

        return response;
    }

    /** Update DTO용 step strip helper — create와 동일 의도. */
    private static List<RecipeStepRequestDto> stripStepIngredientsForUpdate(List<RecipeStepRequestDto> originalSteps) {
        return stripStepIngredients(originalSteps);
    }

    @Transactional
    public Long deleteRecipe(Long userId, Long recipeId) {
        return recipeService.deleteRecipe(recipeId, userId);
    }
}
