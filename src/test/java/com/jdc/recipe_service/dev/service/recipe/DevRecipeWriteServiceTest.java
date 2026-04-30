package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeService;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeWriteService 분기 매트릭스.
 *
 * 핵심 invariants:
 *  - <b>create</b>: payload null guard + nested bean validation + remix origin gate
 *  - <b>update</b>: payload null guard + owner + ACTIVE only
 *  - <b>delete</b>: 게이트 없음, 운영 service에 단순 위임
 *
 * Validator는 real instance ({@link Validation#buildDefaultValidatorFactory()}) 사용 — 실제 constraint 위반을 잡는지
 * 모킹 없이 검증.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeWriteServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock RecipeRepository recipeRepository;
    @Mock RecipeService recipeService;
    @Mock DevRecipeIngredientPersistService devIngredientPersist;
    @Mock Recipe recipe;
    @Mock User owner;
    @Mock User otherUser;
    @Mock PresignedUrlResponse presignedResponse;

    DevRecipeWriteService devRecipeWriteService;

    private static final Long USER_ID = 7L;
    private static final Long OTHER_ID = 99L;
    private static final Long RECIPE_ID = 100L;
    private static final Long ORIGIN_ID = 200L;

    @BeforeEach
    void setUp() {
        // real Validator — 실제 @NotBlank 등 제약을 검증
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();
        devRecipeWriteService = new DevRecipeWriteService(
                accessValidator, recipeRepository, recipeService, devIngredientPersist, validator);
    }

    // ---------- create: payload null guard ----------

    @Test
    @DisplayName("[create] request=null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void create_nullRequest_throwsInvalidInputBeforeOperationalService() {
        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(recipeService, accessValidator);
    }

    @Test
    @DisplayName("[create] request.recipe=null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void create_nullRecipeDto_throwsInvalidInputBeforeOperationalService() {
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().recipe(null).build();

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(recipeService, accessValidator);
    }

    // ---------- create: nested bean validation (MUST 회귀 차단) ----------

    @Test
    @DisplayName("[create] **MUST 회귀 차단**: blank title → INVALID_INPUT_VALUE, 운영 service 미호출 (controller @Valid가 nested cascade 안 됨을 보완)")
    void create_blankTitle_failsNestedValidation() {
        RecipeWithImageUploadRequest req = createRequest(null, "", "찜요리");

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(recipeService, accessValidator);
    }

    @Test
    @DisplayName("[create] blank dishType → INVALID_INPUT_VALUE")
    void create_blankDishType_failsNestedValidation() {
        RecipeWithImageUploadRequest req = createRequest(null, "valid title", "");

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(recipeService, accessValidator);
    }

    @Test
    @DisplayName("[create] null title (whitespace 아님) → INVALID_INPUT_VALUE")
    void create_nullTitle_failsNestedValidation() {
        RecipeWithImageUploadRequest req = createRequest(null, null, "찜요리");

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
    }

    // ---------- create: validation pass + remix origin ----------

    @Test
    @DisplayName("[create] validation pass + origin 없음 → validator skip, 운영 service 위임 + dev persist 호출")
    void create_validPayloadNoOrigin_skipsRemixGate() {
        RecipeWithImageUploadRequest req = createRequest(null, "김치찌개", "찜요리");
        given(recipeService.createRecipeAndGenerateUrls(any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.USER), eq(null)))
                .willReturn(presignedResponse);
        given(presignedResponse.getRecipeId()).willReturn(RECIPE_ID);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        PresignedUrlResponse result = devRecipeWriteService.createRecipe(USER_ID, req);

        assertThat(result).isSameAs(presignedResponse);
        verifyNoInteractions(accessValidator);
        verify(devIngredientPersist).persistAll(eq(recipe), any(), any());
    }

    @Test
    @DisplayName("[create] validation pass + remix origin 가시성 통과 → 운영 service 위임 + dev persist 호출")
    void create_validPayloadWithAccessibleOrigin_delegates() {
        RecipeWithImageUploadRequest req = createRequest(ORIGIN_ID, "김치찌개", "찜요리");
        given(recipeService.createRecipeAndGenerateUrls(any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.USER), eq(null)))
                .willReturn(presignedResponse);
        given(presignedResponse.getRecipeId()).willReturn(RECIPE_ID);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        PresignedUrlResponse result = devRecipeWriteService.createRecipe(USER_ID, req);

        assertThat(result).isSameAs(presignedResponse);
        InOrder order = inOrder(accessValidator, recipeService, devIngredientPersist);
        order.verify(accessValidator).loadAndCheckInteractable(ORIGIN_ID, USER_ID);
        order.verify(recipeService).createRecipeAndGenerateUrls(any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.USER), eq(null));
        order.verify(devIngredientPersist).persistAll(eq(recipe), any(), any());
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: [create] 운영 호출에는 ingredients=[] 빈 리스트, dev persist에는 원본 리스트 전달 (DTO 안 망가짐)")
    void create_emptyThenSave_originalIngredientsPreserved() {
        RecipeIngredientRequestDto raw1 = RecipeIngredientRequestDto.builder().name("마늘").quantity("3").customUnit("쪽").build();
        RecipeIngredientRequestDto raw2 = RecipeIngredientRequestDto.builder().name("양파").quantity("1").customUnit("개").build();
        List<RecipeIngredientRequestDto> originalIngredients = new ArrayList<>(List.of(raw1, raw2));

        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setTitle("김치찌개");
        dto.setDishType("찜요리");
        dto.setIngredients(originalIngredients);
        dto.setMarketPrice(8500);

        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().recipe(dto).build();

        given(recipeService.createRecipeAndGenerateUrls(any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.USER), eq(null)))
                .willReturn(presignedResponse);
        given(presignedResponse.getRecipeId()).willReturn(RECIPE_ID);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        devRecipeWriteService.createRecipe(USER_ID, req);

        // 운영 호출: ingredients=[] 빈 리스트
        ArgumentCaptor<RecipeWithImageUploadRequest> opCaptor = ArgumentCaptor.forClass(RecipeWithImageUploadRequest.class);
        verify(recipeService).createRecipeAndGenerateUrls(opCaptor.capture(), eq(USER_ID), eq(RecipeSourceType.USER), eq(null));
        assertThat(opCaptor.getValue().getRecipe().getIngredients())
                .as("운영 호출에는 ingredients가 빈 리스트로 가야 dedupe/saveAll이 무동작")
                .isEmpty();
        assertThat(opCaptor.getValue().getRecipe().getTitle())
                .as("기타 필드는 원본 그대로 클론됨")
                .isEqualTo("김치찌개");

        // dev persist: 원본 ingredients 리스트 + marketPrice 그대로 전달
        ArgumentCaptor<List<RecipeIngredientRequestDto>> ingCaptor = ArgumentCaptor.forClass(List.class);
        ArgumentCaptor<Integer> mpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(devIngredientPersist).persistAll(eq(recipe), ingCaptor.capture(), mpCaptor.capture());
        assertThat(ingCaptor.getValue()).containsExactly(raw1, raw2);
        assertThat(mpCaptor.getValue()).isEqualTo(8500);

        // **회귀 차단**: 원본 dto의 ingredients는 mutate되지 않음
        assertThat(dto.getIngredients()).containsExactly(raw1, raw2);
    }

    @Test
    @DisplayName("[create] validation pass + remix origin RESTRICTED → validator throw, 운영 service 미호출")
    void create_validPayloadRemixRestrictedOrigin_throws() {
        RecipeWithImageUploadRequest req = createRequest(ORIGIN_ID, "김치찌개", "찜요리");
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(ORIGIN_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[create] validation pass + remix origin non-ACTIVE → RECIPE_NOT_FOUND, 운영 service 미호출")
    void create_validPayloadRemixNonActiveOrigin_throwsNotFound() {
        RecipeWithImageUploadRequest req = createRequest(ORIGIN_ID, "김치찌개", "찜요리");
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(ORIGIN_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeWriteService.createRecipe(USER_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeService);
    }

    // ---------- update: payload null guard ----------

    @Test
    @DisplayName("[update] **SHOULD 회귀 차단**: request=null → INVALID_INPUT_VALUE, recipeRepository 미호출 (불필요한 DB hit 회피)")
    void update_nullRequest_throwsInvalidInputBeforeDbHit() {
        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(recipeRepository, never()).findById(any());
        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[update] request.recipe=null → INVALID_INPUT_VALUE, recipeRepository 미호출")
    void update_nullRecipeDto_throwsInvalidInputBeforeDbHit() {
        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().recipe(null).build();

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(recipeRepository, never()).findById(any());
        verifyNoInteractions(recipeService);
    }

    // ---------- update: owner + ACTIVE 정책 ----------

    @Test
    @DisplayName("[update] owner + ACTIVE + isIngredientsModified=false → 운영 service에 원본 그대로 위임 (dev persist 미호출)")
    void update_ownerActive_ingredientsNotModified_delegatesAsIs() {
        RecipeUpdateWithImageRequest req = updateRequest();  // isIngredientsModified=false (default)
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(owner);
        given(owner.getId()).willReturn(USER_ID);
        given(recipe.getLifecycleStatus()).willReturn(RecipeLifecycleStatus.ACTIVE);
        given(recipeService.updateUserRecipe(RECIPE_ID, USER_ID, req)).willReturn(presignedResponse);

        PresignedUrlResponse result = devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req);

        assertThat(result).isSameAs(presignedResponse);
        verifyNoInteractions(devIngredientPersist);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: [update] isIngredientsModified=true → 운영 호출은 ingredients=[], dev persist replaceAll에는 원본 전달")
    void update_ingredientsModified_emptyThenReplaceAll() {
        RecipeIngredientRequestDto raw1 = RecipeIngredientRequestDto.builder().name("마늘").quantity("3").customUnit("쪽").build();
        RecipeIngredientRequestDto raw2 = RecipeIngredientRequestDto.builder().name("양파").quantity("1").customUnit("개").build();
        List<RecipeIngredientRequestDto> originalIngredients = new ArrayList<>(List.of(raw1, raw2));

        RecipeUpdateRequestDto dto = new RecipeUpdateRequestDto();
        dto.setTitle("김치찌개");
        dto.setIngredients(originalIngredients);
        dto.setIsIngredientsModified(true);
        dto.setMarketPrice(7700);

        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().recipe(dto).build();

        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(owner);
        given(owner.getId()).willReturn(USER_ID);
        given(recipe.getLifecycleStatus()).willReturn(RecipeLifecycleStatus.ACTIVE);
        given(recipeService.updateUserRecipe(eq(RECIPE_ID), eq(USER_ID), any(RecipeUpdateWithImageRequest.class)))
                .willReturn(presignedResponse);
        given(presignedResponse.getRecipeId()).willReturn(RECIPE_ID);

        devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req);

        // 운영 update 호출: ingredients=[] 빈 리스트
        ArgumentCaptor<RecipeUpdateWithImageRequest> opCaptor = ArgumentCaptor.forClass(RecipeUpdateWithImageRequest.class);
        verify(recipeService).updateUserRecipe(eq(RECIPE_ID), eq(USER_ID), opCaptor.capture());
        assertThat(opCaptor.getValue().getRecipe().getIngredients())
                .as("운영 update에는 ingredients가 빈 리스트로 가서 기존 ingredient들만 정리(step_ingredient FK 포함)")
                .isEmpty();
        assertThat(opCaptor.getValue().getRecipe().getIsIngredientsModified())
                .as("isIngredientsModified=true 유지 → 운영이 deleteByRecipeId 실행 + afterCommit AI 분석 trigger")
                .isTrue();

        // dev replaceAll: 원본 ingredients + marketPrice 전달
        ArgumentCaptor<List<RecipeIngredientRequestDto>> ingCaptor = ArgumentCaptor.forClass(List.class);
        ArgumentCaptor<Integer> mpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(devIngredientPersist).replaceAll(eq(recipe), ingCaptor.capture(), mpCaptor.capture());
        assertThat(ingCaptor.getValue()).containsExactly(raw1, raw2);
        assertThat(mpCaptor.getValue()).isEqualTo(7700);

        // **회귀 차단**: 원본 dto의 ingredients는 mutate되지 않음
        assertThat(dto.getIngredients()).containsExactly(raw1, raw2);
    }

    @Test
    @DisplayName("[update] non-owner → RECIPE_ACCESS_DENIED, 운영 service 미호출")
    void update_nonOwner_throwsAccessDenied() {
        RecipeUpdateWithImageRequest req = updateRequest();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(otherUser);
        given(otherUser.getId()).willReturn(OTHER_ID);

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[update] owner + lifecycle=HIDDEN → RECIPE_NOT_FOUND (admin 우회 차단)")
    void update_ownerNonActive_throwsNotFound() {
        RecipeUpdateWithImageRequest req = updateRequest();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(owner);
        given(owner.getId()).willReturn(USER_ID);
        given(recipe.getLifecycleStatus()).willReturn(RecipeLifecycleStatus.HIDDEN);

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[update] owner + lifecycle=BANNED → RECIPE_NOT_FOUND (admin 모더레이션 우회 차단)")
    void update_ownerBanned_throwsNotFound() {
        RecipeUpdateWithImageRequest req = updateRequest();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(owner);
        given(owner.getId()).willReturn(USER_ID);
        given(recipe.getLifecycleStatus()).willReturn(RecipeLifecycleStatus.BANNED);

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("[update] 레시피 없음 → RECIPE_NOT_FOUND")
    void update_recipeNotFound_throws() {
        RecipeUpdateWithImageRequest req = updateRequest();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[update] recipe.user가 null → RECIPE_ACCESS_DENIED (방어적 체크)")
    void update_recipeWithNullUser_throwsAccessDenied() {
        RecipeUpdateWithImageRequest req = updateRequest();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(null);

        assertThatThrownBy(() -> devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    // ---------- delete ----------

    @Test
    @DisplayName("[delete] 운영 service에 단순 위임 (lifecycle 무관)")
    void delete_delegatesUnconditionally() {
        given(recipeService.deleteRecipe(RECIPE_ID, USER_ID)).willReturn(RECIPE_ID);

        Long result = devRecipeWriteService.deleteRecipe(USER_ID, RECIPE_ID);

        assertThat(result).isEqualTo(RECIPE_ID);
        verifyNoInteractions(accessValidator);
        verify(recipeRepository, never()).findById(any());
    }

    @Test
    @DisplayName("[delete] 운영 service throw (RECIPE_ACCESS_DENIED) → 그대로 propagate")
    void delete_propagatesAccessDenied() {
        willThrow(new CustomException(ErrorCode.RECIPE_ACCESS_DENIED))
                .given(recipeService).deleteRecipe(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeWriteService.deleteRecipe(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    @Test
    @DisplayName("[delete] 운영 service throw (RECIPE_NOT_FOUND) → 그대로 propagate")
    void delete_propagatesNotFound() {
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(recipeService).deleteRecipe(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeWriteService.deleteRecipe(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    // ---------- helpers ----------

    private static RecipeWithImageUploadRequest createRequest(Long originId, String title, String dishType) {
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setOriginRecipeId(originId);
        dto.setTitle(title);
        dto.setDishType(dishType);
        return RecipeWithImageUploadRequest.builder().recipe(dto).build();
    }

    private static RecipeUpdateWithImageRequest updateRequest() {
        return RecipeUpdateWithImageRequest.builder()
                .recipe(new RecipeUpdateRequestDto())
                .build();
    }
}
