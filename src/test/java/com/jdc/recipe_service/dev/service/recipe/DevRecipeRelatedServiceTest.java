package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeRemixQueryRepository;
import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeRecommendationService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;

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
 * DevRecipeRelatedService 분기 매트릭스.
 *
 *  - 두 endpoint 모두 base recipe validator 게이트
 *  - recommendations: post-filter로 RESTRICTED 후보 silent 제외 (운영 후보 쿼리는 isPrivate=false만 봄)
 *  - remixes: dev 전용 strict repo 사용 (imageReady 필터 추가)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeRelatedServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock DevRecipeAccessProjectionRepository accessProjectionRepository;
    @Mock RecipeRecommendationService recipeRecommendationService;
    @Mock DevRecipeRemixQueryRepository devRecipeRemixQueryRepository;
    @Mock RecipeSimpleDto remixDto;

    @InjectMocks DevRecipeRelatedService devRecipeRelatedService;

    private static final Long USER_ID = 7L;
    private static final Long OWNER_ID = 99L;
    private static final Long RECIPE_ID = 100L;
    private static final Pageable PAGE = PageRequest.of(0, 10);

    // ---------- recommendations ----------

    @Test
    @DisplayName("[recommendations] validator 통과 + 후보 모두 PUBLIC → 운영 결과 그대로 반환 (post-filter no-op)")
    void recommendations_allCandidatesPublic_returnsAll() {
        RecipeSimpleStaticDto dto1 = mockDto(1L);
        RecipeSimpleStaticDto dto2 = mockDto(2L);
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1, dto2));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicListed(1L), publicListed(2L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).extracting(RecipeSimpleStaticDto::getId).containsExactly(1L, 2L);
        InOrder order = inOrder(accessValidator, recipeRecommendationService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(recipeRecommendationService).getRecommendations(RECIPE_ID, 10);
    }

    @Test
    @DisplayName("[recommendations] **MUST 회귀 차단**: 운영이 RESTRICTED 후보 섞어 반환 → post-filter로 silent 제외, 결과 size 줄어듦")
    void recommendations_restrictedCandidateMixedIn_postFilterRemovesIt() {
        RecipeSimpleStaticDto dto1 = mockDto(1L);  // PUBLIC
        RecipeSimpleStaticDto dto2Restricted = mockDto(2L);  // RESTRICTED — 운영 후보 쿼리는 isPrivate=false만 보므로 통과
        RecipeSimpleStaticDto dto3 = mockDto(3L);  // PUBLIC
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1, dto2Restricted, dto3));
        // batch projection: 1=PUBLIC, 2=RESTRICTED, 3=PUBLIC
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(
                        publicListed(1L),
                        restricted(2L),
                        publicListed(3L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        // 2L (RESTRICTED) silent 제외 → 결과 size=2 (요청 size=10보다 작음)
        assertThat(result).extracting(RecipeSimpleStaticDto::getId).containsExactly(1L, 3L);
        assertThat(result).hasSize(2);
    }

    @Test
    @DisplayName("[recommendations] 모든 후보 차단 → 빈 리스트 반환")
    void recommendations_allCandidatesBlocked_returnsEmpty() {
        RecipeSimpleStaticDto dto1 = mockDto(1L);
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(restricted(1L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("[recommendations] **SHOULD 회귀 차단**: 운영이 PUBLIC+ACTIVE+LISTED지만 imageStatus=PENDING 후보 반환 → post-filter 제외")
    void recommendations_pendingImageCandidate_postFilterRemoves() {
        RecipeSimpleStaticDto dto1 = mockDto(1L);
        RecipeSimpleStaticDto dto2Pending = mockDto(2L);  // 가시성 PUBLIC OK, image PENDING
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1, dto2Pending));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicListed(1L), publicListedPending(2L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).extracting(RecipeSimpleStaticDto::getId).containsExactly(1L);
    }

    @Test
    @DisplayName("[recommendations] imageStatus=FAILED 후보도 제외")
    void recommendations_failedImageCandidate_postFilterRemoves() {
        RecipeSimpleStaticDto dto1Failed = mockDto(1L);
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1Failed));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(publicListedFailed(1L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("[recommendations] imageStatus=NULL legacy 후보 → 통과 (dev 컨벤션: READY OR NULL)")
    void recommendations_nullImageLegacyCandidate_passes() {
        RecipeSimpleStaticDto dto1Null = mockDto(1L);
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1Null));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(publicListedNullImage(1L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).extracting(RecipeSimpleStaticDto::getId).containsExactly(1L);
    }

    @Test
    @DisplayName("[recommendations] 운영이 빈 결과 반환 → projection 조회 자체 안 함")
    void recommendations_emptyOperationalResult_skipsProjection() {
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of());

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10);

        assertThat(result).isEmpty();
        verify(accessProjectionRepository, never()).findAccessProjectionsByIds(any());
    }

    @Test
    @DisplayName("[recommendations] base validator throw → 운영 service + projection 둘 다 미호출")
    void recommendations_validatorThrow_skipsAll() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(recipeRecommendationService, accessProjectionRepository);
    }

    @Test
    @DisplayName("[recommendations] anonymous + RESTRICTED 후보: anonymous는 owner 분기 적용 불가 → 차단")
    void recommendations_anonymousFiltersOutRestricted() {
        RecipeSimpleStaticDto dto1Restricted = mockDto(1L);
        given(recipeRecommendationService.getRecommendations(eq(RECIPE_ID), eq(10)))
                .willReturn(List.of(dto1Restricted));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(restricted(1L)));

        List<RecipeSimpleStaticDto> result = devRecipeRelatedService.getRecommendations(null, RECIPE_ID, 10);

        assertThat(result).isEmpty();
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, null);
    }

    // ---------- remixes ----------

    @Test
    @DisplayName("[remixes] validator 통과 → dev 전용 strict repo (imageReady 포함) 호출")
    void remixes_validatorPass_callsDevStrictRepo() {
        Page<RecipeSimpleDto> stub = new PageImpl<>(List.of(remixDto), PAGE, 1);
        given(devRecipeRemixQueryRepository.findStrictRemixesByOriginRecipeId(eq(RECIPE_ID), eq(PAGE)))
                .willReturn(stub);

        Page<RecipeSimpleDto> result = devRecipeRelatedService.findRemixes(USER_ID, RECIPE_ID, PAGE);

        assertThat(result).isSameAs(stub);
        InOrder order = inOrder(accessValidator, devRecipeRemixQueryRepository);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(devRecipeRemixQueryRepository).findStrictRemixesByOriginRecipeId(RECIPE_ID, PAGE);
    }

    @Test
    @DisplayName("[remixes] validator throw → dev repo 미호출")
    void remixes_validatorThrow_skipsRepo() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeRelatedService.findRemixes(USER_ID, RECIPE_ID, PAGE))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(devRecipeRemixQueryRepository);
    }

    @Test
    @DisplayName("[remixes] anonymous: validator에 null 전달")
    void remixes_anonymous_passesNullViewerId() {
        Page<RecipeSimpleDto> stub = new PageImpl<>(List.of(), PAGE, 0);
        given(devRecipeRemixQueryRepository.findStrictRemixesByOriginRecipeId(eq(RECIPE_ID), eq(PAGE)))
                .willReturn(stub);

        devRecipeRelatedService.findRemixes(null, RECIPE_ID, PAGE);

        InOrder order = inOrder(accessValidator, devRecipeRemixQueryRepository);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, null);
        order.verify(devRecipeRemixQueryRepository).findStrictRemixesByOriginRecipeId(RECIPE_ID, PAGE);
    }

    // ---------- helpers ----------

    private static RecipeSimpleStaticDto mockDto(Long id) {
        RecipeSimpleStaticDto dto = org.mockito.Mockito.mock(RecipeSimpleStaticDto.class);
        org.mockito.Mockito.when(dto.getId()).thenReturn(id);
        return dto;
    }

    private static DevRecipeAccessProjection publicListed(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, null);
    }

    private static DevRecipeAccessProjection restricted(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, null);
    }

    private static DevRecipeAccessProjection publicListedPending(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.PENDING, null);
    }

    private static DevRecipeAccessProjection publicListedFailed(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.FAILED, null);
    }

    private static DevRecipeAccessProjection publicListedNullImage(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                null, null);
    }
}
