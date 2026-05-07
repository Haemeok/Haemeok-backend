package com.jdc.recipe_service.dev.service.status;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeSaveStatusResponse;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import com.jdc.recipe_service.service.RecipeStatusService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeStatusService 분기 매트릭스.
 *
 * 핵심 invariants:
 *  - 단건 status: validator throw → 운영 service 미호출
 *  - 배치 status: 접근 가능한 ID만 결과 포함 (silent filter, per-id throw 없음)
 *  - 배치 status: 모든 ID 차단되면 빈 Map (운영 service 미호출)
 *  - 배치 status: 입력 빈 리스트면 빈 Map
 *  - saved-books: 게이트 없이 단순 위임 (validator 미호출)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeStatusServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock DevRecipeAccessProjectionRepository accessProjectionRepository;
    @Mock RecipeStatusService recipeStatusService;
    @Mock RecipeBookService recipeBookService;
    @Mock RecipeDetailStatusDto detailStatusDto;
    @Mock RecipeSimpleStatusDto simpleStatusDto;
    @Mock RecipeSaveStatusResponse saveStatusResponse;

    @InjectMocks DevRecipeStatusService devRecipeStatusService;

    private static final Long USER_ID = 7L;
    private static final Long OWNER_ID = 99L;
    private static final Long RECIPE_ID = 100L;

    // ---------- 단건 getDetailStatus ----------

    @Test
    @DisplayName("[detail] validator 통과 + service에 결과 있음 → dto 반환")
    void detail_validatorPass_returnsDto() {
        given(recipeStatusService.getStatuses(eq(List.of(RECIPE_ID)), eq(USER_ID)))
                .willReturn(Map.of(RECIPE_ID, detailStatusDto));

        RecipeDetailStatusDto result = devRecipeStatusService.getDetailStatus(USER_ID, RECIPE_ID);

        assertThat(result).isSameAs(detailStatusDto);
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    @Test
    @DisplayName("[detail] validator throw → 운영 service 미호출 + 에러 propagate")
    void detail_validatorThrow_skipsService() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRecipeStatusService.getDetailStatus(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(recipeStatusService);
    }

    @Test
    @DisplayName("[detail] validator 통과했지만 service가 빈 결과 → RECIPE_NOT_FOUND")
    void detail_validatorPassButNoResult_throwsNotFound() {
        given(recipeStatusService.getStatuses(eq(List.of(RECIPE_ID)), eq(USER_ID)))
                .willReturn(Collections.emptyMap());

        assertThatThrownBy(() -> devRecipeStatusService.getDetailStatus(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("[detail] anonymous (viewerId=null): validator에 null 전달")
    void detail_anonymous_passesNullViewerId() {
        given(recipeStatusService.getStatuses(eq(List.of(RECIPE_ID)), eq(null)))
                .willReturn(Map.of(RECIPE_ID, detailStatusDto));

        RecipeDetailStatusDto result = devRecipeStatusService.getDetailStatus(null, RECIPE_ID);

        assertThat(result).isSameAs(detailStatusDto);
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, null);
    }

    // ---------- 배치 getBatchSimpleStatuses ----------

    @Test
    @DisplayName("[batch] 빈 입력 → 빈 Map, repository/service 미호출")
    void batch_emptyInput_returnsEmpty() {
        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, List.of());

        assertThat(result).isEmpty();
        verifyNoInteractions(accessProjectionRepository, recipeStatusService);
    }

    @Test
    @DisplayName("[batch] null 입력 → 빈 Map")
    void batch_nullInput_returnsEmpty() {
        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, null);

        assertThat(result).isEmpty();
        verifyNoInteractions(accessProjectionRepository, recipeStatusService);
    }

    @Test
    @DisplayName("[batch] 모든 ID 접근 가능 → 모두 운영 service에 전달, 결과 그대로 반환")
    void batch_allAccessible_passesThrough() {
        List<Long> ids = List.of(1L, 2L, 3L);
        given(accessProjectionRepository.findAccessProjectionsByIds(ids))
                .willReturn(List.of(
                        publicListed(1L),
                        publicListed(2L),
                        publicListed(3L)));
        Map<Long, RecipeDetailStatusDto> details = Map.of(
                1L, detailStatusDto, 2L, detailStatusDto, 3L, detailStatusDto);
        given(recipeStatusService.getStatuses(eq(ids), eq(USER_ID))).willReturn(details);
        given(recipeStatusService.convertToSimpleStatus(details))
                .willReturn(Map.of(1L, simpleStatusDto, 2L, simpleStatusDto, 3L, simpleStatusDto));

        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, ids);

        assertThat(result).hasSize(3).containsKeys(1L, 2L, 3L);
    }

    @Test
    @DisplayName("[batch] 일부만 접근 가능 → 차단된 ID는 응답에서 silently 제외 (per-id throw 없음)")
    void batch_partialAccess_filtersOutBlockedIds() {
        // 1=PUBLIC OK, 2=PRIVATE 다른 사람 (차단), 3=PUBLIC OK
        List<Long> ids = List.of(1L, 2L, 3L);
        given(accessProjectionRepository.findAccessProjectionsByIds(ids))
                .willReturn(List.of(
                        publicListed(1L),
                        privateOther(2L),
                        publicListed(3L)));
        Map<Long, RecipeDetailStatusDto> details = Map.of(1L, detailStatusDto, 3L, detailStatusDto);
        given(recipeStatusService.getStatuses(eq(List.of(1L, 3L)), eq(USER_ID))).willReturn(details);
        given(recipeStatusService.convertToSimpleStatus(details))
                .willReturn(Map.of(1L, simpleStatusDto, 3L, simpleStatusDto));

        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, ids);

        assertThat(result).hasSize(2).containsKeys(1L, 3L).doesNotContainKey(2L);
        // 핵심 invariant: 차단된 ID는 throw가 아니라 silent exclude
    }

    @Test
    @DisplayName("[batch] 모든 ID 차단됨 → 빈 Map, 운영 service 미호출 (불필요한 조회 회피)")
    void batch_allBlocked_skipsOperationalService() {
        List<Long> ids = List.of(2L, 4L);
        given(accessProjectionRepository.findAccessProjectionsByIds(ids))
                .willReturn(List.of(privateOther(2L), nonActive(4L)));

        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, ids);

        assertThat(result).isEmpty();
        verify(recipeStatusService, times(0)).getStatuses(anyList(), any());
    }

    @Test
    @DisplayName("[batch] anonymous (viewerId=null): owner 분기 없이 PUBLIC+LISTED+ACTIVE만 통과")
    void batch_anonymous_passesOnlyPublic() {
        List<Long> ids = List.of(1L, 2L);
        given(accessProjectionRepository.findAccessProjectionsByIds(ids))
                .willReturn(List.of(
                        publicListed(1L),
                        privateOther(2L))); // anonymous에게는 owner 분기 적용 불가
        given(recipeStatusService.getStatuses(eq(List.of(1L)), eq(null)))
                .willReturn(Map.of(1L, detailStatusDto));
        given(recipeStatusService.convertToSimpleStatus(any()))
                .willReturn(Map.of(1L, simpleStatusDto));

        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(null, ids);

        assertThat(result).hasSize(1).containsKey(1L);
    }

    @Test
    @DisplayName("[batch] owner는 본인의 PRIVATE/RESTRICTED도 결과에 포함")
    void batch_ownerSeesOwnPrivate() {
        List<Long> ids = List.of(1L);
        given(accessProjectionRepository.findAccessProjectionsByIds(ids))
                .willReturn(List.of(privateOwnedBy(1L, USER_ID)));
        given(recipeStatusService.getStatuses(eq(List.of(1L)), eq(USER_ID)))
                .willReturn(Map.of(1L, detailStatusDto));
        given(recipeStatusService.convertToSimpleStatus(any()))
                .willReturn(Map.of(1L, simpleStatusDto));

        Map<Long, RecipeSimpleStatusDto> result =
                devRecipeStatusService.getBatchSimpleStatuses(USER_ID, ids);

        assertThat(result).hasSize(1).containsKey(1L);
    }

    // ---------- saved-books ----------

    @Test
    @DisplayName("[saved-books] 게이트 없이 운영 recipeBookService에 단순 위임")
    void savedBooks_noGateDelegates() {
        given(recipeBookService.getSaveStatus(USER_ID, RECIPE_ID)).willReturn(saveStatusResponse);

        RecipeSaveStatusResponse result = devRecipeStatusService.getSaveStatus(USER_ID, RECIPE_ID);

        assertThat(result).isSameAs(saveStatusResponse);
        verifyNoInteractions(accessValidator);
    }

    // ---------- helpers ----------

    private DevRecipeAccessProjection publicListed(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, null);
    }

    private DevRecipeAccessProjection privateOther(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, null);
    }

    private DevRecipeAccessProjection privateOwnedBy(Long id, Long ownerId) {
        return new DevRecipeAccessProjection(id, ownerId,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, null);
    }

    private DevRecipeAccessProjection nonActive(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, null);
    }
}
