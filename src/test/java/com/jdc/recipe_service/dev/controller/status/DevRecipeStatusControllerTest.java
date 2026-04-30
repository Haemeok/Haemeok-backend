package com.jdc.recipe_service.dev.controller.status;

import com.jdc.recipe_service.dev.service.status.DevRecipeStatusService;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeSaveStatusResponse;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeStatusRequest;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.hashids.Hashids;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeStatusController 단위 검증.
 *
 * 컨트롤러 책임: viewerId 추출(anonymous=null), service 위임, batch encoding 변환, 응답 status.
 * 게이트 분기는 DevRecipeStatusServiceTest가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeStatusControllerTest {

    @Mock DevRecipeStatusService devRecipeStatusService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock RecipeDetailStatusDto detailStatusDto;
    @Mock RecipeSimpleStatusDto simpleStatusDto;
    @Mock RecipeSaveStatusResponse saveStatusResponse;

    DevRecipeStatusController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private Hashids hashids;

    @BeforeEach
    void setUp() {
        hashids = new Hashids("TEST_SALT_FOR_DEV_STATUS_CTRL", 8);
        controller = new DevRecipeStatusController(devRecipeStatusService, hashids);
    }

    // ---------- GET /{id}/status ----------

    @Test
    @DisplayName("detail anonymous: viewerId=null로 service 호출")
    void detail_anonymous_passesNullViewerId() {
        given(devRecipeStatusService.getDetailStatus(null, RECIPE_ID)).willReturn(detailStatusDto);

        ResponseEntity<RecipeDetailStatusDto> response = controller.getDetailStatus(RECIPE_ID, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(detailStatusDto);
    }

    @Test
    @DisplayName("detail authenticated: viewerId=user.id로 service 호출")
    void detail_authenticated_passesUserId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeStatusService.getDetailStatus(USER_ID, RECIPE_ID)).willReturn(detailStatusDto);

        ResponseEntity<RecipeDetailStatusDto> response = controller.getDetailStatus(RECIPE_ID, userDetails);

        assertThat(response.getBody()).isSameAs(detailStatusDto);
    }

    @Test
    @DisplayName("detail: service throw → propagate")
    void detail_serviceThrows_propagates() {
        given(devRecipeStatusService.getDetailStatus(null, RECIPE_ID))
                .willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        assertThatThrownBy(() -> controller.getDetailStatus(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    // ---------- POST /status ----------

    @Test
    @DisplayName("batch: 결과 Map의 key를 HashID로 인코딩하여 반환")
    void batch_encodesKeysToHashId() {
        RecipeStatusRequest req = new RecipeStatusRequest(List.of(1L, 2L));
        Map<Long, RecipeSimpleStatusDto> rawResult = Map.of(1L, simpleStatusDto, 2L, simpleStatusDto);
        given(devRecipeStatusService.getBatchSimpleStatuses(null, List.of(1L, 2L))).willReturn(rawResult);

        ResponseEntity<Map<String, RecipeSimpleStatusDto>> response = controller.getBatchStatuses(req, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).hasSize(2);
        assertThat(response.getBody()).containsKey(hashids.encode(1L));
        assertThat(response.getBody()).containsKey(hashids.encode(2L));
    }

    @Test
    @DisplayName("batch authenticated: viewerId 전달 + 결과 인코딩")
    void batch_authenticated_passesUserIdAndEncodes() {
        RecipeStatusRequest req = new RecipeStatusRequest(List.of(1L));
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeStatusService.getBatchSimpleStatuses(USER_ID, List.of(1L)))
                .willReturn(Map.of(1L, simpleStatusDto));

        ResponseEntity<Map<String, RecipeSimpleStatusDto>> response = controller.getBatchStatuses(req, userDetails);

        assertThat(response.getBody()).containsOnlyKeys(hashids.encode(1L));
    }

    @Test
    @DisplayName("batch: silent filter로 누락된 ID는 응답에도 누락 (id=2 차단됨)")
    void batch_filteredIdsAreAbsentFromResponse() {
        RecipeStatusRequest req = new RecipeStatusRequest(List.of(1L, 2L, 3L));
        // service가 1, 3만 반환 (2 차단됨)
        given(devRecipeStatusService.getBatchSimpleStatuses(null, List.of(1L, 2L, 3L)))
                .willReturn(Map.of(1L, simpleStatusDto, 3L, simpleStatusDto));

        ResponseEntity<Map<String, RecipeSimpleStatusDto>> response = controller.getBatchStatuses(req, null);

        assertThat(response.getBody()).containsOnlyKeys(hashids.encode(1L), hashids.encode(3L));
        assertThat(response.getBody()).doesNotContainKey(hashids.encode(2L));
    }

    // ---------- GET /{id}/saved-books ----------

    @Test
    @DisplayName("savedBooks anonymous: UNAUTHORIZED + service 미호출")
    void savedBooks_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getSavedBooks(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeStatusService);
    }

    @Test
    @DisplayName("savedBooks authenticated: service 위임 + 200")
    void savedBooks_authenticated_delegates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeStatusService.getSaveStatus(USER_ID, RECIPE_ID)).willReturn(saveStatusResponse);

        ResponseEntity<RecipeSaveStatusResponse> response = controller.getSavedBooks(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(saveStatusResponse);
    }

    // helper to keep imports tidy
    @SuppressWarnings("unused")
    private void wireUserId() {
        ReflectionTestUtils.setField(user, "id", USER_ID);
    }
}
