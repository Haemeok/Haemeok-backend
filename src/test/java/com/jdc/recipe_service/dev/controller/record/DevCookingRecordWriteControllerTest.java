package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.dev.service.record.DevCookingRecordWriteService;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.hashids.Hashids;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevCookingRecordWriteController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계, service 위임, 응답 shape (recordId hashid + message). 게이트는 service test가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevCookingRecordWriteControllerTest {

    @Mock DevCookingRecordWriteService devCookingRecordWriteService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock CookingRecord cookingRecord;

    DevCookingRecordWriteController controller;
    Hashids hashids;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private static final Long RECORD_ID = 500L;

    @BeforeEach
    void setUp() {
        hashids = new Hashids("TEST_SALT_FOR_DEV_RECORD_CTRL", 8);
        controller = new DevCookingRecordWriteController(devCookingRecordWriteService, hashids);
    }

    // ---------- create ----------

    @Test
    @DisplayName("create anonymous: UNAUTHORIZED + service 미호출")
    void create_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.createRecord(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordWriteService);
    }

    @Test
    @DisplayName("create authenticated + recipeId=null (query param 누락) → INVALID_INPUT_VALUE pre-DB, service 미호출 (userDetails.getUser() 미접근)")
    void create_authenticatedNullRecipeId_throwsInvalidInputAndSkipsService() {
        // 의도: recipeId null guard가 userDetails.getUser() 호출 전에 throw — userDetails stubbing 불필요
        assertThatThrownBy(() -> controller.createRecord(null, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(devCookingRecordWriteService);
    }

    @Test
    @DisplayName("create authenticated: 200 + recordId hashid + message")
    void create_authenticated_returnsHashIdAndMessage() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(cookingRecord.getId()).willReturn(RECORD_ID);
        given(devCookingRecordWriteService.createCookingRecord(USER_ID, RECIPE_ID)).willReturn(cookingRecord);

        ResponseEntity<Map<String, Object>> response = controller.createRecord(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody())
                .containsEntry("recordId", hashids.encode(RECORD_ID))
                .containsEntry("message", "요리 기록이 추가되었습니다.");
    }

    @Test
    @DisplayName("create: service throw → propagate")
    void create_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devCookingRecordWriteService).createCookingRecord(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.createRecord(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    // ---------- delete ----------

    @Test
    @DisplayName("delete anonymous: UNAUTHORIZED + service 미호출")
    void delete_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.deleteRecord(RECORD_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordWriteService);
    }

    @Test
    @DisplayName("delete authenticated: 200 + 메시지 응답 + service 위임")
    void delete_authenticated_returns200WithMessage() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<Map<String, String>> response = controller.deleteRecord(RECORD_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("message", "요리 기록이 삭제되었습니다.");
        verify(devCookingRecordWriteService).deleteCookingRecord(USER_ID, RECORD_ID);
    }

    @Test
    @DisplayName("delete: service throw (USER_ACCESS_DENIED) → propagate")
    void delete_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.USER_ACCESS_DENIED))
                .given(devCookingRecordWriteService).deleteCookingRecord(USER_ID, RECORD_ID);

        assertThatThrownBy(() -> controller.deleteRecord(RECORD_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.USER_ACCESS_DENIED);
    }
}
