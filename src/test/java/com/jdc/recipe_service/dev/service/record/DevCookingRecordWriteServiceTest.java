package com.jdc.recipe_service.dev.service.record;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CookingRecordService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevCookingRecordWriteService 분기 매트릭스.
 *
 *  - create: validator 게이트 → throw 시 운영 service 미호출 (RESTRICTED record 누수 차단)
 *  - delete: 게이트 없이 운영 service에 단순 위임 (cleanup right, 운영 ownership check)
 */
@ExtendWith(MockitoExtension.class)
class DevCookingRecordWriteServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock CookingRecordService cookingRecordService;
    @Mock CookingRecord cookingRecord;

    @InjectMocks DevCookingRecordWriteService devCookingRecordWriteService;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private static final Long RECORD_ID = 500L;

    // ---------- create ----------

    @Test
    @DisplayName("[create] validator 통과 → 운영 service 위임, 결과 반환")
    void create_validatorPass_delegates() {
        given(cookingRecordService.createCookingRecord(USER_ID, RECIPE_ID)).willReturn(cookingRecord);

        CookingRecord result = devCookingRecordWriteService.createCookingRecord(USER_ID, RECIPE_ID);

        assertThat(result).isSameAs(cookingRecord);
        InOrder order = inOrder(accessValidator, cookingRecordService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(cookingRecordService).createCookingRecord(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("[create] **🚨 운영 leak 차단**: validator throw (RESTRICTED non-owner) → 운영 service 미호출")
    void create_validatorThrowsAccessDenied_skipsOperationalService() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCookingRecordWriteService.createCookingRecord(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(cookingRecordService);
    }

    @Test
    @DisplayName("[create] validator throw (non-ACTIVE) → RECIPE_NOT_FOUND, 운영 service 미호출")
    void create_validatorThrowsNotFound_skipsOperationalService() {
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCookingRecordWriteService.createCookingRecord(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(cookingRecordService);
    }

    // ---------- delete ----------

    @Test
    @DisplayName("[delete] 게이트 없이 운영 service에 단순 위임 (cleanup right)")
    void delete_noGateDelegates() {
        devCookingRecordWriteService.deleteCookingRecord(USER_ID, RECORD_ID);

        verify(cookingRecordService).deleteCookingRecord(USER_ID, RECORD_ID);
        verifyNoInteractions(accessValidator);
    }

    @Test
    @DisplayName("[delete] 운영 service throw (USER_ACCESS_DENIED) → 그대로 propagate")
    void delete_propagatesAccessDenied() {
        willThrow(new CustomException(ErrorCode.USER_ACCESS_DENIED))
                .given(cookingRecordService).deleteCookingRecord(USER_ID, RECORD_ID);

        assertThatThrownBy(() -> devCookingRecordWriteService.deleteCookingRecord(USER_ID, RECORD_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.USER_ACCESS_DENIED);
    }

    @Test
    @DisplayName("[delete] 운영 service throw (COOKING_RECORD_NOT_FOUND) → 그대로 propagate")
    void delete_propagatesNotFound() {
        willThrow(new CustomException(ErrorCode.COOKING_RECORD_NOT_FOUND))
                .given(cookingRecordService).deleteCookingRecord(USER_ID, RECORD_ID);

        assertThatThrownBy(() -> devCookingRecordWriteService.deleteCookingRecord(USER_ID, RECORD_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.COOKING_RECORD_NOT_FOUND);
    }
}
