package com.jdc.recipe_service.dev.controller.report;

import com.jdc.recipe_service.dev.service.report.DevReportService;
import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevReportController 단위 검증.
 */
@ExtendWith(MockitoExtension.class)
class DevReportControllerTest {

    @Mock DevReportService devReportService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock IngredientReportRequest request;

    @InjectMocks DevReportController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    @Test
    @DisplayName("report anonymous: UNAUTHORIZED + service 미호출")
    void report_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.reportIngredient(RECIPE_ID, request, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devReportService);
    }

    @Test
    @DisplayName("report authenticated: 200 + 메시지 응답 + service 위임")
    void report_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<String> response = controller.reportIngredient(RECIPE_ID, request, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isEqualTo("소중한 제보 감사합니다.");
        verify(devReportService).createReportByName(USER_ID, RECIPE_ID, request);
    }

    @Test
    @DisplayName("report: service throw (RECIPE_PRIVATE_ACCESS_DENIED) → propagate")
    void report_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devReportService).createReportByName(USER_ID, RECIPE_ID, request);

        assertThatThrownBy(() -> controller.reportIngredient(RECIPE_ID, request, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }
}
