package com.jdc.recipe_service.dev.controller.calendar;

import com.jdc.recipe_service.dev.service.record.DevCookingRecordReadService;
import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
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

import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class DevCalendarControllerTest {

    @Mock DevCookingRecordReadService devCookingRecordReadService;
    @Mock CalendarMonthSummaryDto monthSummaryDto;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock CookingRecordSummaryDto summaryDto;

    @InjectMocks DevCalendarController controller;

    private static final Long USER_ID = 7L;
    private static final LocalDate DATE = LocalDate.of(2026, 4, 26);

    @Test
    @DisplayName("dayRecords anonymous: UNAUTHORIZED + service 미호출")
    void dayRecords_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.dayRecords(null, DATE))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("dayRecords authenticated + date null → INVALID_INPUT_VALUE, service 미호출")
    void dayRecords_nullDate_throwsInvalidInput() {
        // controller가 userDetails != null check 후 date null check이므로 userDetails 필요
        assertThatThrownBy(() -> controller.dayRecords(userDetails, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("dayRecords authenticated + date 있음: 200 + service 위임")
    void dayRecords_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCookingRecordReadService.getDayRecords(eq(USER_ID), eq(DATE), any()))
                .willReturn(List.of(summaryDto));

        ResponseEntity<List<CookingRecordSummaryDto>> response = controller.dayRecords(userDetails, DATE);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsExactly(summaryDto);
    }

    // ---------- monthSummary ----------

    @Test
    @DisplayName("monthSummary anonymous: UNAUTHORIZED + service 미호출")
    void monthSummary_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.monthSummary(null, 2026, 4))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("monthSummary authenticated: 200 + service 위임 (year/month 전달)")
    void monthSummary_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCookingRecordReadService.getMonthlySummary(eq(USER_ID), eq(2026), eq(4), any()))
                .willReturn(monthSummaryDto);

        ResponseEntity<CalendarMonthSummaryDto> response = controller.monthSummary(userDetails, 2026, 4);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(monthSummaryDto);
    }

    @Test
    @DisplayName("monthSummary authenticated + month=13 (boundary 위반) → INVALID_INPUT_VALUE, service 미호출")
    void monthSummary_invalidMonth_throwsInvalidInput() {
        assertThatThrownBy(() -> controller.monthSummary(userDetails, 2026, 13))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("monthSummary authenticated + month=0 → INVALID_INPUT_VALUE")
    void monthSummary_zeroMonth_throwsInvalidInput() {
        assertThatThrownBy(() -> controller.monthSummary(userDetails, 2026, 0))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("monthSummary authenticated + year=0 → INVALID_INPUT_VALUE")
    void monthSummary_zeroYear_throwsInvalidInput() {
        assertThatThrownBy(() -> controller.monthSummary(userDetails, 0, 4))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: monthSummary authenticated + year=10000 (upper bound 위반) → INVALID_INPUT_VALUE (LocalDate.of DateTimeException 500 회피)")
    void monthSummary_yearAboveUpperBound_throwsInvalidInput() {
        assertThatThrownBy(() -> controller.monthSummary(userDetails, 10000, 4))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("monthSummary authenticated + year=Integer.MAX_VALUE → INVALID_INPUT_VALUE (DateTimeException 500 회피)")
    void monthSummary_hugeYear_throwsInvalidInput() {
        assertThatThrownBy(() -> controller.monthSummary(userDetails, Integer.MAX_VALUE, 4))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("monthSummary authenticated + year=9999 (upper boundary) → 정상 위임 (boundary 안)")
    void monthSummary_year9999_passes() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCookingRecordReadService.getMonthlySummary(eq(USER_ID), eq(9999), eq(12), any()))
                .willReturn(monthSummaryDto);

        ResponseEntity<CalendarMonthSummaryDto> response = controller.monthSummary(userDetails, 9999, 12);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
    }
}
