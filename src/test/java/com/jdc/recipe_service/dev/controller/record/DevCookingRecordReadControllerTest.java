package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.dev.service.record.DevCookingRecordReadService;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordFeedResponse;
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
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class DevCookingRecordReadControllerTest {

    @Mock DevCookingRecordReadService devCookingRecordReadService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock CookingRecordFeedResponse feedResponse;
    @Mock CookingRecordDto detailDto;

    @InjectMocks DevCookingRecordReadController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECORD_ID = 500L;
    private static final Pageable PAGE = PageRequest.of(0, 20);

    @Test
    @DisplayName("timeline anonymous: UNAUTHORIZED + service 미호출")
    void timeline_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getRecordFeed(null, PAGE))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("timeline authenticated: 200 + service 위임")
    void timeline_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCookingRecordReadService.getRecordFeed(USER_ID, PAGE)).willReturn(feedResponse);

        ResponseEntity<CookingRecordFeedResponse> response = controller.getRecordFeed(userDetails, PAGE);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(feedResponse);
    }

    @Test
    @DisplayName("detail anonymous: UNAUTHORIZED + service 미호출")
    void detail_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getRecordDetail(null, RECORD_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devCookingRecordReadService);
    }

    @Test
    @DisplayName("detail authenticated: 200 + service 위임")
    void detail_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCookingRecordReadService.getRecordDetail(USER_ID, RECORD_ID)).willReturn(detailDto);

        ResponseEntity<CookingRecordDto> response = controller.getRecordDetail(userDetails, RECORD_ID);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(detailDto);
    }
}
