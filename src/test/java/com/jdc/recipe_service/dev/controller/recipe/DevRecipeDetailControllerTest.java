package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeDetailService;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.BDDMockito.given;

/**
 * DevRecipeDetailController 단위 테스트.
 *
 * 검증:
 *  1. 비로그인(userDetails=null) → service에 null userId 전달 + 정상 200 응답 (공개 레시피)
 *  2. 로그인 → service에 user.id 전달
 *  3. service가 RECIPE_PRIVATE_ACCESS_DENIED 던지면 그대로 propagate (글로벌 핸들러가 403 매핑)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeDetailControllerTest {

    @Mock DevRecipeDetailService devRecipeDetailService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevRecipeDetailController controller;

    private static final Long RECIPE_ID = 100L;
    private static final Long USER_ID = 1L;

    @BeforeEach
    void setUp() {
        // 로그인 case에서 사용 — 비로그인 case는 userDetails 자체를 null로 호출
    }

    @Test
    @DisplayName("비로그인 (anonymous): service에 currentUserId=null 전달 + 200 응답")
    void anonymous_passesNullUserId() {
        DevRecipeDetailDto dto = DevRecipeDetailDto.builder().build();
        given(devRecipeDetailService.getRecipeDetail(eq(RECIPE_ID), isNull()))
                .willReturn(dto);

        ResponseEntity<DevRecipeDetailDto> response = controller.getRecipeDetail(RECIPE_ID, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(dto);
    }

    @Test
    @DisplayName("로그인: service에 user.id 전달")
    void authenticated_passesUserId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        DevRecipeDetailDto dto = DevRecipeDetailDto.builder().build();
        given(devRecipeDetailService.getRecipeDetail(RECIPE_ID, USER_ID)).willReturn(dto);

        ResponseEntity<DevRecipeDetailDto> response = controller.getRecipeDetail(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(dto);
    }

    @Test
    @DisplayName("private 레시피 + 비로그인: service의 RECIPE_PRIVATE_ACCESS_DENIED가 그대로 propagate (글로벌 핸들러가 403 매핑)")
    void privateRecipe_anonymous_propagatesException() {
        given(devRecipeDetailService.getRecipeDetail(eq(RECIPE_ID), isNull()))
                .willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED));

        assertThatThrownBy(() -> controller.getRecipeDetail(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }
}
