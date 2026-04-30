package com.jdc.recipe_service.dev.controller.ingredient;

import com.jdc.recipe_service.dev.service.ingredient.DevIngredientService;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
import static org.mockito.Mockito.verify;

/**
 * DevIngredientController 단위 테스트.
 *
 * 검증:
 *  1. decoded ingredient id가 service로 정확히 전달
 *  2. service 결과를 200 body로 그대로 반환
 *  3. service exception (INGREDIENT_NOT_FOUND 등)이 그대로 propagate (글로벌 핸들러가 매핑)
 */
@ExtendWith(MockitoExtension.class)
class DevIngredientControllerTest {

    @Mock DevIngredientService devIngredientService;

    @InjectMocks DevIngredientController controller;

    private static final Long INGREDIENT_ID = 42L;

    @Test
    @DisplayName("decoded id가 service로 전달 + 200 body 반환")
    void getIngredientDetail_passesIdAndReturns200() {
        IngredientDetailDto dto = IngredientDetailDto.builder()
                .id(INGREDIENT_ID)
                .name("대파")
                .build();
        given(devIngredientService.findDetailByIdDev(INGREDIENT_ID)).willReturn(dto);

        ResponseEntity<IngredientDetailDto> response = controller.getIngredientDetail(INGREDIENT_ID);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(dto);
        verify(devIngredientService).findDetailByIdDev(INGREDIENT_ID);
    }

    @Test
    @DisplayName("service가 INGREDIENT_NOT_FOUND 던지면 그대로 propagate (글로벌 핸들러가 404 매핑)")
    void getIngredientDetail_propagatesServiceException() {
        given(devIngredientService.findDetailByIdDev(INGREDIENT_ID))
                .willThrow(new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

        assertThatThrownBy(() -> controller.getIngredientDetail(INGREDIENT_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
    }
}
