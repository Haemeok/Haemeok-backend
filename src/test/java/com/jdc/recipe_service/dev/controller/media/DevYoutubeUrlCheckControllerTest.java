package com.jdc.recipe_service.dev.controller.media;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.media.YoutubeUrlCheckService;
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

/**
 * DevYoutubeUrlCheckController 단위 검증.
 *
 * 운영보다 엄격한 {@link YoutubeUrlCheckService}를 사용한다는 wiring 자체와 응답 shape (RecipeIdResponse 또는 null)
 * 만 잠근다. 실제 4-enum + official user 필터는 service 레이어가 책임.
 */
@ExtendWith(MockitoExtension.class)
class DevYoutubeUrlCheckControllerTest {

    @Mock YoutubeUrlCheckService youtubeUrlCheckService;

    @InjectMocks DevYoutubeUrlCheckController controller;

    private static final String VALID_URL = "https://www.youtube.com/watch?v=abc123";

    @Test
    @DisplayName("URL 매칭됨: RecipeIdResponse(recipeId) 반환")
    void check_matchedReturnsRecipeId() {
        given(youtubeUrlCheckService.checkUrlExistence(VALID_URL)).willReturn(42L);

        ResponseEntity<DevYoutubeUrlCheckController.RecipeIdResponse> response = controller.check(VALID_URL);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().recipeId()).isEqualTo(42L);
    }

    @Test
    @DisplayName("URL이 strict 필터(visibility/listing/lifecycle/imageReady) 통과 못함: null 반환 (존재 누설 방지)")
    void check_filteredOutReturnsNull() {
        given(youtubeUrlCheckService.checkUrlExistence(VALID_URL)).willReturn(null);

        ResponseEntity<DevYoutubeUrlCheckController.RecipeIdResponse> response = controller.check(VALID_URL);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isNull();
    }

    @Test
    @DisplayName("INVALID_URL_FORMAT: service가 throw하면 그대로 propagate")
    void check_invalidUrlPropagates() {
        willThrow(new CustomException(ErrorCode.INVALID_URL_FORMAT))
                .given(youtubeUrlCheckService).checkUrlExistence("not-a-url");

        assertThatThrownBy(() -> controller.check("not-a-url"))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);
    }
}
