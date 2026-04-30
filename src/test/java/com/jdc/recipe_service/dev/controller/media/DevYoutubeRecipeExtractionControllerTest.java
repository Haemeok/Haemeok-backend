package com.jdc.recipe_service.dev.controller.media;

import com.jdc.recipe_service.dev.facade.DevYoutubeRecipeExtractionFacade;
import com.jdc.recipe_service.dev.facade.DevYoutubeRecipeExtractionFacade.JobCreateResult;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.entity.User;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevYoutubeRecipeExtractionController 단위 테스트.
 *
 * 핵심 회귀 가드: facade가 created=false (idempotency reused) 반환 시
 * controller가 processYoutubeExtractionAsync()를 호출하지 않는지 보장.
 */
@ExtendWith(MockitoExtension.class)
class DevYoutubeRecipeExtractionControllerTest {

    @Mock DevYoutubeRecipeExtractionFacade facade;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevYoutubeRecipeExtractionController controller;

    private static final Long USER_ID = 1L;
    private static final String IDEMPOTENCY_KEY = "key-abc";
    private static final String URL = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
    private static final String IMAGE_MODEL = "gemini-2.5-flash-image";

    @BeforeEach
    void setUp() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
    }

    @Test
    @DisplayName("created=true (신규 job): processYoutubeExtractionAsync 호출됨")
    void createdTrue_invokesAsync() {
        given(facade.createYoutubeExtractionJob(any(), any(), any(), any()))
                .willReturn(new JobCreateResult(100L, true));

        ResponseEntity<JobIdResponse> response = controller.extractYoutubeRecipe(
                IDEMPOTENCY_KEY, URL, IMAGE_MODEL, userDetails);

        assertThat(response.getBody().jobId()).isEqualTo(100L);
        verify(facade).processYoutubeExtractionAsync(eq(100L), eq(URL), eq(IMAGE_MODEL), eq(USER_ID));
    }

    @Test
    @DisplayName("created=false (idempotency 재요청): processYoutubeExtractionAsync 호출 안 됨, 기존 jobId만 반환")
    void createdFalse_skipsAsync() {
        given(facade.createYoutubeExtractionJob(any(), any(), any(), any()))
                .willReturn(new JobCreateResult(50L, false));

        ResponseEntity<JobIdResponse> response = controller.extractYoutubeRecipe(
                "reused-key", URL, IMAGE_MODEL, userDetails);

        assertThat(response.getBody().jobId()).isEqualTo(50L);
        verify(facade, never()).processYoutubeExtractionAsync(any(), any(), any(), any());
    }

    @Test
    @DisplayName("Idempotency-Key 없으면 controller가 UUID 자동 생성해서 facade로 전달")
    void noIdempotencyHeader_generatesUuid() {
        given(facade.createYoutubeExtractionJob(any(), any(), any(), any()))
                .willReturn(new JobCreateResult(42L, true));

        controller.extractYoutubeRecipe(null, URL, IMAGE_MODEL, userDetails);

        // facade로 non-null/non-blank 키 전달됐는지 확인
        verify(facade).createYoutubeExtractionJob(eq(URL), eq(IMAGE_MODEL), eq(USER_ID),
                org.mockito.ArgumentMatchers.argThat(key -> key != null && !key.isBlank()));
    }
}
