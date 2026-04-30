package com.jdc.recipe_service.dev.controller.ai;

import com.jdc.recipe_service.dev.facade.DevAiRecipeFacade;
import com.jdc.recipe_service.dev.facade.DevAiRecipeFacade.JobCreateResult;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.JobIdResponse;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
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
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevAiRecipeGenerationController 단위 테스트.
 *
 * 핵심 회귀 가드: facade가 created=false (idempotency reused) 반환 시
 * controller가 processAiGenerationAsync()를 호출하지 않는지 보장.
 * (이 회귀가 일어나면 같은 키 재요청마다 async가 다시 fire되어 중복 생성 발생.)
 */
@ExtendWith(MockitoExtension.class)
class DevAiRecipeGenerationControllerTest {

    @Mock DevAiRecipeFacade facade;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevAiRecipeGenerationController controller;

    private static final Long USER_ID = 1L;
    private static final String IDEMPOTENCY_KEY = "key-abc";
    private static final String IMAGE_MODEL = "gemini-2.5-flash-image";

    private RecipeWithImageUploadRequest validRequest;

    @BeforeEach
    void setUp() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        AiRecipeRequestDto aiReq = new AiRecipeRequestDto();
        validRequest = RecipeWithImageUploadRequest.builder()
                .aiRequest(aiReq)
                .build();
    }

    @Test
    @DisplayName("created=true (신규 job): processAiGenerationAsync가 호출됨")
    void createdTrue_invokesAsync() {
        given(facade.createAiGenerationJob(any(), any(), any(), any(), any()))
                .willReturn(new JobCreateResult(100L, true, false));

        ResponseEntity<JobIdResponse> response = controller.generateAiRecipe(
                IDEMPOTENCY_KEY, AiRecipeConcept.INGREDIENT_FOCUS, IMAGE_MODEL, validRequest, userDetails);

        assertThat(response.getBody().jobId()).isEqualTo(100L);
        verify(facade).processAiGenerationAsync(
                eq(100L), any(), eq(AiRecipeConcept.INGREDIENT_FOCUS),
                eq(IMAGE_MODEL), eq(USER_ID), eq(false));
    }

    @Test
    @DisplayName("created=true + usedToken=true: usedToken 값이 그대로 async에 전달됨")
    void createdTrueWithToken_passesUsedTokenToAsync() {
        given(facade.createAiGenerationJob(any(), any(), any(), any(), any()))
                .willReturn(new JobCreateResult(101L, true, true));

        controller.generateAiRecipe(
                IDEMPOTENCY_KEY, AiRecipeConcept.INGREDIENT_FOCUS, IMAGE_MODEL, validRequest, userDetails);

        verify(facade).processAiGenerationAsync(
                eq(101L), any(), any(), any(), eq(USER_ID), eq(true));
    }

    @Test
    @DisplayName("created=false (idempotency 재요청): processAiGenerationAsync 호출 안 됨, 기존 jobId만 반환")
    void createdFalse_skipsAsync() {
        given(facade.createAiGenerationJob(any(), any(), any(), any(), any()))
                .willReturn(new JobCreateResult(50L, false, false));

        ResponseEntity<JobIdResponse> response = controller.generateAiRecipe(
                "reused-key", AiRecipeConcept.INGREDIENT_FOCUS, IMAGE_MODEL, validRequest, userDetails);

        assertThat(response.getBody().jobId()).isEqualTo(50L);
        verify(facade, never()).processAiGenerationAsync(
                any(), any(), any(), any(), any(), anyBoolean());
    }
}
