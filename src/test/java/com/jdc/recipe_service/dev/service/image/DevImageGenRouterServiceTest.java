package com.jdc.recipe_service.dev.service.image;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.image.GeminiImageService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class DevImageGenRouterServiceTest {

    @Mock GeminiImageService geminiImageService;
    @Mock DevGptImageService devGptImageService;

    @InjectMocks
    DevImageGenRouterService router;

    private static final String PROMPT = "kimchi stew";
    private static final Long USER_ID = 42L;
    private static final Long RECIPE_ID = 7L;

    // --- routing happy paths ---

    @Test
    @DisplayName("gemini-2.5-flash-image → GeminiImageService에 위임 (quality 인자 없음)")
    void routesGeminiToGeminiService() {
        // given
        given(geminiImageService.generateImageUrls(PROMPT, USER_ID, RECIPE_ID))
                .willReturn(List.of("https://s3/gemini.webp"));

        // when
        List<String> result = router.generate("gemini-2.5-flash-image", PROMPT, USER_ID, RECIPE_ID);

        // then
        assertThat(result).containsExactly("https://s3/gemini.webp");
        verify(geminiImageService).generateImageUrls(PROMPT, USER_ID, RECIPE_ID);
        verifyNoInteractions(devGptImageService);
    }

    @Test
    @DisplayName("gpt-image-2-low → DevGptImageService에 apiModel='gpt-image-2', quality='low'로 위임")
    void routesGptLowToGptService() {
        // given
        given(devGptImageService.generateImageUrls(PROMPT, "gpt-image-2", "low", USER_ID, RECIPE_ID))
                .willReturn(List.of("https://s3/gpt-low.webp"));

        // when
        List<String> result = router.generate("gpt-image-2-low", PROMPT, USER_ID, RECIPE_ID);

        // then
        assertThat(result).containsExactly("https://s3/gpt-low.webp");
        verify(devGptImageService).generateImageUrls(PROMPT, "gpt-image-2", "low", USER_ID, RECIPE_ID);
        verify(geminiImageService, never()).generateImageUrls(any(), any(), any());
    }

    @Test
    @DisplayName("gpt-image-2-medium → apiModel='gpt-image-2', quality='medium'으로 위임")
    void routesGptMediumWithCorrectQuality() {
        // given
        given(devGptImageService.generateImageUrls(PROMPT, "gpt-image-2", "medium", USER_ID, RECIPE_ID))
                .willReturn(List.of("https://s3/gpt-med.webp"));

        // when
        router.generate("gpt-image-2-medium", PROMPT, USER_ID, RECIPE_ID);

        // then
        verify(devGptImageService).generateImageUrls(eq(PROMPT), eq("gpt-image-2"), eq("medium"), eq(USER_ID), eq(RECIPE_ID));
    }

    @Test
    @DisplayName("gpt-image-2-high → apiModel='gpt-image-2', quality='high'로 위임")
    void routesGptHighWithCorrectQuality() {
        // given
        given(devGptImageService.generateImageUrls(PROMPT, "gpt-image-2", "high", USER_ID, RECIPE_ID))
                .willReturn(List.of("https://s3/gpt-hi.webp"));

        // when
        router.generate("gpt-image-2-high", PROMPT, USER_ID, RECIPE_ID);

        // then
        verify(devGptImageService).generateImageUrls(eq(PROMPT), eq("gpt-image-2"), eq("high"), eq(USER_ID), eq(RECIPE_ID));
    }

    // --- whitelist guard (UNSUPPORTED_IMAGE_MODEL) ---

    @Test
    @DisplayName("화이트리스트에 없는 문자열 → UNSUPPORTED_IMAGE_MODEL 예외, 어떤 service도 호출 안 됨")
    void unknownModelRejected() {
        // when & then
        assertThatThrownBy(() ->
                router.generate("dall-e-3", PROMPT, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verifyNoInteractions(geminiImageService, devGptImageService);
    }

    @Test
    @DisplayName("gpt-image-1 (구 모델 식별자)도 화이트리스트에 없으면 거부 — apiModel과 identifier 분리 검증")
    void oldGptImage1IdentifierRejected() {
        // identifier는 외부 contract. apiModel(gpt-image-2)이 내부에서 쓰이는 것과 별개.
        // 사용자가 'gpt-image-1' 같은 미허용 identifier를 보내면 거부되어야 한다.
        assertThatThrownBy(() ->
                router.generate("gpt-image-1", PROMPT, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verifyNoInteractions(geminiImageService, devGptImageService);
    }

    @Test
    @DisplayName("null 모델명 → UNSUPPORTED_IMAGE_MODEL")
    void nullModelRejected() {
        assertThatThrownBy(() ->
                router.generate(null, PROMPT, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verifyNoInteractions(geminiImageService, devGptImageService);
    }

    @Test
    @DisplayName("blank 모델명 → UNSUPPORTED_IMAGE_MODEL")
    void blankModelRejected() {
        assertThatThrownBy(() ->
                router.generate("   ", PROMPT, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verifyNoInteractions(geminiImageService, devGptImageService);
    }

    @Test
    @DisplayName("대소문자 다르면 거부 (정확 매칭만 허용)")
    void caseSensitiveMatch() {
        assertThatThrownBy(() ->
                router.generate("GPT-IMAGE-2-LOW", PROMPT, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);
    }
}
