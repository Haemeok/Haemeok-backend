//package com.jdc.recipe_service.service;
//
//import com.fasterxml.jackson.databind.ObjectMapper;
//import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
//import com.jdc.recipe_service.exception.CustomException;
//import com.jdc.recipe_service.exception.ErrorCode;
//import com.jdc.recipe_service.service.ai.OpenAiClientService;
//import com.openai.client.OpenAIClient;
//import com.openai.models.chat.completions.ChatCompletion;
//import com.openai.models.chat.completions.ChatCompletionMessage;
//import com.openai.models.chat.completions.ChatCompletionCreateParams;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.mockito.Answers;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.MockitoAnnotations;
//
//import java.util.List;
//import java.util.Optional;
//import java.util.concurrent.CompletionException;
//
//import static org.assertj.core.api.Assertions.assertThat;
//import static org.assertj.core.api.Assertions.assertThatThrownBy;
//import static org.mockito.ArgumentMatchers.any;
//import static org.mockito.BDDMockito.*;
//
//class OpenAiClientServiceTest {
//
//    @Mock(answer = Answers.RETURNS_DEEP_STUBS)
//    private OpenAIClient openAIClient;
//
//    @Mock
//    private ObjectMapper objectMapper;
//
//    @InjectMocks
//    private OpenAiClientService openAiClientService;
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//    }
//
//    @Test
//    void generateRecipeJson_success() throws Exception {
//        // Given
//        String prompt = "테스트 프롬프트";
//
//        ChatCompletion completionMock = mock(ChatCompletion.class);
//        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
//        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);
//
//        given(openAIClient.chat()
//                .completions()
//                .create(any(ChatCompletionCreateParams.class)))
//                .willReturn(completionMock);
//
//        given(completionMock.choices()).willReturn(List.of(choiceMock));
//        given(choiceMock.message()).willReturn(messageMock);
//        given(messageMock.content()).willReturn(Optional.of("{\"title\":\"dummy\"}"));
//
//        RecipeCreateRequestDto dtoMock = new RecipeCreateRequestDto();
//        given(objectMapper.readValue("{\"title\":\"dummy\"}", RecipeCreateRequestDto.class))
//                .willReturn(dtoMock);
//
//        // When
//        RecipeCreateRequestDto result = openAiClientService
//                .generateRecipeJson(prompt)
//                .join();
//
//        // Then
//        assertThat(result).isSameAs(dtoMock);
//    }
//
//    @Test
//    void generateRecipeJson_emptyChoices_throwsCustomException() {
//        // Given
//        ChatCompletion completionMock = mock(ChatCompletion.class);
//
//        given(openAIClient.chat()
//                .completions()
//                .create(any(ChatCompletionCreateParams.class)))
//                .willReturn(completionMock);
//
//        given(completionMock.choices()).willReturn(List.of());
//
//        // When & Then
//        assertThatThrownBy(() -> openAiClientService.generateRecipeJson("p").join())
//                .isInstanceOf(CompletionException.class)
//                .satisfies(e -> {
//                    assertThat(e.getCause()).isInstanceOf(CustomException.class);
//                    assertThat(((CustomException) e.getCause()).getErrorCode())
//                            .isEqualTo(ErrorCode.AI_RECIPE_GENERATION_FAILED);
//                });
//    }
//
//    @Test
//    void generateRecipeJson_noContent_throwsCustomException() {
//        // Given
//        ChatCompletion completionMock = mock(ChatCompletion.class);
//        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
//        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);
//
//        given(openAIClient.chat()
//                .completions()
//                .create(any(ChatCompletionCreateParams.class)))
//                .willReturn(completionMock);
//
//        given(completionMock.choices()).willReturn(List.of(choiceMock));
//        given(choiceMock.message()).willReturn(messageMock);
//        given(messageMock.content()).willReturn(Optional.empty());
//
//        // When & Then
//        assertThatThrownBy(() -> openAiClientService.generateRecipeJson("p").join())
//                .isInstanceOf(CompletionException.class)
//                .satisfies(e -> {
//                    assertThat(e.getCause()).isInstanceOf(CustomException.class);
//                    assertThat(((CustomException) e.getCause()).getErrorCode())
//                            .isEqualTo(ErrorCode.AI_RECIPE_GENERATION_FAILED);
//                });
//    }
//
//    @Test
//    void generateRecipeJson_invalidJson_throwsCustomException() throws Exception {
//        // Given
//        ChatCompletion completionMock = mock(ChatCompletion.class);
//        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
//        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);
//
//        given(openAIClient.chat()
//                .completions()
//                .create(any(ChatCompletionCreateParams.class)))
//                .willReturn(completionMock);
//
//        given(completionMock.choices()).willReturn(List.of(choiceMock));
//        given(choiceMock.message()).willReturn(messageMock);
//        given(messageMock.content()).willReturn(Optional.of("잘못된 JSON"));
//        given(objectMapper.readValue("잘못된 JSON", RecipeCreateRequestDto.class))
//                .willThrow(new RuntimeException("파싱 에러"));
//
//        // When & Then
//        assertThatThrownBy(() -> openAiClientService.generateRecipeJson("p").join())
//                .isInstanceOf(CompletionException.class)
//                .satisfies(e -> {
//                    assertThat(e.getCause()).isInstanceOf(CustomException.class);
//                    assertThat(((CustomException) e.getCause()).getErrorCode())
//                            .isEqualTo(ErrorCode.INTERNAL_SERVER_ERROR);
//                });
//    }
//}
