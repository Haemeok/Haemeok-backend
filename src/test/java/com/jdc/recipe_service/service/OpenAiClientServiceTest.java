package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.openai.client.OpenAIClient;
import com.openai.models.chat.completions.ChatCompletion;
import com.openai.models.chat.completions.ChatCompletionMessage;
import com.openai.models.chat.completions.ChatCompletionCreateParams;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Answers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

class OpenAiClientServiceTest {

    @Mock(answer = Answers.RETURNS_DEEP_STUBS)
    private OpenAIClient openAIClient;

    @Mock
    private ObjectMapper objectMapper;

    @InjectMocks
    private OpenAiClientService openAiClientService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void generateRecipeJson_success() throws Exception {
        String prompt = "테스트 프롬프트";

        ChatCompletion completionMock = mock(ChatCompletion.class);
        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);

        when(openAIClient.chat()
                .completions()
                .create(any(ChatCompletionCreateParams.class)))
                .thenReturn(completionMock);

        when(completionMock.choices()).thenReturn(List.of(choiceMock));
        when(choiceMock.message()).thenReturn(messageMock);
        when(messageMock.content()).thenReturn(Optional.of("{\"title\":\"dummy\"}"));

        RecipeCreateRequestDto dtoMock = new RecipeCreateRequestDto();
        when(objectMapper.readValue("{\"title\":\"dummy\"}", RecipeCreateRequestDto.class))
                .thenReturn(dtoMock);

        RecipeCreateRequestDto result = openAiClientService
                .generateRecipeJson(prompt)
                .join();

        assertSame(dtoMock, result);
    }

    @Test
    void generateRecipeJson_emptyChoices_throwsCustomException() {
        ChatCompletion completionMock = mock(ChatCompletion.class);

        when(openAIClient.chat()
                .completions()
                .create(any(ChatCompletionCreateParams.class)))
                .thenReturn(completionMock);

        when(completionMock.choices()).thenReturn(List.of());

        CompletionException ex = assertThrows(CompletionException.class, () ->
                openAiClientService.generateRecipeJson("p").join()
        );
        assertTrue(ex.getCause() instanceof CustomException);
        assertEquals(ErrorCode.AI_RECIPE_GENERATION_FAILED,
                ((CustomException) ex.getCause()).getErrorCode());
    }

    @Test
    void generateRecipeJson_noContent_throwsCustomException() {
        ChatCompletion completionMock = mock(ChatCompletion.class);
        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);

        when(openAIClient.chat()
                .completions()
                .create(any(ChatCompletionCreateParams.class)))
                .thenReturn(completionMock);

        when(completionMock.choices()).thenReturn(List.of(choiceMock));
        when(choiceMock.message()).thenReturn(messageMock);
        when(messageMock.content()).thenReturn(Optional.empty());

        CompletionException ex = assertThrows(CompletionException.class, () ->
                openAiClientService.generateRecipeJson("p").join()
        );
        assertTrue(ex.getCause() instanceof CustomException);
        assertEquals(ErrorCode.AI_RECIPE_GENERATION_FAILED,
                ((CustomException) ex.getCause()).getErrorCode());
    }

    @Test
    void generateRecipeJson_invalidJson_throwsCustomException() throws Exception {
        ChatCompletion completionMock = mock(ChatCompletion.class);
        ChatCompletion.Choice choiceMock = mock(ChatCompletion.Choice.class);
        ChatCompletionMessage messageMock = mock(ChatCompletionMessage.class);

        when(openAIClient.chat()
                .completions()
                .create(any(ChatCompletionCreateParams.class)))
                .thenReturn(completionMock);

        when(completionMock.choices()).thenReturn(List.of(choiceMock));
        when(choiceMock.message()).thenReturn(messageMock);
        when(messageMock.content()).thenReturn(Optional.of("잘못된 JSON"));
        when(objectMapper.readValue("잘못된 JSON", RecipeCreateRequestDto.class))
                .thenThrow(new RuntimeException("파싱 에러"));

        CompletionException ex = assertThrows(CompletionException.class, () ->
                openAiClientService.generateRecipeJson("p").join()
        );
        assertTrue(ex.getCause() instanceof CustomException);
        assertEquals(ErrorCode.INTERNAL_SERVER_ERROR,
                ((CustomException) ex.getCause()).getErrorCode());
    }
}
