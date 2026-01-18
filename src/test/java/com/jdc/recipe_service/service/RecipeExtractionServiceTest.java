package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.media.YtDlpService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeExtractionServiceTest {

    @Mock private YtDlpService ytDlpService;
    @Mock private GrokClientService grokClientService;
    @Mock private GeminiMultimodalService geminiMultimodalService;
    @Mock private RecipeService recipeService;
    @Mock private DailyQuotaService dailyQuotaService;
    @Mock private RecipeFavoriteService recipeFavoriteService;
    @Mock private RecipeActivityService recipeActivityService;
    @Mock private RecipeRepository recipeRepository;
    @Mock private TransactionTemplate transactionTemplate;

    @InjectMocks
    private RecipeExtractionService recipeExtractionService;

    @Test
    @DisplayName("유튜브 추출 성공 시 닉네임을 포함하여 활동 로그가 저장되어야 한다")
    void extractAndCreateRecipe_logsActivityWithNickname() throws ExecutionException, InterruptedException {
        // Given
        String url = "https://www.youtube.com/watch?v=test1234";
        Long userId = 100L;
        String nickname = "요리왕비룡";

        String richDescription = "이 영상은 맛있는 김치볶음밥 레시피입니다. 재료는 김치, 밥, 참기름이 필요합니다.";
        String richScript = "먼저 팬에 식용유를 두르고 김치를 볶아주세요. 간장 1큰술을 넣고 밥을 넣어 잘 섞어줍니다. 맛있게 드세요.";

        when(ytDlpService.getVideoDataFull(anyString())).thenReturn(
                new YtDlpService.YoutubeFullDataDto(
                        "test1234",
                        url,
                        "맛있는 김치볶음밥",
                        richDescription,
                        "댓글입니다.",
                        "[00:00] " + richScript,
                        richScript,
                        "백종원 PAIK JONG WON",
                        "http://thumb.url",
                        "http://profile.url",
                        100000L
                )
        );

        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);

        when(grokClientService.generateRecipeStep1(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));
        when(grokClientService.refineRecipeToStandard(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));

        when(transactionTemplate.execute(any())).thenAnswer(inv -> {
            TransactionCallback<?> callback = inv.getArgument(0);
            return callback.doInTransaction(null);
        });

        when(recipeService.createRecipeAndGenerateUrls(any(), any(), eq(RecipeSourceType.YOUTUBE), isNull()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());

        CompletableFuture<PresignedUrlResponse> future =
                recipeExtractionService.extractAndCreateRecipe(url, userId, nickname);

        future.join();

        verify(recipeActivityService, times(1)).saveLog(
                eq(userId),
                eq(nickname),
                eq(ActivityLogType.YOUTUBE_EXTRACT)
        );
    }
}