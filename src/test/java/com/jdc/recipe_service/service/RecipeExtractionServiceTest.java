package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.media.YtDlpService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.ArrayList;
import java.util.concurrent.*;

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
    @Mock private YoutubeTargetChannelRepository youtubeTargetChannelRepository;
    @Mock private YoutubeRecommendationRepository youtubeRecommendationRepository;
    @Mock private TransactionTemplate transactionTemplate;
    @Mock private AsyncImageService asyncImageService;
    @Mock private DeferredResultHolder deferredResultHolder;
    @Mock private RecipeSearchService recipeSearchService;

    @Spy private ObjectMapper objectMapper = new ObjectMapper();

    @Test
    @DisplayName("유튜브 병렬 추출 성공 테스트 (extractAndCreateRecipeParallel)")
    void extractAndCreateRecipeParallel_success() {
        ExecutorService realExecutor = Executors.newSingleThreadExecutor();

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                realExecutor, asyncImageService, deferredResultHolder, recipeSearchService,
                objectMapper
        );

        String url = "https://www.youtube.com/watch?v=test1234";
        Long userId = 100L;
        String nickname = "요리왕비룡";

        when(ytDlpService.getVideoDataFull(anyString())).thenReturn(
                new YtDlpService.YoutubeFullDataDto(
                        "test1234", url, "맛있는 김치볶음밥", "설명", "댓글",
                        "[00:00] 자막", "자막", "채널", "id", "http://thumb", "http://prof", 100L, 100L
                )
        );

        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);
        mockDto.setTitle("김치볶음밥");
        mockDto.setIngredients(new ArrayList<>());

        when(grokClientService.generateRecipeStep1(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(mockDto));

        when(grokClientService.refineIngredientsOnly(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(new ArrayList<>()));

        when(asyncImageService.generateImageFromDto(any(), eq(userId)))
                .thenReturn(CompletableFuture.completedFuture("https://s3.bucket/image.jpg"));

        when(transactionTemplate.execute(any())).thenAnswer(inv -> ((TransactionCallback<?>) inv.getArgument(0)).doInTransaction(null));

        when(recipeService.createRecipeAndGenerateUrls(any(), any(), eq(RecipeSourceType.YOUTUBE), isNull()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());

        DeferredResult<ResponseEntity<PresignedUrlResponse>> result = service.extractAndCreateRecipe(url, userId, nickname);

        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify(recipeActivityService, times(1)).saveLog(eq(userId), eq(nickname), eq(ActivityLogType.YOUTUBE_EXTRACT));

        verify(asyncImageService, times(1)).generateImageFromDto(any(), eq(userId));

        verify(asyncImageService, never()).generateAndUploadAiImage(anyLong(), eq(false));
    }
}