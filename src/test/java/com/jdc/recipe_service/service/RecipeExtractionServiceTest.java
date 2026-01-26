package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
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
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.context.request.async.DeferredResult;

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

    @Test
    @DisplayName("유튜브 추출 성공 테스트 (DeferredResult 반환)")
    void extractAndCreateRecipe_logsActivityWithNickname() {
        ExecutorService realExecutor = Executors.newSingleThreadExecutor();

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                realExecutor, asyncImageService, deferredResultHolder, recipeSearchService
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
        when(grokClientService.generateRecipeStep1(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));

        when(grokClientService.refineRecipeToStandard(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));

        when(transactionTemplate.execute(any())).thenAnswer(inv -> ((TransactionCallback<?>) inv.getArgument(0)).doInTransaction(null));
        when(recipeService.createRecipeAndGenerateUrls(any(), any(), eq(RecipeSourceType.YOUTUBE), isNull()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());

        DeferredResult<ResponseEntity<PresignedUrlResponse>> mockDeferredResult = new DeferredResult<>();
        when(deferredResultHolder.create(anyLong(), anyLong())).thenReturn(mockDeferredResult); // create 호출 시 모의 객체 반환

        DeferredResult<ResponseEntity<PresignedUrlResponse>> result = service.extractAndCreateRecipe(url, userId, nickname);

        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify(recipeActivityService, times(1)).saveLog(eq(userId), eq(nickname), eq(ActivityLogType.YOUTUBE_EXTRACT));
        verify(asyncImageService, times(1)).generateAndUploadAiImage(eq(1L), eq(false));
    }
}