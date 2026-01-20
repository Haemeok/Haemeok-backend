package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.media.YtDlpService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.concurrent.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeExtractionServiceTest {

    @Mock
    private YtDlpService ytDlpService;
    @Mock
    private GrokClientService grokClientService;
    @Mock
    private GeminiMultimodalService geminiMultimodalService;
    @Mock
    private RecipeService recipeService;
    @Mock
    private DailyQuotaService dailyQuotaService;
    @Mock
    private RecipeFavoriteService recipeFavoriteService;
    @Mock
    private RecipeActivityService recipeActivityService;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private YoutubeTargetChannelRepository youtubeTargetChannelRepository;
    @Mock
    private YoutubeRecommendationRepository youtubeRecommendationRepository;
    @Mock
    private TransactionTemplate transactionTemplate;

    @Test
    @DisplayName("유튜브 추출 성공 시 닉네임을 포함하여 활동 로그가 저장되어야 한다 (기존 테스트)")
    void extractAndCreateRecipe_logsActivityWithNickname() {
        ExecutorService realExecutor = Executors.newSingleThreadExecutor();

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService,
                grokClientService,
                geminiMultimodalService,
                recipeService,
                dailyQuotaService,
                recipeActivityService,
                recipeRepository,
                recipeFavoriteService,
                youtubeTargetChannelRepository,
                youtubeRecommendationRepository,
                transactionTemplate,
                realExecutor
        );

        String url = "https://www.youtube.com/watch?v=test1234";
        Long userId = 100L;
        String nickname = "요리왕비룡";

        String richDescription = "이 영상은 정말 맛있는 김치볶음밥 레시피입니다. 재료는 김치, 밥, 참기름이 필요합니다. 꼭 따라해보세요.";
        String richScript = "먼저 팬에 식용유를 두르고 김치를 볶아주세요. 간장 1큰술을 넣고 밥을 넣어 잘 섞어줍니다. 아주 맛있습니다.";

        when(ytDlpService.getVideoDataFull(anyString())).thenReturn(
                new YtDlpService.YoutubeFullDataDto(
                        "test1234", url, "맛있는 김치볶음밥", richDescription, "댓글",
                        "[00:00] " + richScript, richScript, "채널", "id", "http://thumb", "http://prof", 100L
                )
        );

        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);

        when(grokClientService.generateRecipeStep1(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));
        when(grokClientService.refineRecipeToStandard(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));

        when(transactionTemplate.execute(any())).thenAnswer(inv -> ((TransactionCallback<?>) inv.getArgument(0)).doInTransaction(null));

        when(recipeService.createRecipeAndGenerateUrls(any(), any(), eq(RecipeSourceType.YOUTUBE), isNull()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());

        CompletableFuture<PresignedUrlResponse> future = service.extractAndCreateRecipe(url, userId, nickname);
        future.join();

        verify(recipeActivityService, times(1)).saveLog(eq(userId), eq(nickname), eq(ActivityLogType.YOUTUBE_EXTRACT));
    }

    @Test
    @DisplayName("동시 요청 시: AI 호출은 1번만 실행되고, 두 유저 모두 결과와 즐겨찾기를 받아야 한다 (버스 태우기)")
    void concurrentRequests_shouldCoalesce() throws InterruptedException {
        ExecutorService realExecutor = Executors.newFixedThreadPool(5);

        RecipeExtractionService concurrentService = new RecipeExtractionService(
                ytDlpService,
                grokClientService,
                geminiMultimodalService,
                recipeService,
                dailyQuotaService,
                recipeActivityService,
                recipeRepository,
                recipeFavoriteService,
                youtubeTargetChannelRepository,
                youtubeRecommendationRepository,
                transactionTemplate,
                realExecutor
        );

        String url = "https://www.youtube.com/watch?v=TEST_VIDEO";
        Long userA = 100L;
        Long userB = 200L;
        String description = "이것은 맛있는 요리를 위한 재료 소개 영상입니다. 설명을 잘 읽어보시면 도움이 됩니다. 길이가 충분해야 합니다.";
        String script = "설탕 100g을 넣어주세요. 그리고 소금 1작은술도 필요합니다. 잘 섞어서 볶아주시면 완성됩니다.";

        CountDownLatch latch = new CountDownLatch(1);

        doAnswer(invocation -> {
            java.util.function.Consumer<org.springframework.transaction.TransactionStatus> callback =
                    invocation.getArgument(0);
            callback.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());

        when(ytDlpService.getVideoDataFull(anyString())).thenAnswer(invocation -> {
            System.out.println("🐌 [Mock] yt-dlp 작업 시작... 잠시 대기");
            latch.await(1, TimeUnit.SECONDS);
            System.out.println("⚡ [Mock] yt-dlp 작업 재개!");
            return new YtDlpService.YoutubeFullDataDto(
                    "TEST_VIDEO", url, "Test Title", description, "Cmt",
                    "[00:00] " + script, script, "Ch", "Id", "Thumb", "Prof", 100L
            );
        });

        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);
        when(grokClientService.generateRecipeStep1(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));
        when(grokClientService.refineRecipeToStandard(any(), any())).thenReturn(CompletableFuture.completedFuture(mockDto));

        when(transactionTemplate.execute(any())).thenAnswer(inv -> ((TransactionCallback<?>) inv.getArgument(0)).doInTransaction(null));

        when(recipeService.createRecipeAndGenerateUrls(any(), any(), any(), any()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(777L).build());

        System.out.println("🚀 User A 요청 시작");
        CompletableFuture<PresignedUrlResponse> futureA = concurrentService.extractAndCreateRecipe(url, userA, "UserA");

        Thread.sleep(100);

        System.out.println("🚀 User B 요청 시작 (User A 작업 중)");
        CompletableFuture<PresignedUrlResponse> futureB = concurrentService.extractAndCreateRecipe(url, userB, "UserB");

        latch.countDown();

        PresignedUrlResponse resultA = futureA.join();
        PresignedUrlResponse resultB = futureB.join();

        System.out.println("✅ 검증 시작");
        assertEquals(777L, resultA.getRecipeId());
        assertEquals(777L, resultB.getRecipeId());

        verify(ytDlpService, times(1)).getVideoDataFull(anyString());
        verify(recipeFavoriteService, times(1)).addFavoriteIfNotExists(eq(userA), eq(777L));
        verify(recipeFavoriteService, times(1)).addFavoriteIfNotExists(eq(userB), eq(777L));

        System.out.println("🎉 테스트 통과: AI 호출 1회, 즐겨찾기 2회 성공");
    }
}