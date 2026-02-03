package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.media.YtDlpService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.Optional;
import java.util.concurrent.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
    @Mock private RecipeGenerationJobRepository jobRepository;

    @Spy private ObjectMapper objectMapper = new ObjectMapper();

    private RecipeExtractionService service;
    private ExecutorService testExecutor;

    private static final String SUFFICIENT_TEXT = "recipe ingredient salt sugar water boil fry cook step 1 2 3 minutes";

    @BeforeEach
    void setUp() {
        testExecutor = Executors.newCachedThreadPool();

        service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                testExecutor, asyncImageService, objectMapper
        );

        // TransactionTemplate Mocking
        lenient().when(transactionTemplate.execute(any())).thenAnswer(invocation -> {
            TransactionCallback<Object> callback = invocation.getArgument(0);
            return callback.doInTransaction(null);
        });

        lenient().doAnswer(invocation -> {
            java.util.function.Consumer<org.springframework.transaction.TransactionStatus> consumer =
                    invocation.getArgument(0);
            consumer.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());

        // [핵심] Gemini 기본 Mocking (NPE 방지) - 이게 없어서 터졌음
        RecipeCreateRequestDto defaultGeminiResponse = new RecipeCreateRequestDto();
        defaultGeminiResponse.setIsRecipe(true);
        lenient().when(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .thenReturn(CompletableFuture.completedFuture(defaultGeminiResponse));
    }

    @Test
    @DisplayName("🚌 [버스 로직] 동시 요청 시 AI는 1번만 실행되고, 결과는 공유되어야 한다")
    void testBusLogic_Deduplication() throws InterruptedException, ExecutionException {
        String videoUrl = "https://www.youtube.com/watch?v=busvideo1";
        Long userA = 100L;
        Long userB = 200L;
        Long expectedRecipeId = 777L;

        RecipeGenerationJob jobA = spy(RecipeGenerationJob.builder().id(1L).userId(userA).status(JobStatus.PENDING).build());
        RecipeGenerationJob jobB = spy(RecipeGenerationJob.builder().id(2L).userId(userB).status(JobStatus.PENDING).build());

        when(jobRepository.findById(1L)).thenReturn(Optional.of(jobA));
        when(jobRepository.findById(2L)).thenReturn(Optional.of(jobB));

        CountDownLatch latch = new CountDownLatch(1);

        // yt-dlp Delay Mock
        when(ytDlpService.getVideoDataFull(anyString())).thenAnswer(inv -> {
            System.out.println("🐌 [Test] AI 처리 중... (Delay)");
            latch.await(1, TimeUnit.SECONDS);
            return new YtDlpService.YoutubeFullDataDto("busvideo1", videoUrl, "Title", SUFFICIENT_TEXT, "Comments", "Sub", SUFFICIENT_TEXT, "Ch", "Id", "Thumb", "Prof", 100L, 100L, 60L);
        });

        mockSuccessFlow(expectedRecipeId);

        CompletableFuture<Void> futureA = CompletableFuture.runAsync(() ->
                service.processYoutubeExtractionAsyncV2(1L, videoUrl, userA, "UserA")
        );

        Thread.sleep(200);
        CompletableFuture<Void> futureB = CompletableFuture.runAsync(() ->
                service.processYoutubeExtractionAsyncV2(2L, videoUrl, userB, "UserB")
        );

        latch.countDown();
        CompletableFuture.allOf(futureA, futureB).join();

        verify(ytDlpService, times(1)).getVideoDataFull(anyString());
        verify(jobA).setResultRecipeId(expectedRecipeId);
        verify(jobB).setResultRecipeId(expectedRecipeId);

        verify(recipeFavoriteService, times(1)).addFavoriteIfNotExists(userA, expectedRecipeId);
        verify(recipeFavoriteService, times(1)).addFavoriteIfNotExists(userB, expectedRecipeId);
    }

    @Test
    @DisplayName("🚫 [에러/환불] '레시피 아님' 에러 시 환불되지 않아야 한다 (901)")
    void testError_NotRecipe_NoRefund() {
        Long jobId = 1L;
        String videoUrl = "https://www.youtube.com/watch?v=mukbang";
        RecipeGenerationJob job = spy(RecipeGenerationJob.builder().id(jobId).status(JobStatus.PENDING).build());

        when(jobRepository.findById(jobId)).thenReturn(Optional.of(job));

        when(ytDlpService.getVideoDataFull(anyString())).thenReturn(
                new YtDlpService.YoutubeFullDataDto("mukbang", videoUrl, "Mukbang", SUFFICIENT_TEXT, "Eat", "", SUFFICIENT_TEXT, "Ch", "Id", "", "", 0L, 0L, 100L)
        );

        // Grok 결과: 레시피 아님
        RecipeCreateRequestDto fakeResult = new RecipeCreateRequestDto();
        fakeResult.setIsRecipe(false);
        fakeResult.setNonRecipeReason("그냥 먹방임");
        when(grokClientService.generateRecipeStep1(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(fakeResult));

        service.processYoutubeExtractionAsyncV2(jobId, videoUrl, 100L, "User");

        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(job).setErrorMessage(captor.capture());

        // 901 에러 확인
        assertTrue(captor.getValue().startsWith("901::"));
        // 환불 호출 X 확인
        verify(dailyQuotaService, never()).refund(anyLong(), any(), anyBoolean());
    }

    @Test
    @DisplayName("💸 [에러/환불] '시스템 에러' 발생 시 환불되어야 한다 (500/701)")
    void testError_SystemFail_Refund() {
        Long jobId = 1L;
        String videoUrl = "https://www.youtube.com/watch?v=error";
        RecipeGenerationJob job = spy(RecipeGenerationJob.builder().id(jobId).status(JobStatus.PENDING).build());

        when(jobRepository.findById(jobId)).thenReturn(Optional.of(job));

        // 1. yt-dlp 실패 (의도적 예외)
        when(ytDlpService.getVideoDataFull(anyString())).thenThrow(new RuntimeException("Connection Error"));

        // 2. Fallback Gemini도 실패하도록 설정 (Mock 재설정)
        when(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .thenThrow(new RuntimeException("Gemini Also Failed"));

        service.processYoutubeExtractionAsyncV2(jobId, videoUrl, 100L, "User");

        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(job).setErrorMessage(captor.capture());

        // 에러 코드 500 or 701 확인
        System.out.println("Captured Error: " + captor.getValue());

        // 환불 호출 확인
        verify(dailyQuotaService, times(1)).refund(eq(100L), eq(QuotaType.YOUTUBE_EXTRACTION), eq(true));
    }

    @Test
    @DisplayName("📱 [쇼츠] Shorts URL 파싱 및 정상 동작 확인")
    void testShortsUrl_Parsing() {
        Long jobId = 1L;
        String shortsUrl = "https://www.youtube.com/shorts/shorts123?feature=share";
        RecipeGenerationJob job = spy(RecipeGenerationJob.builder().id(jobId).status(JobStatus.PENDING).build());

        when(jobRepository.findById(jobId)).thenReturn(Optional.of(job));

        when(ytDlpService.getVideoDataFull(anyString())).thenAnswer(inv -> {
            String url = inv.getArgument(0);
            if(url.contains("shorts123")) {
                return new YtDlpService.YoutubeFullDataDto("shorts123", url, "Shorts", SUFFICIENT_TEXT, "", "", SUFFICIENT_TEXT, "Ch", "Id", "", "", 0L, 0L, 50L);
            }
            throw new RuntimeException("Invalid URL passed to yt-dlp");
        });

        mockSuccessFlow(123L);

        service.processYoutubeExtractionAsyncV2(jobId, shortsUrl, 100L, "User");

        verify(job).setResultRecipeId(123L);
        verify(job).updateProgress(JobStatus.COMPLETED, 100);
    }

    @Test
    @DisplayName("🔍 [상태조회] 저장된 에러 메시지(Code::Msg)가 DTO로 잘 분리되는지 확인")
    void testGetJobStatus_ParsesErrorCode() {
        Long jobId = 99L;
        RecipeGenerationJob failedJob = RecipeGenerationJob.builder()
                .id(jobId)
                .status(JobStatus.FAILED)
                .errorMessage("901::레시피 영상이 아닙니다.")
                .progress(0)
                .build();

        when(jobRepository.findById(jobId)).thenReturn(Optional.of(failedJob));

        JobStatusDto statusDto = service.getJobStatus(jobId);

        assertEquals("901", statusDto.getCode());
        assertEquals("레시피 영상이 아닙니다.", statusDto.getMessage());
        assertEquals(JobStatus.FAILED, statusDto.getStatus());
    }

    // --- Helper Methods ---
    private void mockSuccessFlow(Long recipeId) {
        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);
        mockDto.setIngredients(new ArrayList<>());

        lenient().when(grokClientService.generateRecipeStep1(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(mockDto));

        lenient().when(grokClientService.refineIngredientsOnly(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(new ArrayList<>()));

        lenient().when(asyncImageService.generateImageFromDto(any(), anyLong()))
                .thenReturn(CompletableFuture.completedFuture("http://img.com/a.jpg"));

        PresignedUrlResponse response = PresignedUrlResponse.builder().recipeId(recipeId).build();
        lenient().when(recipeService.createRecipeAndGenerateUrls(any(), anyLong(), any(), any()))
                .thenReturn(response);
    }
}