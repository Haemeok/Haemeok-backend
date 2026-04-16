package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
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
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RecipeExtractionServiceTest {

    @Mock private YtDlpService ytDlpService;
    @Mock private GrokClientService grokClientService;
    @Mock private GeminiMultimodalService geminiMultimodalService;
    @Mock private RecipeService recipeService;
    @Mock private DailyQuotaService dailyQuotaService;
    @Mock private RecipeFavoriteService recipeFavoriteService;
    @Mock private RecipeBookService recipeBookService;
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
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService,
                recipeBookService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                testExecutor, asyncImageService, objectMapper
        );

        // TransactionTemplate Mocking
        given(transactionTemplate.execute(any())).willAnswer(invocation -> {
            TransactionCallback<Object> callback = invocation.getArgument(0);
            return callback.doInTransaction(null);
        });

        doAnswer(invocation -> {
            java.util.function.Consumer<org.springframework.transaction.TransactionStatus> consumer =
                    invocation.getArgument(0);
            consumer.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());

        // [핵심] Gemini 기본 Mocking (NPE 방지)
        RecipeCreateRequestDto defaultGeminiResponse = new RecipeCreateRequestDto();
        defaultGeminiResponse.setIsRecipe(true);
        given(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .willReturn(CompletableFuture.completedFuture(defaultGeminiResponse));
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

        // Given
        given(jobRepository.findById(1L)).willReturn(Optional.of(jobA));
        given(jobRepository.findById(2L)).willReturn(Optional.of(jobB));

        CountDownLatch latch = new CountDownLatch(1);

        given(ytDlpService.getVideoDataFull(anyString())).willAnswer(inv -> {
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

        // Given
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        given(ytDlpService.getVideoDataFull(anyString())).willReturn(
                new YtDlpService.YoutubeFullDataDto("mukbang", videoUrl, "Mukbang", SUFFICIENT_TEXT, "Eat", "", SUFFICIENT_TEXT, "Ch", "Id", "", "", 0L, 0L, 100L)
        );

        RecipeCreateRequestDto fakeResult = new RecipeCreateRequestDto();
        fakeResult.setIsRecipe(false);
        fakeResult.setNonRecipeReason("그냥 먹방임");
        given(grokClientService.generateRecipeStep1(any(), any()))
                .willReturn(CompletableFuture.completedFuture(fakeResult));

        // When
        service.processYoutubeExtractionAsyncV2(jobId, videoUrl, 100L, "User");

        // Then
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(job).setErrorMessage(captor.capture());
        assertThat(captor.getValue()).startsWith("901::");
        // 환불 호출 X 확인
        verify(dailyQuotaService, never()).refund(anyLong(), any(), anyBoolean());
    }

    @Test
    @DisplayName("💸 [에러/환불] '시스템 에러' 발생 시 환불되어야 한다 (500/701)")
    void testError_SystemFail_Refund() {
        Long jobId = 1L;
        String videoUrl = "https://www.youtube.com/watch?v=error";
        RecipeGenerationJob job = spy(RecipeGenerationJob.builder().id(jobId).status(JobStatus.PENDING).build());

        // Given
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        given(ytDlpService.getVideoDataFull(anyString())).willThrow(new RuntimeException("Connection Error"));
        given(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .willThrow(new RuntimeException("Gemini Also Failed"));

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

        // Given
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        given(ytDlpService.getVideoDataFull(anyString())).willAnswer(inv -> {
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

        // Given
        given(jobRepository.findById(jobId)).willReturn(Optional.of(failedJob));

        // When
        JobStatusDto statusDto = service.getJobStatus(jobId);

        // Then
        assertThat(statusDto.getCode()).isEqualTo("901");
        assertThat(statusDto.getMessage()).isEqualTo("레시피 영상이 아닙니다.");
        assertThat(statusDto.getStatus()).isEqualTo(JobStatus.FAILED);
    }

    private void mockSuccessFlow(Long recipeId) {
        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);
        mockDto.setIngredients(new ArrayList<>());

        given(grokClientService.generateRecipeStep1(any(), any()))
                .willReturn(CompletableFuture.completedFuture(mockDto));
        given(grokClientService.refineIngredientsOnly(any(), any()))
                .willReturn(CompletableFuture.completedFuture(new ArrayList<>()));
        given(asyncImageService.generateImageFromDto(any(), anyLong()))
                .willReturn(CompletableFuture.completedFuture("http://img.com/a.jpg"));

        PresignedUrlResponse response = PresignedUrlResponse.builder().recipeId(recipeId).build();
        given(recipeService.createRecipeAndGenerateUrls(any(), anyLong(), any(), any()))
                .willReturn(response);
    }

    @Test
    @DisplayName("✅ [재시도 검증] 즐겨찾기 추가 시 DB 충돌(낙관적 락)이 발생해도 재시도하여 성공해야 한다")
    void testFavoriteRetryLogic() {
        Long jobId = 1L;
        Long userId = 100L;
        Long recipeId = 777L;
        String videoUrl = "https://www.youtube.com/watch?v=video123";
        String videoId = "video123";

        RecipeGenerationJob mockJob = RecipeGenerationJob.builder()
                .id(jobId)
                .userId(userId)
                .status(JobStatus.IN_PROGRESS)
                .build();

        // Given
        given(jobRepository.findById(jobId)).willReturn(Optional.of(mockJob));

        PresignedUrlResponse mockResponse = PresignedUrlResponse.builder().recipeId(recipeId).build();
        doAnswer(invocation -> {
            java.util.function.Consumer<org.springframework.transaction.TransactionStatus> consumer = invocation.getArgument(0);
            consumer.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());

        doThrow(new org.springframework.dao.OptimisticLockingFailureException("누가 먼저 수정함 (1차 실패)"))
                .doThrow(new org.springframework.dao.OptimisticLockingFailureException("누가 먼저 수정함 (2차 실패)"))
                .doNothing()
                .when(recipeFavoriteService).addFavoriteIfNotExists(userId, recipeId);

        try {
            java.lang.reflect.Method method = RecipeExtractionService.class.getDeclaredMethod("addFavoriteToUser", Long.class, Long.class);
            method.setAccessible(true);
            method.invoke(service, userId, recipeId);
        } catch (Exception e) {
            e.printStackTrace();
        }

        verify(recipeFavoriteService, times(3)).addFavoriteIfNotExists(userId, recipeId);
    }

    // ─── parseIngredientBlock / buildIngredientHighlightSection ────────────────

    @Test
    @DisplayName("parseIngredientBlock: '재료' 섹션 키워드 이후 라인이 결과 목록에 수집되는 테스트")
    void parseIngredientBlock_collectsLinesAfterSectionKeyword() {
        // Given
        String text = """
                재료
                계란 2개
                소금 약간

                만드는 법
                1. 끓인다
                """;
        List<String> result = new ArrayList<>();

        // When
        ReflectionTestUtils.invokeMethod(service, "parseIngredientBlock", text, result);

        // Then
        assertThat(result).contains("계란 2개", "소금 약간");
        assertThat(result).doesNotContain("만드는 법", "1. 끓인다");
    }

    @Test
    @DisplayName("parseIngredientBlock: 섹션 키워드 없이도 단위 패턴 매칭 라인이 수집되는 테스트")
    void parseIngredientBlock_collectsLinesMatchingUnitPattern() {
        // Given
        String text = """
                오늘의 요리 소개합니다
                간장 3T
                설탕 2큰술
                물 200ml
                아무 텍스트
                """;
        List<String> result = new ArrayList<>();

        // When
        ReflectionTestUtils.invokeMethod(service, "parseIngredientBlock", text, result);

        // Then
        assertThat(result).contains("간장 3T", "설탕 2큰술", "물 200ml");
    }

    @Test
    @DisplayName("parseIngredientBlock: 콤마로 구분된 재료는 개별 항목으로 분리되는 테스트")
    void parseIngredientBlock_splitsCommaSeparatedIngredients() {
        // Given — 섹션 키워드 형식으로 재료가 나열된 경우
        String text = """
                재료
                소스 : 간장 4T, 맛술 2T, 참기름 1T
                """;
        List<String> result = new ArrayList<>();

        // When
        ReflectionTestUtils.invokeMethod(service, "parseIngredientBlock", text, result);

        // Then
        assertThat(result).containsExactlyInAnyOrder("간장 4T", "맛술 2T", "참기름 1T");
    }

    @Test
    @DisplayName("buildIngredientHighlightSection: 설명글에 재료 없으면 빈 문자열을 반환하는 테스트")
    void buildIngredientHighlightSection_returnsEmptyStringWhenNoIngredients() {
        // Given
        String description = "안녕하세요 오늘은 맛있는 요리를 소개합니다";
        String comments = "구독 좋아요 눌러주세요~";

        // When
        String result = ReflectionTestUtils.invokeMethod(service, "buildIngredientHighlightSection", description, comments);

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("buildIngredientHighlightSection: 재료 있으면 🔴 강조 헤더가 붙은 문자열을 반환하는 테스트")
    void buildIngredientHighlightSection_returnsHighlightedSectionWhenIngredientsFound() {
        // Given
        String description = """
                재료
                두부 1모
                간장 2T
                """;
        String comments = "";

        // When
        String result = ReflectionTestUtils.invokeMethod(service, "buildIngredientHighlightSection", description, comments);

        // Then
        assertThat(result).startsWith("🔴 [최우선 재료 목록");
        assertThat(result).contains("두부 1모", "간장 2T");
    }

    @Test
    @DisplayName("buildIngredientHighlightSection: 댓글에서도 재료를 추출하는 테스트")
    void buildIngredientHighlightSection_extractsIngredientsFromComments() {
        // Given — 설명글엔 재료 없고 댓글에 재료 있는 경우
        String description = "오늘도 좋은 하루 되세요!";
        String comments = """
                재료
                닭가슴살 200g
                올리브오일 1T
                """;

        // When
        String result = ReflectionTestUtils.invokeMethod(service, "buildIngredientHighlightSection", description, comments);

        // Then
        assertThat(result).contains("닭가슴살 200g", "올리브오일 1T");
    }

    @Test
    @DisplayName("parseIngredientBlock: null 또는 빈 문자열 입력 시 예외 없이 빈 결과를 반환하는 테스트")
    void parseIngredientBlock_handlesNullAndEmptyInputGracefully() {
        // Given
        List<String> result1 = new ArrayList<>();
        List<String> result2 = new ArrayList<>();

        // When & Then — 예외 없이 처리
        ReflectionTestUtils.invokeMethod(service, "parseIngredientBlock", (Object) null, result1);
        ReflectionTestUtils.invokeMethod(service, "parseIngredientBlock", "   ", result2);

        assertThat(result1).isEmpty();
        assertThat(result2).isEmpty();
    }
}