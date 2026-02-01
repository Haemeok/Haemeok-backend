package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.QuotaType;
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
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
    @Mock private RecipeGenerationJobRepository jobRepository;
    @Mock private RecipeExtractionService service;

    @Spy private ObjectMapper objectMapper = new ObjectMapper();

    @Test
    @DisplayName("유튜브 병렬 추출 성공 테스트 (extractAndCreateRecipeParallel)")
    void extractAndCreateRecipeParallel_success() {
        ExecutorService realExecutor = Executors.newSingleThreadExecutor();

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                realExecutor, asyncImageService, objectMapper
        );

        String url = "https://www.youtube.com/watch?v=test1234";
        Long userId = 100L;
        String nickname = "요리왕비룡";
        String sufficientDesc = "이 영상은 맛있는 김치볶음밥을 만드는 레시피입니다. 정말 맛있어요. 재료는 김치, 밥, 참기름이 필요합니다. " +
                "김치를 볶다가 밥을 넣고 잘 섞어주세요. 참기름 1큰술을 넣으면 더 맛있습니다.";
        String sufficientScript = "자 오늘은 김치볶음밥을 만들어볼게요. 먼저 팬에 식용유를 두르고 김치를 볶습니다. " +
                "그 다음에 밥을 넣고 볶아주세요. 간장 1큰술 추가합니다.";

        when(ytDlpService.getVideoDataFull(anyString())).thenReturn(
                new YtDlpService.YoutubeFullDataDto(
                        "test1234", url, "맛있는 김치볶음밥",sufficientDesc , "댓글",
                        "[00:00] 자막", sufficientScript, "채널", "id", "http://thumb", "http://prof", 100L, 100L,600L
                )
        );

        RecipeCreateRequestDto mockDto = new RecipeCreateRequestDto();
        mockDto.setIsRecipe(true);
        mockDto.setTitle("김치볶음밥");
        mockDto.setDescription("맛있는 김치볶음밥입니다.");
        mockDto.setCookingTime(15);
        mockDto.setServings(1);
        mockDto.setDishType("볶음");
        List<RecipeIngredientRequestDto> mockIngredients = new ArrayList<>();
        RecipeIngredientRequestDto ing1 = new RecipeIngredientRequestDto();
        ing1.setName("김치"); ing1.setQuantity("1"); ing1.setCustomUnit("포기");
        mockIngredients.add(ing1);

        RecipeIngredientRequestDto ing2 = new RecipeIngredientRequestDto();
        ing2.setName("밥"); ing2.setQuantity("1"); ing2.setCustomUnit("공기");
        mockIngredients.add(ing2);
        mockDto.setIngredients(mockIngredients);

        when(grokClientService.generateRecipeStep1(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(mockDto));

        when(grokClientService.refineIngredientsOnly(any(), any()))
                .thenReturn(CompletableFuture.completedFuture(new ArrayList<>()));

        when(asyncImageService.generateImageFromDto(any(), anyLong()))
                .thenReturn(CompletableFuture.completedFuture("https://s3.bucket/image.jpg"));

        when(transactionTemplate.execute(any())).thenAnswer(invocation -> {
            TransactionCallback<PresignedUrlResponse> callback = invocation.getArgument(0);
            return callback.doInTransaction(null);
        });

        when(recipeService.createRecipeAndGenerateUrls(any(), anyLong(), any(), any()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());

        when(recipeService.createRecipeAndGenerateUrls(any(), anyLong(), any(), any()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(1L).build());
        DeferredResult<ResponseEntity<PresignedUrlResponse>> result = service.extractAndCreateRecipe(url, userId, nickname);

        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify(recipeActivityService, times(1)).saveLog(eq(userId), eq(nickname), eq(ActivityLogType.YOUTUBE_EXTRACT));

        verify(asyncImageService, times(1)).generateImageFromDto(any(), anyLong());

        verify(asyncImageService, never()).generateAndUploadAiImage(anyLong(), eq(false));
    }

    @Test
    @DisplayName("[V2] 유튜브 추출 작업 접수 및 비동기 실행 성공 테스트")
    void createYoutubeExtractionJobV2_Success() {
        // Given
        String url = "https://www.youtube.com/watch?v=v2test";
        Long userId = 100L;
        String nickname = "요리사";
        String idempotencyKey = "unique-key-123";
        Long jobId = 1L;

        // Mock: save 호출 시 전달받은 객체에 ID를 강제로 세팅해서 반환하도록 설정 (ID 0 문제 해결)
        when(jobRepository.findByIdempotencyKey(idempotencyKey)).thenReturn(Optional.empty());
        when(jobRepository.save(any(RecipeGenerationJob.class))).thenAnswer(invocation -> {
            RecipeGenerationJob job = invocation.getArgument(0);
            job.setId(jobId); // ID 수동 주입
            return job;
        });

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                Executors.newSingleThreadExecutor(), asyncImageService, objectMapper
        );

        // When: 1단계 접수
        Long returnedJobId = service.createYoutubeExtractionJobV2(url, userId, nickname, idempotencyKey);

        // Then: 접수 확인
        assertEquals(jobId, returnedJobId);
        verify(dailyQuotaService, times(1)).consumeForUserOrThrow(userId, QuotaType.YOUTUBE_EXTRACTION);
    }

    @Test
    @DisplayName("[V2] 동일한 멱등성 키로 요청 시 기존 JobID 반환 테스트")
    void createYoutubeExtractionJobV2_Idempotency() {
        // Given
        String url = "https://www.youtube.com/watch?v=v2test";
        String idempotencyKey = "same-key";
        Long existingJobId = 999L;

        // 명확하게 ID가 세팅된 Mock 객체 반환
        RecipeGenerationJob existingJob = RecipeGenerationJob.builder()
                .id(existingJobId)
                .idempotencyKey(idempotencyKey)
                .build();

        when(jobRepository.findByIdempotencyKey(idempotencyKey)).thenReturn(Optional.of(existingJob));

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                Executors.newSingleThreadExecutor(), asyncImageService, objectMapper
        );

        // When
        Long jobId = service.createYoutubeExtractionJobV2(url, 1L, "nick", idempotencyKey);

        // Then
        assertEquals(existingJobId, jobId);
        verify(dailyQuotaService, never()).consumeForUserOrThrow(any(), any());
    }

    @Test
    @DisplayName("[V2] 추출 실패 시 쿼터 환불 및 Job 상태 FAILED 변경 테스트")
    void processYoutubeExtractionAsyncV2_Failure_Refund() {
        // Given
        Long jobId = 1L;
        String url = "https://www.youtube.com/watch?v=fail";
        // 상태 변화를 추적하기 위해 필드값이 채워진 객체 생성
        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .id(jobId)
                .userId(100L)
                .status(JobStatus.PENDING)
                .build();

        when(jobRepository.findById(jobId)).thenReturn(Optional.of(job));
        // 의도적 예외 발생
        when(ytDlpService.getVideoDataFull(any())).thenThrow(new RuntimeException("yt-dlp error"));

        RecipeExtractionService service = new RecipeExtractionService(
                ytDlpService, grokClientService, geminiMultimodalService, recipeService,
                dailyQuotaService, recipeActivityService, recipeRepository, recipeFavoriteService, jobRepository,
                youtubeTargetChannelRepository, youtubeRecommendationRepository, transactionTemplate,
                Executors.newSingleThreadExecutor(), asyncImageService, objectMapper
        );

        // When
        service.processYoutubeExtractionAsyncV2(jobId, url, 100L, "nick");

        // Then
        assertEquals(JobStatus.FAILED, job.getStatus()); // 이제 정상적으로 FAILED 검증됨
        verify(dailyQuotaService, times(1)).refund(eq(100L), eq(QuotaType.YOUTUBE_EXTRACTION), eq(true));
    }
}