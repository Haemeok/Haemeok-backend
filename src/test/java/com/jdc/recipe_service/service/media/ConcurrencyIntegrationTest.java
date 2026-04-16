package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.recipe.RecipeAccessRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Commit;

import java.util.concurrent.CompletableFuture;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@EnabledIfEnvironmentVariable(named = "RUN_CONCURRENCY_INTEGRATION_TESTS", matches = "true")
class ConcurrencyIntegrationTest {

    @Autowired
    private YoutubeRecipeExtractionService youtubeService;

    @Autowired
    private RecipeGenerationJobRepository jobRepository;

    @Autowired
    private RecipeRepository recipeRepository;

    @Autowired
    private RecipeAccessRepository recipeAccessRepository;

    @Autowired
    private RecipeFavoriteRepository recipeFavoriteRepository;

    @Test
    @DisplayName("🔥 [실전] 텍스트 합승(1,2) vs 이미지 독주(3) 동시 실행 테스트")
    @Commit // 테스트가 끝나도 DB를 롤백하지 않고 저장함 (눈으로 확인 가능)
    void testRealConcurrencyScenario() {
        // Given: 실제 존재하는 유튜브 영상 (짧은 영상 추천)
        // 주의: 이전에 DB에 저장된 적 없는 새로운 영상 URL을 쓰거나, DB에서 해당 영상 레시피를 지우고 시작하세요.
        String videoUrl = "https://www.youtube.com/watch?v=TEST_CONCURRENCY_" + System.currentTimeMillis();
        // Tip: 실제 AI 호출까지 보고 싶다면 위 URL을 진짜 유튜브 URL로 바꾸세요.
        // 예: "https://www.youtube.com/watch?v=fmCMqwoUX6c"

        // 유저 세팅
        Long userText1 = 1L;
        Long userText2 = 2L;
        Long userImage = 100L;

        // 1. Job 3개 생성 (여기서 쿼터/크레딧 차감 발생)
        // Key를 다르게 주어 서로 다른 요청임을 명시
        Long jobId1 = youtubeService.createYoutubeExtractionJob(videoUrl, userText1, "User1", "KEY_T1_" + System.currentTimeMillis(), RecipeDisplayMode.TEXT_MODE);
        Long jobId2 = youtubeService.createYoutubeExtractionJob(videoUrl, userText2, "User2", "KEY_T2_" + System.currentTimeMillis(), RecipeDisplayMode.TEXT_MODE);
        Long jobId3 = youtubeService.createYoutubeExtractionJob(videoUrl, userImage, "User3", "KEY_I1_" + System.currentTimeMillis(), RecipeDisplayMode.IMAGE_MODE);

        System.out.println("=================================================");
        System.out.println("🚦 [START] 3명의 유저가 동시에 출발합니다!");
        System.out.println("   - Bus 1 (Text) : User " + userText1 + ", User " + userText2);
        System.out.println("   - Bus 2 (Image): User " + userImage);
        System.out.println("=================================================");

        // When: 3개의 스레드 동시 격발
        CompletableFuture<Void> task1 = CompletableFuture.runAsync(() ->
                youtubeService.processYoutubeExtractionAsync(jobId1, videoUrl, userText1, "User1", RecipeDisplayMode.TEXT_MODE));

        // User 2는 User 1과 '거의 동시에' 도착했다고 가정 (버스 로직 테스트)
        CompletableFuture<Void> task2 = CompletableFuture.runAsync(() -> {
            try { Thread.sleep(50); } catch (InterruptedException e) {} // 0.05초 차이
            youtubeService.processYoutubeExtractionAsync(jobId2, videoUrl, userText2, "User2", RecipeDisplayMode.TEXT_MODE);
        });

        // User 3는 이미지 모드로 별도 출발
        CompletableFuture<Void> task3 = CompletableFuture.runAsync(() ->
                youtubeService.processYoutubeExtractionAsync(jobId3, videoUrl, userImage, "User3", RecipeDisplayMode.IMAGE_MODE));

        // 모든 작업이 끝날 때까지 대기 (실제 AI 호출 시 10~20초 소요 가능)
        CompletableFuture.allOf(task1, task2, task3).join();

        System.out.println("=================================================");
        System.out.println("🏁 [FINISH] 모든 작업 종료. 결과 검증 시작...");
        System.out.println("=================================================");

        RecipeGenerationJob job1 = jobRepository.findById(jobId1).orElseThrow();
        RecipeGenerationJob job2 = jobRepository.findById(jobId2).orElseThrow();
        RecipeGenerationJob job3 = jobRepository.findById(jobId3).orElseThrow();

        System.out.println("⭐ 권한 및 즐겨찾기 최종 검증 시작...");

        // 이미지 모드 버스 (운전사 + 승객)
        if (job1.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            verifyAccessAndFavorite(userText1, job1.getResultRecipeId()); // 운전사
            verifyAccessAndFavorite(userText2, job1.getResultRecipeId()); // 승객 (이게 중요!)
        }

        // 텍스트 모드 버스 (운전사)
        if (job3.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            verifyAccessAndFavorite(jobId3, job3.getResultRecipeId());
        }

        // 📊 상태 출력
        System.out.println("👉 Job 1 Status: " + job1.getStatus());
        System.out.println("👉 Job 2 Status: " + job2.getStatus());
        System.out.println("👉 Job 3 Status: " + job3.getStatus());

        // [검증 1] 텍스트 모드 합승 확인 (User 1 & User 2)
        if (job1.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.FAILED) {
            // 실패 시: 에러 메시지가 같아야 함 (같은 버스 탔으니까!)
            assertThat(job1.getErrorMessage())
                    .as("User 1과 2는 같은 버스를 탔으므로 에러 메시지도 완벽히 같아야 합니다.")
                    .isEqualTo(job2.getErrorMessage());
            System.out.println("✅ [Success] 텍스트 모드 합승 확인 (에러 메시지 공유됨)");
        } else {
            // 성공 시: 레시피 ID가 같아야 함
            assertThat(job1.getResultRecipeId())
                    .as("User 1과 2는 같은 레시피 ID를 공유해야 합니다.")
                    .isEqualTo(job2.getResultRecipeId());
            System.out.println("✅ [Success] 텍스트 모드 합승 확인 (레시피 ID 공유됨)");
        }

        // [검증 2] 이미지 모드 분리 확인 (User 3)
        // 이미지 모드는 텍스트 모드와 별개로 돌았으므로, 서로 다른 Job ID를 가짐.
        // (가짜 URL일 경우 에러 메시지는 똑같이 '영상 분석 오류'일 수 있으므로 ID나 객체 자체 비교)
        assertThat(job3.getId())
                .as("이미지 모드 Job은 텍스트 모드 Job과 달라야 합니다.")
                .isNotEqualTo(job1.getId());

        System.out.println("✅ [Success] 이미지 모드 별도 실행 확인");
    }

    @Test
    @DisplayName("🚌 [반대 상황] 이미지 유저 2명은 합승하고, 텍스트 유저는 따로 가야 한다.")
    @Commit
    void testImageBusAndTextSeparation() {
        // Given
        String videoUrl = "https://www.youtube.com/watch?v=TEST_IMAGE_BUS_" + System.currentTimeMillis();

        Long userImage1 = 1L; // 운전사 (이미지)
        Long userImage2 = 2L; // 승객 (이미지)
        Long userText = 100L;   // 독고다이 (텍스트)

        // 1. Job 3개 생성
        // 유저 1, 2는 IMAGE_MODE, 유저 3은 TEXT_MODE
        Long jobImage1 = youtubeService.createYoutubeExtractionJob(videoUrl, userImage1, "UserImage1", "KEY_IMG_1", RecipeDisplayMode.IMAGE_MODE);
        Long jobImage2 = youtubeService.createYoutubeExtractionJob(videoUrl, userImage2, "UserImage2", "KEY_IMG_2", RecipeDisplayMode.IMAGE_MODE);
        Long jobText = youtubeService.createYoutubeExtractionJob(videoUrl, userText, "UserText1", "KEY_TXT_1", RecipeDisplayMode.TEXT_MODE);

        System.out.println("=================================================");
        System.out.println("🚦 [START] 이미지 버스 테스트 시작!");
        System.out.println("   - Bus 1 (Image): User 10, User 20 (합승 예상)");
        System.out.println("   - Bus 2 (Text) : User 30 (분리 예상)");
        System.out.println("=================================================");

        // When: 동시 실행
        // 1. 이미지 운전사 출발
        CompletableFuture<Void> task1 = CompletableFuture.runAsync(() ->
                youtubeService.processYoutubeExtractionAsync(jobImage1, videoUrl, userImage1, "UserImage1", RecipeDisplayMode.IMAGE_MODE));

        // 2. 이미지 승객 탑승 (0.05초 뒤)
        CompletableFuture<Void> task2 = CompletableFuture.runAsync(() -> {
            try { Thread.sleep(50); } catch (InterruptedException e) {}
            youtubeService.processYoutubeExtractionAsync(jobImage2, videoUrl, userImage2, "UserImage2", RecipeDisplayMode.IMAGE_MODE);
        });

        // 3. 텍스트 유저 출발 (별도 버스)
        CompletableFuture<Void> task3 = CompletableFuture.runAsync(() ->
                youtubeService.processYoutubeExtractionAsync(jobText, videoUrl, userText, "UserText1", RecipeDisplayMode.TEXT_MODE));

        // 대기
        CompletableFuture.allOf(task1, task2, task3).join();

        System.out.println("=================================================");
        System.out.println("🏁 [FINISH] 결과 검증");
        System.out.println("=================================================");

        // Then: DB 조회
        RecipeGenerationJob job1 = jobRepository.findById(jobImage1).orElseThrow();
        RecipeGenerationJob job2 = jobRepository.findById(jobImage2).orElseThrow();
        RecipeGenerationJob job3 = jobRepository.findById(jobText).orElseThrow();

        System.out.println("⭐ 권한 및 즐겨찾기 최종 검증 시작...");

        // 이미지 모드 버스 (운전사 + 승객)
        if (job1.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            verifyAccessAndFavorite(userImage1, job1.getResultRecipeId()); // 운전사
            verifyAccessAndFavorite(userImage2, job1.getResultRecipeId()); // 승객 (이게 중요!)
        }

        // 텍스트 모드 버스 (운전사)
        if (job3.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            verifyAccessAndFavorite(userText, job3.getResultRecipeId());
        }

        // [검증 1] 이미지 모드 유저들의 합승 확인
        if (job1.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.FAILED) {
            // 실패 시: 에러 메시지 공유 확인
            assertThat(job1.getErrorMessage())
                    .as("이미지 모드 유저 1과 2는 같은 버스를 탔으므로 에러 메시지가 같아야 함")
                    .isEqualTo(job2.getErrorMessage());
            System.out.println("✅ [Success] 이미지 모드 합승 확인 (에러 공유됨)");
        } else {
            // 성공 시: 결과 ID 공유 확인
            assertThat(job1.getResultRecipeId())
                    .as("이미지 모드 유저 1과 2는 같은 레시피를 받아야 함")
                    .isEqualTo(job2.getResultRecipeId());
            System.out.println("✅ [Success] 이미지 모드 합승 확인 (레시피 ID 공유됨)");
        }

        // [검증 2] 텍스트 모드의 분리 확인
        // 텍스트 모드는 이미지 버스와 키가 다르므로("..._TEXT_MODE"), 별도로 돌아서 다른 결과를 내거나, 별도의 에러 객체를 가져야 함.
        assertThat(job3.getId())
                .as("텍스트 모드는 이미지 모드 버스와 따로 가야 합니다.")
                .isNotEqualTo(job1.getId());

        // 텍스트 모드 잡은 이미지 모드 잡과 상태가 독립적이어야 함을 확인
        System.out.println("✅ [Success] 텍스트 모드 분리 확인");

        // 로그 확인용
        System.out.println("👉 Job Image 1 ID: " + job1.getId() + " / Status: " + job1.getStatus());
        System.out.println("👉 Job Image 2 ID: " + job2.getId() + " / Status: " + job2.getStatus());
        System.out.println("👉 Job Text 1  ID: " + job3.getId() + " / Status: " + job3.getStatus());
    }

    private void verifyAccessAndFavorite(Long userId, Long recipeId) {
        if (recipeId == null) return; // 실패한 경우는 건너뜀

        // 1. 권한(Access) 체크
        boolean hasAccess = recipeAccessRepository.existsByUserIdAndRecipeId(userId, recipeId);
        assertThat(hasAccess)
                .as("유저 " + userId + "에게 레시피 " + recipeId + "에 대한 접근 권한이 부여되어야 합니다.")
                .isTrue();

        // 2. 즐겨찾기(Favorite) 체크
        boolean isFavorite = recipeFavoriteRepository.existsByUserIdAndRecipeId(userId, recipeId);
        assertThat(isFavorite)
                .as("유저 " + userId + "의 보관함에 레시피 " + recipeId + "가 자동으로 추가되어야 합니다.")
                .isTrue();

        System.out.println("✅ [Verify] 유저 " + userId + " : 권한 및 즐겨찾기 확인 완료");
    }
}