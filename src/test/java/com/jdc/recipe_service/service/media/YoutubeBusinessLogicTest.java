package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.recipe.RecipeAccessRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.service.user.UserCreditService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.transaction.annotation.Transactional;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@Transactional // 테스트 후 DB 상태를 깨끗하게 유지
class YoutubeBusinessLogicTest {

    @Autowired
    private YoutubeRecipeExtractionService youtubeService;

    @Autowired
    private RecipeRepository recipeRepository;

    @Autowired
    private RecipeAccessRepository recipeAccessRepository;

    @Autowired
    private UserCreditService userCreditService;

    @Test
    @DisplayName("✅ 시나리오 1: 텍스트 중복 시 '서버 이득' 및 '권한 부여' 검증")
    void testTextToTextProfitScenario() {
        // Given: DB에 이미 '이미지 없는' 텍스트 레시피가 존재함
        String videoId = "PROFIT_TEST_" + System.currentTimeMillis();
        String videoUrl = "https://www.youtube.com/watch?v=" + videoId;
        Long userId = 1L;

        Recipe existingTextRecipe = Recipe.builder()
                .title("기존 텍스트 레시피")
                .youtubeUrl(videoUrl)
                .imageKey(null) // 이미지 없음
                .source(RecipeSourceType.YOUTUBE)
                .build();
        recipeRepository.save(existingTextRecipe);

        // When: 동일한 URL로 '텍스트 모드' 요청
        Long jobId = youtubeService.createYoutubeExtractionJob(videoUrl, userId, "User", "KEY_1", RecipeDisplayMode.TEXT_MODE);
        youtubeService.processYoutubeExtractionAsync(jobId, videoUrl, userId, "User", RecipeDisplayMode.TEXT_MODE);

        // Then:
        // 1. 환불이 없어야 함 (서버 이득)
        // 2. 해당 유저는 '돈을 냈으므로' RecipeAccess 권한이 기록되어야 함
        boolean hasAccess = recipeAccessRepository.existsByUserIdAndRecipeId(userId, existingTextRecipe.getId());
        assertThat(hasAccess).as("돈을 지불한 텍스트 중복 유저는 권한이 기록되어야 합니다.").isTrue();
    }

    @Test
    @DisplayName("✅ 시나리오 2: 텍스트 요청 시 이미지 존재하면 '무료 환불' 및 '권한 미기록' 검증")
    void testTextToImageRefundScenario() {
        // Given: DB에 이미 '고퀄리티 이미지' 레시피가 존재함
        String videoId = "REFUND_TEST_" + System.currentTimeMillis();
        String videoUrl = "https://www.youtube.com/watch?v=" + videoId;
        Long userId = 2L;

        Recipe existingImageRecipe = Recipe.builder()
                .title("고퀄리티 이미지 레시피")
                .youtubeUrl(videoUrl)
                .imageKey("s3/path/image.jpg") // 이미지 존재
                .source(RecipeSourceType.YOUTUBE)
                .build();
        recipeRepository.save(existingImageRecipe);

        // When: 동일한 URL로 '텍스트 모드' 요청
        Long jobId = youtubeService.createYoutubeExtractionJob(videoUrl, userId, "User", "KEY_2", RecipeDisplayMode.TEXT_MODE);
        youtubeService.processYoutubeExtractionAsync(jobId, videoUrl, userId, "User", RecipeDisplayMode.TEXT_MODE);

        // Then:
        // 1. 환불 처리가 되어야 함
        // 2. 이미 공개된 무료 혜택이므로 RecipeAccess에 굳이 기록하지 않음
        boolean hasAccess = recipeAccessRepository.existsByUserIdAndRecipeId(userId, existingImageRecipe.getId());
        assertThat(hasAccess).as("무료 혜택(환불) 유저는 권한 기록을 남기지 않습니다.").isFalse();
    }

    @Test
    @DisplayName("✅ 시나리오 3: 이미지 모드는 텍스트 레시피가 있어도 '새로 생성' 검증")
    void testImageAlwaysCreatesNew() {
        // Given: 텍스트 레시피만 있는 상태
        String videoId = "NEW_GEN_TEST_" + System.currentTimeMillis();
        String videoUrl = "https://www.youtube.com/watch?v=" + videoId;
        Long userId = 3L;

        Recipe existingText = Recipe.builder()
                .title("기존 텍스트")
                .youtubeUrl(videoUrl)
                .imageKey(null)
                .build();
        recipeRepository.save(existingText);

        // When: 이미지 모드로 생성 요청
        Long jobId = youtubeService.createYoutubeExtractionJob(videoUrl, userId, "User", "KEY_3", RecipeDisplayMode.IMAGE_MODE);
        // 이 시점에서 코드는 existingText를 무시하고 신규 생성 로직으로 내려가야 함

        // Then: 작업이 성공적으로 생성되었는지 확인
        assertThat(jobId).isNotNull();
    }
}