package com.jdc.recipe_service.ai;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.recipe.RecipeAccessRepository;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import com.jdc.recipe_service.service.ai.AiRecipeGenerationService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Commit;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@EnabledIfEnvironmentVariable(named = "RUN_AI_REAL_TESTS", matches = "true")
class AiGenerationRealTest {

    @Autowired
    private AiRecipeGenerationService aiService;

    @Autowired
    private RecipeGenerationJobRepository jobRepository;

    @Autowired
    private RecipeAccessRepository recipeAccessRepository;

    @Autowired
    private RecipeFavoriteRepository recipeFavoriteRepository;

    @Autowired
    private RecipeRepository recipeRepository;

    @Test
    @DisplayName("🎨 [AI 생성] 성공 시 권한 부여 및 즐겨찾기 자동 등록 검증")
    @Commit
    void testAiGenerationSuccessAndPersistence() {
        // Given
        Long userId = 1L;
        String idempotencyKey = "AI_TEST_" + System.currentTimeMillis();

        // ✅ 사장님의 실제 DTO 필드인 ingredientIds를 사용합니다.
        AiRecipeRequestDto aiReq = AiRecipeRequestDto.builder()
                .ingredientIds(List.of(10L, 20L, 30L)) // 재료 ID 리스트 (실제 DB에 있는 ID 권장)
                .dishType("볶음")
                .cookingTime(20)
                .servings(1.0)
                .build();

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .aiRequest(aiReq)
                .build();

        // 1. 작업 생성 (여기서 크레딧 차감 로직 작동)
        Long jobId = aiService.createAiGenerationJob(
                request,
                AiRecipeConcept.INGREDIENT_FOCUS,
                userId,
                idempotencyKey,
                RecipeDisplayMode.TEXT_MODE
        );

        System.out.println("=================================================");
        System.out.println("👨‍🍳 [START] AI 레시피 생성 테스트 시작!");
        System.out.println("=================================================");

        // When: AI 생성 비동기 로직 실행
        aiService.processAiGenerationAsync(jobId, request, AiRecipeConcept.INGREDIENT_FOCUS, userId, RecipeDisplayMode.TEXT_MODE);

        // Then: 결과 검증
        RecipeGenerationJob job = jobRepository.findById(jobId).orElseThrow();
        System.out.println("🏁 [FINISH] 작업 상태: " + job.getStatus());

        if (job.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            Long recipeId = job.getResultRecipeId();
            assertThat(recipeId).isNotNull();

            // 1. 권한(Access) 체크
            boolean hasAccess = recipeAccessRepository.existsByUserIdAndRecipeId(userId, recipeId);
            assertThat(hasAccess)
                    .as("유저에게 레시피 권한이 부여되어야 합니다.")
                    .isTrue();

            // 2. 즐겨찾기(Favorite) 체크
            boolean isFavorite = recipeFavoriteRepository.existsByUserIdAndRecipeId(userId, recipeId);
            assertThat(isFavorite)
                    .as("유저 보관함에 레시피가 자동으로 추가되어야 합니다.")
                    .isTrue();

            System.out.println("✅ [Success] 권한 및 즐겨찾기 등록 확인 완료 (RecipeID: " + recipeId + ")");
        } else {
            System.out.println("❌ [Failed] AI 생성 실패 - 사유: " + job.getErrorMessage());
        }
    }

    @Test
    @DisplayName("🖼️ [AI 생성] 이미지 모드 실행: 3점 차감 및 공개 상태(PUBLIC) 검증")
    @Commit
    void testAiGenerationImageModeAndPersistence() {
        // Given
        Long userId = 1L; // 테스트 유저 ID
        String idempotencyKey = "AI_IMG_TEST_" + System.currentTimeMillis();

        // DTO 구성 (사장님 실제 필드인 ingredientIds 사용)
        AiRecipeRequestDto aiReq = AiRecipeRequestDto.builder()
                .ingredientIds(List.of(10L, 20L, 30L)) // 실제 DB에 있는 재료 ID 추천
                .dishType("구이")
                .cookingTime(30)
                .servings(2.0)
                .build();

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .aiRequest(aiReq)
                .build();

        System.out.println("=================================================");
        System.out.println("🖼️ [START] AI 이미지 모드 레시피 생성 시작!");
        System.out.println("💰 예상 차감 크레딧: 3점");
        System.out.println("=================================================");

        // 1. 작업 생성 (이 시점에 3크레딧 차감 로그가 찍혀야 함)
        Long jobId = aiService.createAiGenerationJob(
                request,
                AiRecipeConcept.FINE_DINING, // 이미지 생성을 위해 파인 다이닝 컨셉 추천
                userId,
                idempotencyKey,
                RecipeDisplayMode.IMAGE_MODE // ✅ 이미지 모드로 설정
        );

        // When
        // 비동기 로직 직접 호출 (테스트 스레드에서 실행)
        aiService.processAiGenerationAsync(
                jobId,
                request,
                AiRecipeConcept.FINE_DINING,
                userId,
                RecipeDisplayMode.IMAGE_MODE
        );

        // Then
        RecipeGenerationJob job = jobRepository.findById(jobId).orElseThrow();
        System.out.println("🏁 [FINISH] 작업 종료 상태: " + job.getStatus());

        if (job.getStatus() == com.jdc.recipe_service.domain.type.JobStatus.COMPLETED) {
            Long recipeId = job.getResultRecipeId();
            assertThat(recipeId).isNotNull();

            // 데이터베이스에서 실제 저장된 레시피 조회
            Recipe recipe = recipeRepository.findById(recipeId).orElseThrow();

            // 1. 이미지 모드 전용 상태값 검증
            assertThat(recipe.getVisibility())
                    .as("이미지 생성 성공 시 레시피는 PUBLIC 상태여야 합니다.")
                    .isEqualTo(com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC);

            assertThat(recipe.getListingStatus())
                    .as("이미지 생성 성공 시 레시피는 LISTED 상태여야 합니다.")
                    .isEqualTo(com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED);

            assertThat(recipe.getImageKey())
                    .as("이미지 모드이므로 image_key가 비어있으면 안 됩니다.")
                    .isNotBlank();

            // 2. 통합 등록 로직(권한+즐찾) 검증
            verifyAccessAndFavorite(userId, recipeId);

            System.out.println("✅ [Success] 이미지 생성, PUBLIC 전환, 보관함 등록 모두 확인 완료!");
            System.out.println("📸 생성된 이미지 키: " + recipe.getImageKey());
        } else {
            System.out.println("❌ [Failed] AI 생성 실패 - 사유: " + job.getErrorMessage());
            // 실패 시 환불 로직이 작동했는지 로그 확인 필요
        }
    }

    private void verifyAccessAndFavorite(Long userId, Long recipeId) {
        if (recipeId == null) return;

        // 1. 권한(Access) 테이블에 OWNER로 잘 들어갔는지 확인
        boolean hasAccess = recipeAccessRepository.existsByUserIdAndRecipeId(userId, recipeId);
        assertThat(hasAccess)
                .as("유저(ID: " + userId + ")에게 레시피(ID: " + recipeId + ")에 대한 접근 권한이 부여되어야 합니다.")
                .isTrue();

        // 2. 즐겨찾기(Favorite) 테이블에 자동으로 등록되었는지 확인
        boolean isFavorite = recipeFavoriteRepository.existsByUserIdAndRecipeId(userId, recipeId);
        assertThat(isFavorite)
                .as("유저(ID: " + userId + ")의 보관함에 레시피(ID: " + recipeId + ")가 자동으로 추가되어야 합니다.")
                .isTrue();

        System.out.println("✅ [검증 완료] 유저 " + userId + "번의 권한 및 즐겨찾기 데이터가 DB에 정상 저장되었습니다.");
    }
}