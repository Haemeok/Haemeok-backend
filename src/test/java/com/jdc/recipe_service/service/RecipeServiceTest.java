package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.domain.repository.IngredientCandidateRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import org.mockito.ArgumentCaptor;
import com.jdc.recipe_service.service.ai.RecipeAnalysisService;
import com.jdc.recipe_service.service.image.RecipeImageService;
import com.jdc.recipe_service.util.S3Util;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeServiceTest {

    @Mock private RecipeRepository recipeRepository;
    @Mock private UserRepository userRepository;
    @Mock private RecipeIngredientRepository recipeIngredientRepository;
    @Mock private RecipeRatingRepository recipeRatingRepository;

    @Mock private RecipeIngredientService recipeIngredientService;
    @Mock private RecipeStepService recipeStepService;
    @Mock private RecipeTagService recipeTagService;
    @Mock private RecipeFavoriteService recipeFavoriteService;
    @Mock private CommentService commentService;
    @Mock private RecipeImageService recipeImageService;
    @Mock private RecipeLikeService recipeLikeService;
    @Mock private CookingRecordRepository cookingRecordRepository;
    @Mock private IngredientCandidateRepository ingredientCandidateRepository;
    @Mock private RecipeIndexingService recipeIndexingService;
    @Mock private RecipeAnalysisService recipeAnalysisService;
    @Mock private RecipeActivityService recipeActivityService;

    @Mock private S3Util s3Util;
    @Mock private ObjectMapper objectMapper;
    @Mock private EntityManager em;
    @Mock private ApplicationEventPublisher publisher;

    @InjectMocks
    private RecipeService recipeService;

    private User author;
    private RecipeCreateRequestDto createDto;
    private List<FileInfoRequest> emptyFiles;
    private List<FileInfoRequest> nonEmptyFiles;
    private MockedStatic<TransactionSynchronizationManager> mockedTxnManager;

    @BeforeEach
    void setUp() {
        author = User.builder()
                .id(10L)
                .nickname("author")
                .build();

        emptyFiles = Collections.emptyList();

        FileInfoRequest mainFile = FileInfoRequest.builder()
                .type("main")
                .build();

        FileInfoRequest step0 = FileInfoRequest.builder()
                .type("step")
                .stepIndex(0)
                .build();
        FileInfoRequest step1 = FileInfoRequest.builder()
                .type("step")
                .stepIndex(1)
                .build();

        nonEmptyFiles = Collections.singletonList(mainFile);

        createDto = RecipeCreateRequestDto.builder()
                .title("신규 레시피")
                .dishType("볶음")
                .isPrivate(false)
                .ingredients(Collections.emptyList())
                .steps(Collections.emptyList())
                .tags(Collections.emptyList())
                .build();

        mockedTxnManager = Mockito.mockStatic(TransactionSynchronizationManager.class);
        mockedTxnManager.when(() -> TransactionSynchronizationManager.registerSynchronization(any(TransactionSynchronization.class)))
                .thenAnswer(invocation -> {
                    TransactionSynchronization sync = invocation.getArgument(0);
                    sync.afterCommit();
                    return null;
                });
    }

    @AfterEach
    void tearDown() {
        if (mockedTxnManager != null) {
            mockedTxnManager.close();
        }
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: 정상 입력 시 PresignedUrlResponse 반환")
    void createRecipe_success() throws Exception {
        // Given
        given(userRepository.findById(author.getId()))
                .willReturn(Optional.of(author));

        willAnswer(invocation -> {
            Recipe toSave = invocation.getArgument(0);
            Field idField = Recipe.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(toSave, 555L);
            return toSave;
        }).given(recipeRepository).save(any(Recipe.class));

        given(recipeIngredientService.saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.USER)))
                .willReturn(0);

        given(recipeIngredientRepository.findByRecipeId(anyLong()))
                .willReturn(Collections.emptyList());

        willDoNothing().given(recipeStepService).saveAll(any(Recipe.class), anyList());
        willDoNothing().given(recipeTagService).saveAll(any(Recipe.class), anyList());

        given(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .willReturn(Collections.emptyList());

        Recipe fullRecipe = Recipe.builder()
                .id(555L)
                .user(author)
                .title("신규 레시피")
                .build();
        given(recipeRepository.findWithAllRelationsById(555L))
                .willReturn(Optional.of(fullRecipe));

        RecipeWithImageUploadRequest requestWithFile = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();

        // When
        PresignedUrlResponse response = recipeService.createRecipeAndGenerateUrls(
                requestWithFile, author.getId(), RecipeSourceType.USER, null);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getRecipeId()).isEqualTo(555L);
        assertThat(response.getUploads()).isEmpty();

        verify(userRepository, times(1)).findById(author.getId());
        verify(recipeRepository, times(1)).save(any(Recipe.class));
        verify(recipeIngredientService, times(1))
                .saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.USER));
        verify(recipeStepService, times(1))
                .saveAll(any(Recipe.class), anyList());
        verify(recipeTagService, times(1))
                .saveAll(any(Recipe.class), anyList());
        verify(recipeImageService, times(1))
                .generateAndSavePresignedUrls(any(Recipe.class), anyList());
        verify(recipeRepository, times(1)).findWithAllRelationsById(555L);
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: main 타입 파일 없이 요청 시 USER_RECIPE_IMAGE_REQUIRED 예외")
    void createRecipe_noMainFile_throwsImageRequired() {
        // Given
        given(userRepository.findById(author.getId()))
                .willReturn(Optional.of(author));

        createDto.setIsPrivate(null);

        RecipeWithImageUploadRequest requestWithoutFile = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(emptyFiles)
                .build();

        // When & Then
        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(requestWithoutFile, author.getId(), RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_RECIPE_IMAGE_REQUIRED));

        verify(userRepository, times(1)).findById(author.getId());
        verifyNoInteractions(recipeRepository);
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: 사용자 없는 경우 USER_NOT_FOUND 예외")
    void createRecipe_userNotFound_throwsException() {
        // Given
        given(userRepository.findById(999L)).willReturn(Optional.empty());

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(emptyFiles)
                .build();

        // When & Then
        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(request, 999L, RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_NOT_FOUND));

        verify(userRepository, times(1)).findById(999L);
        verifyNoMoreInteractions(recipeRepository);
    }

    @Test
    @DisplayName("updateUserRecipe: 정상 수정 시 PresignedUrlResponse 반환")
    void updateRecipe_success() {
        // Given
        Recipe existing = Recipe.builder()
                .id(100L)
                .user(author)
                .title("기존 레시피")
                .build();
        given(recipeRepository.findWithUserById(100L)).willReturn(Optional.of(existing));
        given(recipeRepository.findWithAllRelationsById(100L))
                .willReturn(Optional.of(existing));

        RecipeUpdateRequestDto updateDto = RecipeUpdateRequestDto.builder()
                .title("수정된 레시피")
                .dishType("볶음")
                .isPrivate(false)
                .ingredients(Collections.emptyList())
                .steps(Collections.emptyList())
                .tags(Collections.emptyList())
                .isIngredientsModified(true)
                .build();

        RecipeUpdateWithImageRequest updateRequest = RecipeUpdateWithImageRequest.builder()
                .recipe(updateDto)
                .files(nonEmptyFiles)
                .build();

        given(recipeIngredientService.updateIngredientsFromUser(eq(existing), anyList()))
                .willReturn(0);

        given(recipeIngredientRepository.findByRecipeId(anyLong()))
                .willReturn(Collections.emptyList());

        willDoNothing().given(recipeStepService).updateStepsFromUser(eq(existing), anyList());
        willDoNothing().given(recipeTagService).updateTags(eq(existing), anyList());

        willDoNothing().given(recipeAnalysisService).analyzeRecipeAsync(anyLong());
        willDoNothing().given(recipeIndexingService).indexRecipeSafelyWithRetry(100L);

        given(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .willReturn(Collections.emptyList());

        // When
        PresignedUrlResponse response = recipeService.updateUserRecipe(
                100L, author.getId(), updateRequest);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getRecipeId()).isEqualTo(100L);
        assertThat(response.getUploads()).isEmpty();

        verify(recipeRepository, times(1)).findWithUserById(100L);
        verify(recipeIngredientService, times(1))
                .updateIngredientsFromUser(eq(existing), anyList());
        verify(recipeStepService, times(1))
                .updateStepsFromUser(eq(existing), anyList());
        verify(recipeTagService, times(1))
                .updateTags(eq(existing), anyList());
        verify(recipeIndexingService, times(1)).indexRecipeSafelyWithRetry(100L);
        verify(recipeImageService, times(1))
                .generateAndSavePresignedUrls(any(Recipe.class), anyList());
    }

    @Test
    @DisplayName("updateUserRecipe: 존재하지 않는 레시피 수정 시 RECIPE_NOT_FOUND 예외")
    void updateRecipe_notFound_throwsException() {
        // Given
        given(recipeRepository.findWithUserById(2000L)).willReturn(Optional.empty());

        RecipeUpdateWithImageRequest dummyRequest = RecipeUpdateWithImageRequest.builder()
                .recipe(RecipeUpdateRequestDto.builder().build())
                .files(emptyFiles)
                .build();

        // When & Then
        assertThatThrownBy(() -> recipeService.updateUserRecipe(2000L, author.getId(), dummyRequest))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(recipeRepository, times(1)).findWithUserById(2000L);
        verifyNoMoreInteractions(recipeIngredientService, recipeStepService, recipeTagService);
    }

    @Test
    @DisplayName("updateUserRecipe: 소유자가 아닌 사용자가 수정 시 RECIPE_ACCESS_DENIED 예외")
    void updateRecipe_notOwner_throwsException() {
        // Given
        User otherUser = User.builder().id(99L).build();
        Recipe existing = Recipe.builder()
                .id(300L)
                .user(otherUser)
                .title("남의 레시피")
                .build();
        given(recipeRepository.findWithUserById(300L))
                .willReturn(Optional.of(existing));

        RecipeUpdateWithImageRequest dummyRequest = RecipeUpdateWithImageRequest.builder()
                .recipe(RecipeUpdateRequestDto.builder().build()) // 빈 UpdateDto
                .files(emptyFiles)
                .build();

        // When & Then
        assertThatThrownBy(() -> recipeService.updateUserRecipe(300L, author.getId(), dummyRequest))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED));

        verify(recipeRepository, times(1)).findWithUserById(300L);
        verifyNoMoreInteractions(recipeIngredientService, recipeStepService, recipeTagService);
    }

    @Test
    @DisplayName("deleteRecipe: 정상 삭제 시 recipeId 반환 (연관 데이터는 DB cascade)")
    void deleteRecipe_success() {
        // Given
        Recipe existing = Recipe.builder()
                .id(400L)
                .user(author)
                .title("삭제할 레시피")
                .build();
        given(recipeRepository.findWithUserById(400L)).willReturn(Optional.of(existing));

        // When
        Long result = recipeService.deleteRecipe(400L, author.getId());

        // Then
        assertThat(result).isEqualTo(400L);

        verify(recipeRepository, times(1)).findWithUserById(400L);
        verify(recipeImageService, times(1)).deleteImagesByRecipeId(400L);
        verify(recipeLikeService, times(1)).deleteByRecipeId(400L);
        verify(recipeFavoriteService, times(1)).deleteByRecipeId(400L);
        verify(commentService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeRatingRepository, times(1)).deleteByRecipeId(400L);
        verify(cookingRecordRepository, times(1)).deleteByRecipeId(400L);
        verify(recipeStepService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeIngredientService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeTagService, times(1)).deleteAllByRecipeId(400L);
        verify(ingredientCandidateRepository, times(1)).clearSourceRecipeId(400L);
        verify(recipeRepository, times(1)).deleteByIdDirectly(400L);
        verify(recipeIndexingService, times(1)).deleteRecipeSafelyWithRetry(400L);
        // ingredient_candidates는 후보 큐 보존을 위해 삭제하지 않고 recipe FK만 끊는다.
        InOrder deleteOrder = inOrder(ingredientCandidateRepository, recipeRepository);
        deleteOrder.verify(ingredientCandidateRepository).clearSourceRecipeId(400L);
        deleteOrder.verify(recipeRepository).deleteByIdDirectly(400L);
    }

    @Test
    @DisplayName("deleteRecipe: 존재하지 않는 레시피 삭제 시 RECIPE_NOT_FOUND 예외")
    void deleteRecipe_notFound_throwsException() {
        // Given
        given(recipeRepository.findWithUserById(500L)).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> recipeService.deleteRecipe(500L, author.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(recipeRepository, times(1)).findWithUserById(500L);
        verifyNoMoreInteractions(recipeImageService, recipeLikeService, recipeFavoriteService,
                commentService, recipeRatingRepository, cookingRecordRepository, recipeStepService,
                recipeIngredientService, recipeTagService, ingredientCandidateRepository, recipeIndexingService);
    }

    @Test
    @DisplayName("deleteRecipe: 소유자가 아닌 사용자가 삭제 시 RECIPE_ACCESS_DENIED 예외")
    void deleteRecipe_notOwner_throwsException() {
        // Given
        User otherUser = User.builder().id(77L).build();
        Recipe existing = Recipe.builder()
                .id(600L)
                .user(otherUser)
                .title("남의 레시피")
                .build();
        given(recipeRepository.findWithUserById(600L))
                .willReturn(Optional.of(existing));

        // When & Then
        assertThatThrownBy(() -> recipeService.deleteRecipe(600L, author.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED));

        verify(recipeRepository, times(1)).findWithUserById(600L);
        verifyNoMoreInteractions(recipeImageService, recipeLikeService, recipeFavoriteService,
                commentService, recipeRatingRepository, cookingRecordRepository, recipeStepService,
                recipeIngredientService, recipeTagService, ingredientCandidateRepository, recipeIndexingService);
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls(USER): isPrivate=true 입력 시 트리플 (PRIVATE+UNLISTED+isPrivate=true) 정규화")
    void createRecipe_userPrivate_appliesTriple() throws Exception {
        // Given - 사용자가 비공개 의도로 레시피 생성
        given(userRepository.findById(author.getId())).willReturn(Optional.of(author));
        willAnswer(invocation -> {
            Recipe toSave = invocation.getArgument(0);
            Field idField = Recipe.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(toSave, 556L);
            return toSave;
        }).given(recipeRepository).save(any(Recipe.class));
        given(recipeIngredientService.saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.USER)))
                .willReturn(0);
        given(recipeIngredientRepository.findByRecipeId(anyLong())).willReturn(Collections.emptyList());
        given(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .willReturn(Collections.emptyList());
        Recipe fullRecipe = Recipe.builder().id(556L).user(author).build();
        given(recipeRepository.findWithAllRelationsById(556L)).willReturn(Optional.of(fullRecipe));

        RecipeCreateRequestDto privateDto = RecipeCreateRequestDto.builder()
                .title("비공개 레시피")
                .dishType("볶음")
                .isPrivate(true)  // ← 핵심: 사용자가 비공개 입력
                .ingredients(Collections.emptyList())
                .steps(Collections.emptyList())
                .tags(Collections.emptyList())
                .build();
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder()
                .recipe(privateDto)
                .files(nonEmptyFiles)
                .build();

        // When
        recipeService.createRecipeAndGenerateUrls(req, author.getId(), RecipeSourceType.USER, null);

        // Then - save된 entity의 트리플이 정규화됐는지 (entity default에 의존하면 PUBLIC+LISTED+isPrivate=true 깨진 row)
        ArgumentCaptor<Recipe> captor = ArgumentCaptor.forClass(Recipe.class);
        verify(recipeRepository).save(captor.capture());
        Recipe persisted = captor.getValue();
        assertThat(persisted.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(persisted.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(persisted.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls(USER): isPrivate=false (default) 입력 시 트리플 (PUBLIC+LISTED+isPrivate=false)")
    void createRecipe_userPublic_appliesTriple() throws Exception {
        given(userRepository.findById(author.getId())).willReturn(Optional.of(author));
        willAnswer(invocation -> {
            Recipe toSave = invocation.getArgument(0);
            Field idField = Recipe.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(toSave, 557L);
            return toSave;
        }).given(recipeRepository).save(any(Recipe.class));
        given(recipeIngredientService.saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.USER)))
                .willReturn(0);
        given(recipeIngredientRepository.findByRecipeId(anyLong())).willReturn(Collections.emptyList());
        given(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .willReturn(Collections.emptyList());
        Recipe fullRecipe = Recipe.builder().id(557L).user(author).build();
        given(recipeRepository.findWithAllRelationsById(557L)).willReturn(Optional.of(fullRecipe));

        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)  // createDto는 isPrivate=false
                .files(nonEmptyFiles)
                .build();

        recipeService.createRecipeAndGenerateUrls(req, author.getId(), RecipeSourceType.USER, null);

        ArgumentCaptor<Recipe> captor = ArgumentCaptor.forClass(Recipe.class);
        verify(recipeRepository).save(captor.capture());
        Recipe persisted = captor.getValue();
        assertThat(persisted.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(persisted.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(persisted.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("finalizeRecipeImages: 일반 원본 user 레시피 → PUBLIC+LISTED+isPrivate=false 트리플 (단일 setter 깨진 조합 방지)")
    void finalize_originalUserRecipe_appliesPublicListed() {
        // Given - PRIVATE 상태의 일반 원본 user 레시피 (origin 없음)
        Recipe recipe = Recipe.builder()
                .id(700L)
                .user(author)
                .visibility(RecipeVisibility.PRIVATE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .isPrivate(true)
                .isAiGenerated(false)
                .build();
        given(recipeRepository.findWithStepsById(700L)).willReturn(Optional.of(recipe));
        given(recipeImageService.getImagesByRecipeId(700L)).willReturn(Collections.emptyList());
        given(s3Util.listKeysInFolder(anyString())).willReturn(Collections.emptySet());

        // When - finalize 호출 (이미지 finalize 후 공개 전환 V1 정책)
        recipeService.finalizeRecipeImages(700L, author.getId(), false);

        // Then - 단일 updateIsPrivate(false) 호출이었다면 PRIVATE+UNLISTED+isPrivate=false 깨진 조합.
        // 헬퍼가 트리플 정규화하는지 검증.
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("finalizeRecipeImages: 리믹스 user 레시피 → PUBLIC+UNLISTED+isPrivate=false 트리플 (link-only 보존)")
    void finalize_remixUserRecipe_appliesPublicUnlisted() {
        // Given - 리믹스 (origin 있음)
        Recipe origin = Recipe.builder().id(799L).build();
        Recipe remix = Recipe.builder()
                .id(701L)
                .user(author)
                .visibility(RecipeVisibility.PRIVATE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .isPrivate(true)
                .isAiGenerated(false)
                .originRecipe(origin)
                .build();
        given(recipeRepository.findWithStepsById(701L)).willReturn(Optional.of(remix));
        given(recipeImageService.getImagesByRecipeId(701L)).willReturn(Collections.emptyList());
        given(s3Util.listKeysInFolder(anyString())).willReturn(Collections.emptySet());

        // When
        recipeService.finalizeRecipeImages(701L, author.getId(), false);

        // Then - 리믹스는 PUBLIC으로 풀려도 검색/추천 누출 차단. UNLISTED 보존.
        assertThat(remix.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(remix.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(remix.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("togglePrivacy: 일반 원본의 PRIVATE → PUBLIC = PUBLIC+LISTED+isPrivate=false (검색 노출)")
    void togglePrivacy_originalRecipe_privateToPublic_setsListed() {
        // Given - origin 없는 일반 원본 레시피, PRIVATE 상태
        Recipe recipe = Recipe.builder()
                .id(123L)
                .user(author)
                .isPrivate(true)
                .isAiGenerated(false)
                .imageKey("image.jpg")
                .build();

        given(recipeRepository.findWithUserById(123L)).willReturn(Optional.of(recipe));

        // When
        boolean result = recipeService.togglePrivacy(123L, author.getId());

        // Then - 일반 원본은 LISTED로 풀려 검색에 노출됨
        assertThat(result).isFalse();
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(recipe.getIsPrivate()).isFalse();

        verify(recipeRepository).findWithUserById(123L);
    }

    @Test
    @DisplayName("togglePrivacy: 리믹스의 PRIVATE → PUBLIC = PUBLIC+UNLISTED+isPrivate=false (link-only 보존)")
    void togglePrivacy_remixRecipe_privateToPublic_setsUnlisted() {
        // Given - origin이 있는 리믹스 레시피 (link-only 정책 유지 대상)
        Recipe origin = Recipe.builder().id(999L).user(author).build();
        Recipe remix = Recipe.builder()
                .id(123L)
                .user(author)
                .isPrivate(true)
                .isAiGenerated(false)
                .imageKey("remix.jpg")
                .originRecipe(origin)
                .build();

        given(recipeRepository.findWithUserById(123L)).willReturn(Optional.of(remix));

        // When
        boolean result = recipeService.togglePrivacy(123L, author.getId());

        // Then - 리믹스는 검색/추천에 갑자기 노출되지 않도록 UNLISTED 유지
        assertThat(result).isFalse();
        assertThat(remix.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(remix.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(remix.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("togglePrivacy: PUBLIC → PRIVATE = PRIVATE+UNLISTED+isPrivate=true (origin 유무 무관)")
    void togglePrivacy_publicToPrivate_appliesPrivate() {
        // Given - PUBLIC 상태 (origin 유무는 PRIVATE 전환에서 영향 없음)
        Recipe recipe = Recipe.builder()
                .id(123L)
                .user(author)
                .isPrivate(false)
                .isAiGenerated(false)
                .imageKey("image.jpg")
                .build();

        given(recipeRepository.findWithUserById(123L)).willReturn(Optional.of(recipe));

        // When
        boolean result = recipeService.togglePrivacy(123L, author.getId());

        // Then
        assertThat(result).isTrue();
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("togglePrivacy: AI 레시피가 이미지 없이 공개되려 할 때 예외")
    void togglePrivacy_aiRecipeWithoutImage_throws() {
        // Given
        Recipe recipe = Recipe.builder()
                .id(124L)
                .user(author)
                .isPrivate(true)
                .isAiGenerated(true)
                .imageKey(null)
                .build();

        given(recipeRepository.findWithUserById(124L)).willReturn(Optional.of(recipe));

        // When & Then
        assertThatThrownBy(() -> recipeService.togglePrivacy(124L, author.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE));
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: AI 레시피 생성 시 로그가 정상적으로 저장되는지 검증")
    void createAiRecipe_logsActivity() {
        // Given
        Long userId = author.getId();
        String nickname = author.getNickname();
        AiRecipeConcept concept = AiRecipeConcept.FINE_DINING;

        given(userRepository.findById(userId)).willReturn(Optional.of(author));

        willAnswer(inv -> {
            Recipe r = inv.getArgument(0);
            Field id = Recipe.class.getDeclaredField("id");
            id.setAccessible(true);
            id.set(r, 777L);
            return r;
        }).given(recipeRepository).save(any(Recipe.class));

        given(recipeIngredientService.saveAll(any(), any(), eq(RecipeSourceType.AI))).willReturn(10000);
        given(recipeIngredientRepository.findByRecipeId(any())).willReturn(Collections.emptyList());
        given(recipeRepository.findWithAllRelationsById(any())).willReturn(Optional.of(Recipe.builder().user(author).id(777L).build()));

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();

        // When
        recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.AI, concept);

        // Then
        verify(recipeActivityService, times(1)).saveLog(
                eq(userId),
                eq(nickname),
                eq(ActivityLogType.AI_RECIPE_FINE_DINING)
        );
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: 유저 직접 생성 시에는 로그가 저장되지 않아야 함")
    void createUserRecipe_doesNotLog() {
        // Given
        Long userId = author.getId();

        given(userRepository.findById(userId)).willReturn(Optional.of(author));
        willAnswer(inv -> {
            Recipe r = inv.getArgument(0);
            Field id = Recipe.class.getDeclaredField("id");
            id.setAccessible(true);
            id.set(r, 888L);
            return r;
        }).given(recipeRepository).save(any(Recipe.class));

        given(recipeIngredientService.saveAll(any(), any(), eq(RecipeSourceType.USER))).willReturn(5000);
        given(recipeIngredientRepository.findByRecipeId(any())).willReturn(Collections.emptyList());
        given(recipeRepository.findWithAllRelationsById(any())).willReturn(Optional.of(Recipe.builder().user(author).id(888L).build()));

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();

        // When
        recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.USER, null);

        // Then
        verify(recipeActivityService, never()).saveLog(any(), any(), any());
    }

}
