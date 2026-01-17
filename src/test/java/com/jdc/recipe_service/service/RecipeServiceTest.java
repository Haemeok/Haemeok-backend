package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

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
        when(userRepository.findById(author.getId()))
                .thenReturn(Optional.of(author));

        doAnswer(invocation -> {
            Recipe toSave = invocation.getArgument(0);
            Field idField = Recipe.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(toSave, 555L);
            return toSave;
        }).when(recipeRepository).save(any(Recipe.class));

        when(recipeIngredientService.saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.USER)))
                .thenReturn(0);

        when(recipeIngredientRepository.findByRecipeId(anyLong()))
                .thenReturn(Collections.emptyList());

        doNothing().when(recipeStepService).saveAll(any(Recipe.class), anyList());
        doNothing().when(recipeTagService).saveAll(any(Recipe.class), anyList());

        when(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .thenReturn(Collections.emptyList());

        Recipe fullRecipe = Recipe.builder()
                .id(555L)
                .user(author)
                .title("신규 레시피")
                .build();
        when(recipeRepository.findWithAllRelationsById(555L))
                .thenReturn(Optional.of(fullRecipe));

        RecipeWithImageUploadRequest requestWithFile = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();

        PresignedUrlResponse response = recipeService.createRecipeAndGenerateUrls(
                requestWithFile, author.getId(), RecipeSourceType.USER, null);

        assertNotNull(response);
        assertEquals(555L, response.getRecipeId());
        assertTrue(response.getUploads().isEmpty());

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
        when(userRepository.findById(author.getId()))
                .thenReturn(Optional.of(author));

        createDto.setIsPrivate(null);

        RecipeWithImageUploadRequest requestWithoutFile = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(emptyFiles)
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.createRecipeAndGenerateUrls(requestWithoutFile, author.getId(), RecipeSourceType.USER, null);
        });
        assertEquals(ErrorCode.USER_RECIPE_IMAGE_REQUIRED, ex.getErrorCode());

        verify(userRepository, times(1)).findById(author.getId());
        verifyNoInteractions(recipeRepository);
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: 사용자 없는 경우 USER_NOT_FOUND 예외")
    void createRecipe_userNotFound_throwsException() {
        when(userRepository.findById(999L)).thenReturn(Optional.empty());

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(emptyFiles)
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.createRecipeAndGenerateUrls(request, 999L, RecipeSourceType.USER, null);
        });
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(userRepository, times(1)).findById(999L);
        verifyNoMoreInteractions(recipeRepository);
    }

    @Test
    @DisplayName("updateUserRecipe: 정상 수정 시 PresignedUrlResponse 반환")
    void updateRecipe_success() {
        Recipe existing = Recipe.builder()
                .id(100L)
                .user(author)
                .title("기존 레시피")
                .build();
        when(recipeRepository.findWithUserById(100L)).thenReturn(Optional.of(existing));
        when(recipeRepository.findWithAllRelationsById(100L))
                .thenReturn(Optional.of(existing));

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

        when(recipeIngredientService.updateIngredientsFromUser(eq(existing), anyList()))
                .thenReturn(0);

        when(recipeIngredientRepository.findByRecipeId(anyLong()))
                .thenReturn(Collections.emptyList());

        doNothing().when(recipeStepService).updateStepsFromUser(eq(existing), anyList());
        doNothing().when(recipeTagService).updateTags(eq(existing), anyList());

        doNothing().when(recipeAnalysisService).analyzeRecipeAsync(anyLong());
        doNothing().when(recipeIndexingService).indexRecipeSafelyWithRetry(100L);

        when(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .thenReturn(Collections.emptyList());

        PresignedUrlResponse response = recipeService.updateUserRecipe(
                100L, author.getId(), updateRequest);

        assertNotNull(response);
        assertEquals(100L, response.getRecipeId());
        assertTrue(response.getUploads().isEmpty());

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
        when(recipeRepository.findWithUserById(2000L)).thenReturn(Optional.empty());

        RecipeUpdateWithImageRequest dummyRequest = RecipeUpdateWithImageRequest.builder()
                .recipe(RecipeUpdateRequestDto.builder().build())
                .files(emptyFiles)
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.updateUserRecipe(2000L, author.getId(), dummyRequest);
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(2000L);
        verifyNoMoreInteractions(recipeIngredientService, recipeStepService, recipeTagService);
    }

    @Test
    @DisplayName("updateUserRecipe: 소유자가 아닌 사용자가 수정 시 RECIPE_ACCESS_DENIED 예외")
    void updateRecipe_notOwner_throwsException() {
        User otherUser = User.builder().id(99L).build();
        Recipe existing = Recipe.builder()
                .id(300L)
                .user(otherUser)
                .title("남의 레시피")
                .build();
        when(recipeRepository.findWithUserById(300L))
                .thenReturn(Optional.of(existing));

        RecipeUpdateWithImageRequest dummyRequest = RecipeUpdateWithImageRequest.builder()
                .recipe(RecipeUpdateRequestDto.builder().build()) // 빈 UpdateDto
                .files(emptyFiles)
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.updateUserRecipe(300L, author.getId(), dummyRequest);
        });
        assertEquals(ErrorCode.RECIPE_ACCESS_DENIED, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(300L);
        verifyNoMoreInteractions(recipeIngredientService, recipeStepService, recipeTagService);
    }

    @Test
    @DisplayName("deleteRecipe: 정상 삭제 시 recipeId 반환")
    void deleteRecipe_success() {
        Recipe existing = Recipe.builder()
                .id(400L)
                .user(author)
                .title("삭제할 레시피")
                .build();
        when(recipeRepository.findWithUserById(400L)).thenReturn(Optional.of(existing));

        doNothing().when(recipeImageService).deleteImagesByRecipeId(400L);
        doNothing().when(recipeLikeService).deleteByRecipeId(400L);
        doNothing().when(recipeFavoriteService).deleteByRecipeId(400L);
        doNothing().when(commentService).deleteAllByRecipeId(400L);
        doNothing().when(recipeStepService).deleteAllByRecipeId(400L);
        doNothing().when(recipeIngredientService).deleteAllByRecipeId(400L);
        doNothing().when(recipeTagService).deleteAllByRecipeId(400L);
        doNothing().when(recipeIndexingService).deleteRecipeSafelyWithRetry(400L);
        doNothing().when(recipeRatingRepository).deleteByRecipeId(400L);

        Long result = recipeService.deleteRecipe(400L, author.getId());
        assertEquals(400L, result);

        verify(recipeRepository, times(1)).findWithUserById(400L);
        verify(recipeImageService, times(1)).deleteImagesByRecipeId(400L);
        verify(recipeLikeService, times(1)).deleteByRecipeId(400L);
        verify(recipeFavoriteService, times(1)).deleteByRecipeId(400L);
        verify(commentService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeStepService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeIngredientService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeTagService, times(1)).deleteAllByRecipeId(400L);
        verify(recipeRatingRepository, times(1)).deleteByRecipeId(400L);
        verify(recipeRepository, times(1)).deleteByIdDirectly(400L);
        verify(recipeIndexingService, times(1)).deleteRecipeSafelyWithRetry(400L);
    }

    @Test
    @DisplayName("deleteRecipe: 존재하지 않는 레시피 삭제 시 RECIPE_NOT_FOUND 예외")
    void deleteRecipe_notFound_throwsException() {
        when(recipeRepository.findWithUserById(500L)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.deleteRecipe(500L, author.getId());
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(500L);
        verifyNoMoreInteractions(recipeImageService, recipeLikeService, recipeFavoriteService,
                commentService, recipeStepService, recipeIngredientService, recipeTagService, recipeIndexingService);
    }

    @Test
    @DisplayName("deleteRecipe: 소유자가 아닌 사용자가 삭제 시 RECIPE_ACCESS_DENIED 예외")
    void deleteRecipe_notOwner_throwsException() {
        User otherUser = User.builder().id(77L).build();
        Recipe existing = Recipe.builder()
                .id(600L)
                .user(otherUser)
                .title("남의 레시피")
                .build();
        when(recipeRepository.findWithUserById(600L))
                .thenReturn(Optional.of(existing));

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.deleteRecipe(600L, author.getId());
        });
        assertEquals(ErrorCode.RECIPE_ACCESS_DENIED, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(600L);
        verifyNoMoreInteractions(recipeImageService, recipeLikeService, recipeFavoriteService,
                commentService, recipeStepService, recipeIngredientService, recipeTagService, recipeIndexingService);
    }

    @Test
    @DisplayName("togglePrivacy: 일반 사용자 레시피 공개/비공개 전환 성공")
    void togglePrivacy_success() {
        Recipe recipe = Recipe.builder()
                .id(123L)
                .user(author)
                .isPrivate(true)
                .isAiGenerated(false)
                .imageKey("image.jpg")
                .build();

        when(recipeRepository.findWithUserById(123L)).thenReturn(Optional.of(recipe));

        boolean result = recipeService.togglePrivacy(123L, author.getId());
        assertFalse(result);

        verify(recipeRepository).findWithUserById(123L);
    }

    @Test
    @DisplayName("togglePrivacy: AI 레시피가 이미지 없이 공개되려 할 때 예외")
    void togglePrivacy_aiRecipeWithoutImage_throws() {
        Recipe recipe = Recipe.builder()
                .id(124L)
                .user(author)
                .isPrivate(true)
                .isAiGenerated(true)
                .imageKey(null)
                .build();

        when(recipeRepository.findWithUserById(124L)).thenReturn(Optional.of(recipe));

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeService.togglePrivacy(124L, author.getId());
        });

        assertEquals(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE, ex.getErrorCode());
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: AI 레시피 생성 시 로그가 정상적으로 저장되는지 검증")
    void createAiRecipe_logsActivity() {
        Long userId = author.getId();
        String nickname = author.getNickname();
        AiRecipeConcept concept = AiRecipeConcept.FINE_DINING;

        when(userRepository.findById(userId)).thenReturn(Optional.of(author));

        doAnswer(inv -> {
            Recipe r = inv.getArgument(0);
            Field id = Recipe.class.getDeclaredField("id");
            id.setAccessible(true);
            id.set(r, 777L);
            return r;
        }).when(recipeRepository).save(any(Recipe.class));

        when(recipeIngredientService.saveAll(any(), any(), eq(RecipeSourceType.AI))).thenReturn(10000);
        when(recipeIngredientRepository.findByRecipeId(any())).thenReturn(Collections.emptyList());
        when(recipeRepository.findWithAllRelationsById(any())).thenReturn(Optional.of(Recipe.builder().user(author).id(777L).build()));

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();


        recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.AI, concept);


        verify(recipeActivityService, times(1)).saveLog(
                eq(userId),
                eq(nickname),
                eq(ActivityLogType.AI_RECIPE_FINE_DINING)
        );
    }

    @Test
    @DisplayName("createRecipeAndGenerateUrls: 유저 직접 생성 시에는 로그가 저장되지 않아야 함")
    void createUserRecipe_doesNotLog() {
        Long userId = author.getId();

        when(userRepository.findById(userId)).thenReturn(Optional.of(author));
        doAnswer(inv -> {
            Recipe r = inv.getArgument(0);
            Field id = Recipe.class.getDeclaredField("id");
            id.setAccessible(true);
            id.set(r, 888L);
            return r;
        }).when(recipeRepository).save(any(Recipe.class));

        when(recipeIngredientService.saveAll(any(), any(), eq(RecipeSourceType.USER))).thenReturn(5000);
        when(recipeIngredientRepository.findByRecipeId(any())).thenReturn(Collections.emptyList());
        when(recipeRepository.findWithAllRelationsById(any())).thenReturn(Optional.of(Recipe.builder().user(author).id(888L).build()));

        RecipeWithImageUploadRequest request = RecipeWithImageUploadRequest.builder()
                .recipe(createDto)
                .files(nonEmptyFiles)
                .build();

        recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.USER, null);

        verify(recipeActivityService, never()).saveLog(any(), any(), any());
    }

}