package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
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
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.BDDMockito.willDoNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Remix(prefill-save) 분기를 RecipeService.createRecipeAndGenerateUrls 기준으로 검증한다.
 *
 * 핵심 불변식:
 *   - 원본 cloneable 5조건 중 하나라도 실패 → RECIPE_REMIX_NOT_ALLOWED (403)
 *   - 동일 유저가 동일 원본으로 2회 이상 → RECIPE_REMIX_ALREADY_EXISTS (409)
 *       · 애플리케이션 레벨: existsByOriginRecipeIdAndUserId 선검사
 *       · DB 레벨: uq_recipes_origin_user 유니크 제약 → DataIntegrityViolationException 매핑
 *   - 서버가 body와 무관하게 visibility=PRIVATE, listingStatus=UNLISTED, isPrivate=true,
 *     source=YOUTUBE, extractorId=null, originRecipe=source 를 강제
 *   - 원본 imageKey가 있으면 S3 CopyObject로 list용(main.webp)과 detail용(main_detail.webp)
 *     두 변형을 모두 새 키에 복제. detail이 없는 legacy 원본은 main만 복사하고 경고 로그.
 *
 * DB 레벨 유니크 제약은 Flyway V20260414_003__add_origin_recipe_id_to_recipes.sql에서 관리된다.
 * RecipeService가 수많은 협력자를 가지기 때문에 @SpringBootTest 대신 Mockito로 분기와 부수효과만 고정한다.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RecipeServiceRemixCreateTest {

    private static final Long OFFICIAL_USER_ID = 90121L;
    private static final Long REMIX_USER_ID = 42L;
    private static final Long SOURCE_RECIPE_ID = 7L;
    private static final Long NEW_RECIPE_ID = 555L;
    private static final String SOURCE_IMAGE_KEY = "images/recipes/7/main.webp";
    private static final String SOURCE_DETAIL_IMAGE_KEY = "images/recipes/7/main_detail.webp";
    private static final String EXPECTED_NEW_IMAGE_KEY = "images/recipes/555/main.webp";
    private static final String EXPECTED_NEW_DETAIL_IMAGE_KEY = "images/recipes/555/main_detail.webp";

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

    private User officialUser;
    private User remixer;
    private MockedStatic<TransactionSynchronizationManager> mockedTxnManager;

    @BeforeEach
    void setUp() throws Exception {
        officialUser = userWithId(OFFICIAL_USER_ID, "official");
        remixer = userWithId(REMIX_USER_ID, "remixer");

        given(userRepository.findById(REMIX_USER_ID)).willReturn(Optional.of(remixer));

        willAnswer(inv -> {
            Recipe toSave = inv.getArgument(0);
            Field idField = Recipe.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(toSave, NEW_RECIPE_ID);
            return toSave;
        }).given(recipeRepository).save(any(Recipe.class));

        given(recipeIngredientService.saveAll(any(Recipe.class), anyList(), eq(RecipeSourceType.YOUTUBE)))
                .willReturn(0);
        given(recipeIngredientRepository.findByRecipeId(anyLong()))
                .willReturn(Collections.emptyList());

        willDoNothing().given(recipeStepService).saveAll(any(Recipe.class), anyList());
        willDoNothing().given(recipeTagService).saveAll(any(Recipe.class), anyList());

        given(recipeImageService.generateAndSavePresignedUrls(any(Recipe.class), anyList()))
                .willReturn(Collections.emptyList());

        // 기본적으로 detail 변형은 존재한다고 가정. legacy 시나리오만 별도 stub 으로 false 주입.
        given(s3Util.doesObjectExist(anyString())).willReturn(true);

        Recipe fullRecipe = Recipe.builder().id(NEW_RECIPE_ID).user(remixer).title("remix").build();
        given(recipeRepository.findWithAllRelationsById(NEW_RECIPE_ID))
                .willReturn(Optional.of(fullRecipe));

        mockedTxnManager = Mockito.mockStatic(TransactionSynchronizationManager.class);
        mockedTxnManager.when(() -> TransactionSynchronizationManager.registerSynchronization(any(TransactionSynchronization.class)))
                .thenAnswer(inv -> {
                    TransactionSynchronization sync = inv.getArgument(0);
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
    @DisplayName("유효한 원본으로 리믹스 시 PRIVATE/UNLISTED/isPrivate=true/source=YOUTUBE/originRecipe 강제, S3 CopyObject가 main + detail 모두 호출")
    void remix_withValidSource_forcesInvariantsAndCopiesImage() {
        // given
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);
        // 클라이언트가 PUBLIC/LISTED로 조작을 시도해도 서버가 덮어써야 한다
        dto.setVisibility(RecipeVisibility.PUBLIC);
        dto.setListingStatus(RecipeListingStatus.LISTED);
        dto.setIsPrivate(false);
        dto.setExtractorId(999L);

        // when
        recipeService.createRecipeAndGenerateUrls(wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null);

        // then
        ArgumentCaptor<Recipe> saved = ArgumentCaptor.forClass(Recipe.class);
        verify(recipeRepository).save(saved.capture());
        Recipe persisted = saved.getValue();

        assertThat(persisted.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(persisted.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(persisted.getIsPrivate()).isTrue();
        assertThat(persisted.getSource()).isEqualTo(RecipeSourceType.YOUTUBE);
        assertThat(persisted.getExtractorId()).isNull();
        assertThat(persisted.getOriginRecipe()).isSameAs(source);
        assertThat(persisted.getUser().getId()).isEqualTo(REMIX_USER_ID);

        verify(s3Util).copyObject(SOURCE_IMAGE_KEY, EXPECTED_NEW_IMAGE_KEY);
        verify(s3Util).copyObject(SOURCE_DETAIL_IMAGE_KEY, EXPECTED_NEW_DETAIL_IMAGE_KEY);
    }

    @Test
    @DisplayName("리믹스 + 새 메인 이미지 업로드 시 CopyObject 생략, 프리사인드 URL 경로로 업로드")
    void remix_whenNewMainUploaded_skipsCopyAndUsesPresignedUrl() {
        // given
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        FileInfoRequest mainFile = FileInfoRequest.builder()
                .type("main")
                .contentType("image/jpeg")
                .build();
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder()
                .recipe(dto)
                .files(List.of(mainFile))
                .build();

        // when
        recipeService.createRecipeAndGenerateUrls(req, REMIX_USER_ID, RecipeSourceType.USER, null);

        // then
        verify(s3Util, never()).copyObject(anyString(), anyString());

        ArgumentCaptor<Recipe> saved = ArgumentCaptor.forClass(Recipe.class);
        verify(recipeRepository).save(saved.capture());
        Recipe persisted = saved.getValue();

        // 리믹스 불변식은 유지되어야 한다
        assertThat(persisted.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(persisted.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(persisted.getIsPrivate()).isTrue();
        assertThat(persisted.getSource()).isEqualTo(RecipeSourceType.YOUTUBE);
        assertThat(persisted.getOriginRecipe()).isSameAs(source);
        // imageKey는 새 레시피의 자체 경로로 배정되어야 한다 (복사가 아닌 프리사인드 업로드 대상)
        assertThat(persisted.getImageKey()).isEqualTo(EXPECTED_NEW_IMAGE_KEY);

        verify(recipeImageService).generateAndSavePresignedUrls(any(Recipe.class), eq(List.of(mainFile)));
    }

    @Test
    @DisplayName("리믹스 + 스텝 이미지 업로드 시 해당 스텝 DTO에 imageKey가 할당되고 메인은 main + detail 모두 복사")
    void remix_whenStepImageUploaded_assignsStepImageKey() {
        // given
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);

        RecipeStepRequestDto step1 = RecipeStepRequestDto.builder()
                .stepNumber(1)
                .instruction("고기를 볶는다")
                .build();
        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);
        dto.setSteps(List.of(step1));

        FileInfoRequest stepFile = FileInfoRequest.builder()
                .type("step1")
                .stepIndex(1)
                .contentType("image/jpeg")
                .build();
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder()
                .recipe(dto)
                .files(List.of(stepFile))
                .build();

        // when
        recipeService.createRecipeAndGenerateUrls(req, REMIX_USER_ID, RecipeSourceType.USER, null);

        // then
        // 메인은 업로드 없음 → 원본 복사 동작 유지 (list + detail 두 변형)
        verify(s3Util).copyObject(SOURCE_IMAGE_KEY, EXPECTED_NEW_IMAGE_KEY);
        verify(s3Util).copyObject(SOURCE_DETAIL_IMAGE_KEY, EXPECTED_NEW_DETAIL_IMAGE_KEY);
        // 스텝 DTO에 imageKey가 할당되어 saveAll로 전달되어야 한다
        assertThat(step1.getImageKey())
                .isEqualTo("images/recipes/" + NEW_RECIPE_ID + "/step_1.webp");
    }

    @Test
    @DisplayName("원본 imageKey가 null이면 S3 CopyObject를 호출하지 않는다")
    void remix_whenSourceHasNoImage_skipsCopyObject() {
        // given
        Recipe source = cloneableSource().imageKey(null).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        // when
        recipeService.createRecipeAndGenerateUrls(wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null);

        // then
        verify(s3Util, never()).copyObject(anyString(), anyString());
    }

    @Test
    @DisplayName("원본에 detail 변형이 없는 legacy 케이스: main만 복사하고 detail은 skip")
    void remix_whenSourceHasNoDetailVariant_copiesOnlyMain() {
        // given
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);
        // legacy: detail 변형이 S3에 없는 상황을 명시적으로 주입
        given(s3Util.doesObjectExist(SOURCE_DETAIL_IMAGE_KEY)).willReturn(false);

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        // when
        recipeService.createRecipeAndGenerateUrls(wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null);

        // then
        verify(s3Util).copyObject(SOURCE_IMAGE_KEY, EXPECTED_NEW_IMAGE_KEY);
        verify(s3Util, never()).copyObject(SOURCE_DETAIL_IMAGE_KEY, EXPECTED_NEW_DETAIL_IMAGE_KEY);
    }

    @Test
    @DisplayName("원본 소유자가 공식 계정이 아니면 RECIPE_REMIX_NOT_ALLOWED")
    void remix_whenSourceNotOwnedByOfficial_throwsNotAllowed() {
        // given
        User unofficial = userWithId(77L, "unofficial");
        Recipe source = cloneableSource().user(unofficial).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));

        // when / then
        assertRemixRejected(source, ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    @Test
    @DisplayName("원본 source가 YOUTUBE가 아니면 RECIPE_REMIX_NOT_ALLOWED")
    void remix_whenSourceIsNotYoutube_throwsNotAllowed() {
        Recipe source = cloneableSource().source(RecipeSourceType.AI).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));

        assertRemixRejected(source, ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    @Test
    @DisplayName("원본이 이미 다른 레시피의 리믹스(originRecipe != null)면 RECIPE_REMIX_NOT_ALLOWED")
    void remix_whenSourceIsAlreadyARemix_throwsNotAllowed() {
        Recipe grandparent = cloneableSource().build();
        Recipe source = cloneableSource().originRecipe(grandparent).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));

        assertRemixRejected(source, ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    @Test
    @DisplayName("원본 lifecycleStatus가 ACTIVE가 아니면 RECIPE_REMIX_NOT_ALLOWED")
    void remix_whenSourceNotActive_throwsNotAllowed() {
        Recipe source = cloneableSource().lifecycleStatus(RecipeLifecycleStatus.DELETED).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));

        assertRemixRejected(source, ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    @Test
    @DisplayName("원본 visibility가 PUBLIC이 아니면 RECIPE_REMIX_NOT_ALLOWED")
    void remix_whenSourceNotPublic_throwsNotAllowed() {
        Recipe source = cloneableSource().visibility(RecipeVisibility.PRIVATE).build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));

        assertRemixRejected(source, ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    @Test
    @DisplayName("originRecipeId가 존재하지 않는 레시피면 RECIPE_NOT_FOUND")
    void remix_whenSourceNotFound_throwsRecipeNotFound() {
        // given
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.empty());

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        // when / then
        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(
                wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                        .isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(recipeRepository, never()).save(any(Recipe.class));
    }

    @Test
    @DisplayName("애플리케이션 선검사(existsByOriginRecipeIdAndUserId)가 true면 RECIPE_REMIX_ALREADY_EXISTS")
    void remix_whenApplicationLevelDuplicateCheckHits_throwsConflict() {
        // given
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(true);

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        // when / then
        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(
                wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                        .isEqualTo(ErrorCode.RECIPE_REMIX_ALREADY_EXISTS));

        verify(recipeRepository, never()).save(any(Recipe.class));
        verify(s3Util, never()).copyObject(anyString(), anyString());
    }

    @Test
    @DisplayName("DB 유니크 제약 위반(DataIntegrityViolationException)이 RECIPE_REMIX_ALREADY_EXISTS로 매핑된다")
    void remix_whenDbUniqueConstraintViolated_mapsToConflict() {
        // given — race condition: 선검사는 통과했지만 DB 인서트 직전 다른 트랜잭션이 먼저 커밋한 경우
        Recipe source = cloneableSource().build();
        given(recipeRepository.findById(SOURCE_RECIPE_ID)).willReturn(Optional.of(source));
        given(recipeRepository.existsByOriginRecipeIdAndUserId(SOURCE_RECIPE_ID, REMIX_USER_ID))
                .willReturn(false);
        willAnswer(inv -> { throw new DataIntegrityViolationException("uq_recipes_origin_user"); })
                .given(recipeRepository).save(any(Recipe.class));

        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        // when / then
        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(
                wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                        .isEqualTo(ErrorCode.RECIPE_REMIX_ALREADY_EXISTS));

        verify(s3Util, never()).copyObject(anyString(), anyString());
    }

    // helpers

    private void assertRemixRejected(Recipe source, ErrorCode expected) {
        RecipeCreateRequestDto dto = baseDto();
        dto.setOriginRecipeId(SOURCE_RECIPE_ID);

        assertThatThrownBy(() -> recipeService.createRecipeAndGenerateUrls(
                wrap(dto), REMIX_USER_ID, RecipeSourceType.USER, null))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(expected));

        verify(recipeRepository, never()).save(any(Recipe.class));
        verify(s3Util, never()).copyObject(anyString(), anyString());
    }

    private Recipe.RecipeBuilder cloneableSource() {
        return Recipe.builder()
                .id(SOURCE_RECIPE_ID)
                .user(officialUser)
                .title("공식 유튜브 원본")
                .dishType(DishType.FRYING)
                .source(RecipeSourceType.YOUTUBE)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .isPrivate(false)
                .imageKey(SOURCE_IMAGE_KEY)
                .originRecipe(null);
    }

    private RecipeCreateRequestDto baseDto() {
        return RecipeCreateRequestDto.builder()
                .title("내 입맛대로 고친 레시피")
                .dishType("볶음")
                .isPrivate(false)
                .ingredients(Collections.emptyList())
                .steps(Collections.emptyList())
                .tags(Collections.emptyList())
                .build();
    }

    private RecipeWithImageUploadRequest wrap(RecipeCreateRequestDto dto) {
        return RecipeWithImageUploadRequest.builder()
                .recipe(dto)
                .files(noFiles())
                .build();
    }

    private List<FileInfoRequest> noFiles() {
        return Collections.emptyList();
    }

    private User userWithId(Long id, String nickname) {
        User u = User.builder().nickname(nickname).build();
        try {
            Field idField = User.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(u, id);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
        return u;
    }
}
