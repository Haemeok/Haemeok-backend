package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.config.DevVisibilityProperties;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeVisibilityService 단위 테스트.
 *
 * 검증 포인트:
 *  1. 모든 visibility 전환 (PUBLIC/PRIVATE/RESTRICTED) 시 트리플(visibility/listingStatus/isPrivate) 정확히 동기화
 *  2. RESTRICTED 활성화 (Phase A3 후) — applyVisibility(RESTRICTED) → listingStatus=UNLISTED, isPrivate=false
 *  3. 작성자 본인만 변경 가능 (RECIPE_ACCESS_DENIED)
 *  4. 레시피 없음 (RECIPE_NOT_FOUND)
 *  5. AI 생성 + PUBLIC + 이미지 없음 → CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE (V1 prod 정책 동등)
 *  6. AI 생성 + PRIVATE 전환 시에는 이미지 없어도 OK
 *  7. PRIVATE/PUBLIC/RESTRICTED 전환 시 OpenSearch 인덱스 갱신 hook 등록 (afterCommit)
 *  8. Phase 6 라이프사이클 가드 — DELETED/HIDDEN/BANNED 레시피의 visibility 변경 차단 (RECIPE_NOT_FOUND)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeVisibilityServiceTest {

    @Mock RecipeRepository recipeRepository;
    @Mock RecipeIndexingService recipeIndexingService;
    @Mock DevVisibilityProperties devVisibilityProperties;

    @InjectMocks DevRecipeVisibilityService service;

    private static final Long RECIPE_ID = 100L;
    private static final Long OWNER_ID = 1L;
    private static final Long OTHER_USER_ID = 99L;

    @BeforeEach
    void initTxSync() {
        // service가 TransactionSynchronizationManager.registerSynchronization 호출하므로 sync 활성화 필요
        // (실제 afterCommit은 commit 신호가 없어 안 fire — 본 테스트는 등록만 검증)
        TransactionSynchronizationManager.initSynchronization();

        // 대부분 테스트는 RESTRICTED 활성 상태에서 동작 (PUBLIC/PRIVATE 테스트는 flag 무관해서 lenient).
        // flag disabled 테스트만 별도로 false override.
        lenient().when(devVisibilityProperties.isRestrictedEnabled()).thenReturn(true);
    }

    @AfterEach
    void clearTxSync() {
        TransactionSynchronizationManager.clearSynchronization();
    }

    /** owner 본인 시나리오용 — owner.getId() stub을 가진 fixture. */
    private Recipe ownedRecipe(RecipeVisibility initial) {
        User owner = mock(User.class);
        given(owner.getId()).willReturn(OWNER_ID);
        return ownedRecipeWith(owner, initial);
    }

    /** 비로그인/비소유자 시나리오용 — owner.getId() stub 불필요한 fixture. (호출자 입장에서 needed only when isOwner reads id) */
    private Recipe recipeWithUnstubbedOwner(RecipeVisibility initial) {
        User owner = mock(User.class);
        return ownedRecipeWith(owner, initial);
    }

    private Recipe ownedRecipeWith(User owner, RecipeVisibility initial) {
        return Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(initial)
                .listingStatus(initial == RecipeVisibility.PUBLIC
                        ? RecipeListingStatus.LISTED : RecipeListingStatus.UNLISTED)
                .isPrivate(initial == RecipeVisibility.PRIVATE)
                .imageKey("images/recipes/1/100/main.webp")
                .build();
    }

    // --- 트리플 동기화 검증 ---

    @Test
    @DisplayName("PUBLIC → PRIVATE: visibility=PRIVATE, listingStatus=UNLISTED, isPrivate=true 트리플 동기화")
    void publicToPrivate_triplesynced() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID);

        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(response.listingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(response.isPrivate()).isTrue();
        // entity도 동기화 검증
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("PRIVATE → PUBLIC: visibility=PUBLIC, listingStatus=LISTED, isPrivate=false 트리플 동기화")
    void privateToPublic_triplesynced() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID);

        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(response.listingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(response.isPrivate()).isFalse();
    }

    @Test
    @DisplayName("PUBLIC → RESTRICTED 트리플 동기화: visibility=RESTRICTED, listingStatus=UNLISTED, isPrivate=false")
    void publicToRestricted_triplesynced() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.RESTRICTED, OWNER_ID);

        // applyVisibility(RESTRICTED) 트리플:
        //  visibility=RESTRICTED, listingStatus=UNLISTED, isPrivate=false
        // 정책: 검색/목록(A2/A3)은 listingStatus=LISTED 기준이라 RESTRICTED는 자연 차단되고,
        //       detail direct link 접근은 허용됨 (RESTRICTED는 link 공유 전용 — owner뿐 아니라 anonymous도 URL로 접근 가능).
        assertThat(response.visibility()).isEqualTo(RecipeVisibility.RESTRICTED);
        assertThat(response.listingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(response.isPrivate()).isFalse();

        // entity 자체도 트리플이 정확히 적용됐는지 확인 (applyVisibility 호출 결과)
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.RESTRICTED);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("RESTRICTED 요청 + flag disabled → INVALID_INPUT_VALUE + repository 호출 없음 (pre-DB guard)")
    void restricted_flagDisabled_throwsInvalidInputValue() {
        // flag disabled 환경 시뮬레이션 (운영 default)
        given(devVisibilityProperties.isRestrictedEnabled()).willReturn(false);

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.RESTRICTED, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        // pre-DB guard invariant: flag 체크가 가장 먼저라 repository까지 절대 안 감.
        // (회귀 시 — 누가 가드 위치를 옮겨서 Recipe 조회 후 차단하면 불필요한 DB hit 발생)
        verifyNoInteractions(recipeRepository);
        verifyNoInteractions(recipeIndexingService);
    }

    @Test
    @DisplayName("PRIVATE → RESTRICTED 전환: 트리플 정확히 갱신 (visibility=RESTRICTED, listingStatus=UNLISTED, isPrivate=false)")
    void privateToRestricted_triplesynced() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.RESTRICTED, OWNER_ID);

        // PRIVATE→RESTRICTED는 isPrivate true→false로 바뀜 (RESTRICTED는 owner 외엔 안 보이지만
        // legacy isPrivate 의미는 false — visibility/listingStatus 트리플이 진짜 의미를 표현)
        assertThat(response.visibility()).isEqualTo(RecipeVisibility.RESTRICTED);
        assertThat(response.listingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(response.isPrivate()).isFalse();
    }

    // --- 권한/존재 검증 ---

    @Test
    @DisplayName("작성자 아니면 RECIPE_ACCESS_DENIED")
    void nonOwner_throws() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OTHER_USER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
        // 변경 적용 안 됨
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
    }

    @Test
    @DisplayName("비로그인(currentUserId=null) → RECIPE_ACCESS_DENIED (isOwner의 null 가드로 즉시 false, owner.getId() 호출 안 됨)")
    void anonymous_throws() {
        // 비로그인 path는 owner.getId() 호출하지 않으므로 stub 없는 fixture 사용 (UnnecessaryStubbing 회피)
        Recipe recipe = recipeWithUnstubbedOwner(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    @Test
    @DisplayName("Recipe 없으면 RECIPE_NOT_FOUND")
    void recipeNotFound() {
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    // --- 도메인 가드: AI + PUBLIC + no image ---

    @Test
    @DisplayName("AI 생성 + 이미지 없음 + PUBLIC 전환 시도 → CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE (V1 정책 동등)")
    void aiGeneratedNoImage_toPublic_throws() {
        User owner = mock(User.class);
        given(owner.getId()).willReturn(OWNER_ID);
        Recipe recipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PRIVATE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .isPrivate(true)
                .isAiGenerated(true)
                .imageKey(null)  // 이미지 없음
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE);
        // 차단되어 visibility 안 바뀜
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
    }

    // --- OpenSearch indexing 갱신 검증 ---

    @Test
    @DisplayName("PRIVATE 전환 시 OpenSearch 인덱스 갱신 hook이 afterCommit에 등록되어 isPrivate=true 전파")
    void privateTransition_registersIndexingAfterCommit() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        service.updateVisibility(RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID);

        // afterCommit을 수동 트리거 (실제 tx commit이 없어 자동으로 안 fire되므로)
        TransactionSynchronizationManager.getSynchronizations()
                .forEach(org.springframework.transaction.support.TransactionSynchronization::afterCommit);

        verify(recipeIndexingService).updatePrivacyStatusSafely(RECIPE_ID, true);
    }

    @Test
    @DisplayName("PUBLIC 전환 시 OpenSearch 인덱스 갱신에 isPrivate=false 전파")
    void publicTransition_indexingWithIsPrivateFalse() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        service.updateVisibility(RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID);

        TransactionSynchronizationManager.getSynchronizations()
                .forEach(org.springframework.transaction.support.TransactionSynchronization::afterCommit);

        verify(recipeIndexingService).updatePrivacyStatusSafely(RECIPE_ID, false);
    }

    @Test
    @DisplayName("RESTRICTED 전환 시 OpenSearch 인덱스 갱신에 isPrivate=false 전파 (RESTRICTED도 isPrivate=false 매핑)")
    void restrictedTransition_indexingWithIsPrivateFalse() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        service.updateVisibility(RECIPE_ID, RecipeVisibility.RESTRICTED, OWNER_ID);

        TransactionSynchronizationManager.getSynchronizations()
                .forEach(org.springframework.transaction.support.TransactionSynchronization::afterCommit);

        // PUBLIC→RESTRICTED 전환 시 isPrivate=false 그대로지만 운영 partial update는 호출됨.
        // 운영 RecipeIndexingService.updatePrivacyStatusSafely 안에서 dev mirror reindex(A1 Batch 2)가
        // 이어 실행되어 dev alias의 visibility/listingStatus/lifecycleStatus 모두 갱신됨.
        verify(recipeIndexingService).updatePrivacyStatusSafely(RECIPE_ID, false);
    }

    // --- 라이프사이클 가드 (Phase 6 audit 산물) ---

    @Test
    @DisplayName("DELETED 레시피의 가시성 변경 → RECIPE_NOT_FOUND (admin 조치 우회 방지)")
    void deletedRecipe_visibilityUpdateBlocked() {
        User owner = mock(User.class);
        Recipe deletedRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .lifecycleStatus(RecipeLifecycleStatus.DELETED)  // 비활성
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(deletedRecipe));

        // owner.getId() stub 불필요 — lifecycle 가드가 ownership 검사 전에 차단
        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("BANNED 레시피의 가시성 변경 → RECIPE_NOT_FOUND")
    void bannedRecipe_visibilityUpdateBlocked() {
        User owner = mock(User.class);
        Recipe bannedRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .lifecycleStatus(RecipeLifecycleStatus.BANNED)
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(bannedRecipe));

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("HIDDEN 레시피의 가시성 변경 → RECIPE_NOT_FOUND")
    void hiddenRecipe_visibilityUpdateBlocked() {
        User owner = mock(User.class);
        Recipe hiddenRecipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .lifecycleStatus(RecipeLifecycleStatus.HIDDEN)
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(hiddenRecipe));

        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("AI 생성 + 이미지 없음 + PRIVATE 전환은 허용 (도메인 가드는 PUBLIC 전환에만 적용)")
    void aiGeneratedNoImage_toPrivateOk() {
        User owner = mock(User.class);
        given(owner.getId()).willReturn(OWNER_ID);
        Recipe recipe = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(RecipeVisibility.PUBLIC)  // PUBLIC에서 시작 (이미지 없는 상태로)
                .listingStatus(RecipeListingStatus.LISTED)
                .isPrivate(false)
                .isAiGenerated(true)
                .imageKey(null)
                .build();
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID);

        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(response.isPrivate()).isTrue();
    }
}
