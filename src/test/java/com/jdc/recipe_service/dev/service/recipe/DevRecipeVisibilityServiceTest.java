package com.jdc.recipe_service.dev.service.recipe;

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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeVisibilityService 단위 테스트.
 *
 * <p>V1.x 정책 검증 포인트:
 * <ol>
 *   <li>PRIVATE 전환 → applyPrivate (visibility=PRIVATE/listingStatus=UNLISTED/isPrivate=true)</li>
 *   <li>PUBLIC + 일반 원본(origin 없음) → applyPublicListed (검색/추천 노출)</li>
 *   <li>PUBLIC + 리믹스(origin 있음) → applyPublicUnlisted (link-only, discovery 미노출)</li>
 *   <li>RESTRICTED 신규 입력 → INVALID_INPUT_VALUE (ACL 기능 부재로 거부)</li>
 *   <li>작성자 본인만 변경 가능 (RECIPE_ACCESS_DENIED)</li>
 *   <li>레시피 없음 (RECIPE_NOT_FOUND)</li>
 *   <li>AI 생성 + PUBLIC + 이미지 없음 → CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE</li>
 *   <li>AI 생성 + PRIVATE 전환 시에는 이미지 없어도 OK</li>
 *   <li>전환 시 OpenSearch 인덱스 갱신 hook 등록 (afterCommit)</li>
 *   <li>라이프사이클 가드 — DELETED/HIDDEN/BANNED 레시피의 visibility 변경 차단 (RECIPE_NOT_FOUND)</li>
 * </ol>
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeVisibilityServiceTest {

    @Mock RecipeRepository recipeRepository;
    @Mock RecipeIndexingService recipeIndexingService;

    @InjectMocks DevRecipeVisibilityService service;

    private static final Long RECIPE_ID = 100L;
    private static final Long OWNER_ID = 1L;
    private static final Long OTHER_USER_ID = 99L;

    @BeforeEach
    void initTxSync() {
        // service가 TransactionSynchronizationManager.registerSynchronization 호출하므로 sync 활성화 필요
        // (실제 afterCommit은 commit 신호가 없어 안 fire — 본 테스트는 등록만 검증)
        TransactionSynchronizationManager.initSynchronization();
    }

    @AfterEach
    void clearTxSync() {
        TransactionSynchronizationManager.clearSynchronization();
    }

    /** owner 본인 시나리오용 — owner.getId() stub을 가진 fixture (origin 없음 = 일반 원본). */
    private Recipe ownedRecipe(RecipeVisibility initial) {
        User owner = mock(User.class);
        given(owner.getId()).willReturn(OWNER_ID);
        return ownedRecipeWith(owner, initial, /* originRecipe */ null);
    }

    /** owner 본인 + origin이 있는 리믹스 fixture. */
    private Recipe ownedRemix(RecipeVisibility initial) {
        User owner = mock(User.class);
        given(owner.getId()).willReturn(OWNER_ID);
        Recipe origin = Recipe.builder().id(999L).build();
        return ownedRecipeWith(owner, initial, origin);
    }

    /** 비로그인/비소유자 시나리오용 — owner.getId() stub 불필요한 fixture. */
    private Recipe recipeWithUnstubbedOwner(RecipeVisibility initial) {
        User owner = mock(User.class);
        return ownedRecipeWith(owner, initial, null);
    }

    private Recipe ownedRecipeWith(User owner, RecipeVisibility initial, Recipe originRecipe) {
        Recipe.RecipeBuilder builder = Recipe.builder()
                .id(RECIPE_ID)
                .user(owner)
                .visibility(initial)
                .listingStatus(initial == RecipeVisibility.PUBLIC
                        ? RecipeListingStatus.LISTED : RecipeListingStatus.UNLISTED)
                .isPrivate(initial == RecipeVisibility.PRIVATE)
                .imageKey("images/recipes/1/100/main.webp");
        if (originRecipe != null) builder.originRecipe(originRecipe);
        return builder.build();
    }

    // --- 트리플 동기화 검증 ---

    @Test
    @DisplayName("PUBLIC → PRIVATE: applyPrivate 트리플 (visibility=PRIVATE, listingStatus=UNLISTED, isPrivate=true)")
    void publicToPrivate_appliesPrivate() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PUBLIC);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PRIVATE, OWNER_ID);

        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(response.isPrivate()).isTrue();
        // entity도 동기화 검증
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("일반 원본 PRIVATE → PUBLIC: applyPublicListed 트리플 (visibility=PUBLIC, listingStatus=LISTED, isPrivate=false)")
    void privateToPublic_originalRecipe_appliesPublicListed() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID);

        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(response.isPrivate()).isFalse();
        // 일반 원본은 entity의 listingStatus가 LISTED (검색 노출). 응답에는 listingStatus가 없어 entity로 검증.
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
    }

    @Test
    @DisplayName("리믹스 PRIVATE → PUBLIC: applyPublicUnlisted 트리플 (link-only — listingStatus=UNLISTED 보존)")
    void privateToPublic_remix_appliesPublicUnlisted() {
        Recipe recipe = ownedRemix(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        DevVisibilityUpdateResponse response = service.updateVisibility(
                RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID);

        // 리믹스는 PUBLIC이어도 검색/추천에 노출되면 안 되므로 entity가 UNLISTED를 유지해야 함.
        assertThat(response.visibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(response.isPrivate()).isFalse();
        // entity도 동일
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    // --- RESTRICTED 신규 입력 거부 ---

    @Test
    @DisplayName("RESTRICTED 요청 → INVALID_INPUT_VALUE + repository 호출 없음 (pre-DB guard)")
    void restricted_isRejected() {
        // RESTRICTED는 ACL 기능 부재로 신규 사용 중지. 외부 입력은 즉시 거부.
        assertThatThrownBy(() -> service.updateVisibility(
                RECIPE_ID, RecipeVisibility.RESTRICTED, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        // pre-DB guard invariant: 가드가 가장 먼저라 repository까지 절대 안 감.
        // 회귀 시 — 누가 가드 위치를 옮겨서 Recipe 조회 후 차단하면 불필요한 DB hit 발생.
        verifyNoInteractions(recipeRepository);
        verifyNoInteractions(recipeIndexingService);
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
    @DisplayName("PUBLIC 전환 시 OpenSearch 인덱스 갱신에 isPrivate=false 전파 (origin 유무 무관)")
    void publicTransition_indexingWithIsPrivateFalse() {
        Recipe recipe = ownedRecipe(RecipeVisibility.PRIVATE);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        service.updateVisibility(RECIPE_ID, RecipeVisibility.PUBLIC, OWNER_ID);

        TransactionSynchronizationManager.getSynchronizations()
                .forEach(org.springframework.transaction.support.TransactionSynchronization::afterCommit);

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
}
