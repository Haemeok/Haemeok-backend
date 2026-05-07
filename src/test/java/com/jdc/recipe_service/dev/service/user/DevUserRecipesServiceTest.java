package com.jdc.recipe_service.dev.service.user;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevMyRecipeSummaryDto;
import com.jdc.recipe_service.dev.repository.recipe.DevUserRecipesQueryRepository;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevUserRecipesService 단위 검증.
 *
 *  1. targetUser 존재 안 함 → USER_NOT_FOUND
 *  2. viewerId != null + recipe 있음 → like batch fetch + likedByCurrentUser 매핑
 *  3. viewerId == null → like repo 호출 안 함 (성능 최적화 invariant)
 *  4. 빈 결과 → like repo 호출 안 함
 *  5. DTO 4 enum 매핑 (visibility/listingStatus/lifecycleStatus/source)
 */
@ExtendWith(MockitoExtension.class)
class DevUserRecipesServiceTest {

    @Mock UserRepository userRepository;
    @Mock DevUserRecipesQueryRepository devUserRecipesQueryRepository;
    @Mock RecipeLikeRepository recipeLikeRepository;

    private DevUserRecipesService service;

    private static final Long TARGET_USER_ID = 100L;
    private static final Long VIEWER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        service = new DevUserRecipesService(userRepository, devUserRecipesQueryRepository, recipeLikeRepository);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    @Test
    @DisplayName("targetUser 존재 안 함 → USER_NOT_FOUND (repository 호출 전 가드)")
    void targetUserNotFound_throws() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> service.getUserRecipesDev(TARGET_USER_ID, VIEWER_ID, null, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.USER_NOT_FOUND);
    }

    @Test
    @DisplayName("viewerId 있음 + content 있음: like batch fetch + likedByCurrentUser 매핑")
    void withViewer_andContent_mapsLikedFlag() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.of(mockUser(TARGET_USER_ID)));
        Recipe r1 = recipeFor(101L, "r1");
        Recipe r2 = recipeFor(102L, "r2");
        given(devUserRecipesQueryRepository.findUserRecipesAccessible(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(new PageImpl<>(List.of(r1, r2), PAGE_10, 2));

        // viewer가 r1만 좋아요한 상태
        RecipeLike like = mock(RecipeLike.class);
        given(like.getRecipe()).willReturn(r1);
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(VIEWER_ID, List.of(101L, 102L)))
                .willReturn(List.of(like));

        Page<DevMyRecipeSummaryDto> result = service.getUserRecipesDev(TARGET_USER_ID, VIEWER_ID, null, PAGE_10);

        assertThat(result.getContent()).hasSize(2);
        assertThat(result.getContent().get(0).isLikedByCurrentUser()).isTrue();
        assertThat(result.getContent().get(1).isLikedByCurrentUser()).isFalse();
    }

    @Test
    @DisplayName("viewerId=null (anonymous): like repo 호출 안 함, likedByCurrentUser는 모두 false")
    void anonymous_skipsLikeQuery() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.of(mockUser(TARGET_USER_ID)));
        Recipe r1 = recipeFor(101L, "r1");
        given(devUserRecipesQueryRepository.findUserRecipesAccessible(eq(TARGET_USER_ID), eq(null), any(), eq(PAGE_10)))
                .willReturn(new PageImpl<>(List.of(r1), PAGE_10, 1));

        Page<DevMyRecipeSummaryDto> result = service.getUserRecipesDev(TARGET_USER_ID, null, null, PAGE_10);

        assertThat(result.getContent().get(0).isLikedByCurrentUser()).isFalse();
        verify(recipeLikeRepository, never()).findByUserIdAndRecipeIdIn(any(), anyList());
    }

    @Test
    @DisplayName("빈 결과: like repo 호출 안 함 (불필요 query 차단)")
    void emptyResult_skipsLikeQuery() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.of(mockUser(TARGET_USER_ID)));
        given(devUserRecipesQueryRepository.findUserRecipesAccessible(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(new PageImpl<>(List.of(), PAGE_10, 0));

        Page<DevMyRecipeSummaryDto> result = service.getUserRecipesDev(TARGET_USER_ID, VIEWER_ID, null, PAGE_10);

        assertThat(result.getContent()).isEmpty();
        verify(recipeLikeRepository, never()).findByUserIdAndRecipeIdIn(any(), anyList());
    }

    @Test
    @DisplayName("DTO 4 enum 매핑: visibility/listingStatus/lifecycleStatus/source 모두 String name으로 + isAiGenerated boolean 매핑")
    void dtoMaps4EnumFields() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.of(mockUser(TARGET_USER_ID)));
        // source=AI + isAiGenerated=true 조합 — 의미상 정합 (AI source면 보통 isAiGenerated도 true).
        Recipe restricted = Recipe.builder()
                .user(mockUser(TARGET_USER_ID))
                .title("restricted ai recipe")
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.RESTRICTED)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.AI)
                .isAiGenerated(true)
                .imageKey("ai.webp")  // AI는 imageKey 있어야 service 변환 정상 (service 자체는 통과시키지만 의미상)
                .imageStatus(RecipeImageStatus.FAILED)
                .isPrivate(false)
                .build();
        ReflectionTestUtils.setField(restricted, "id", 999L);
        given(devUserRecipesQueryRepository.findUserRecipesAccessible(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(new PageImpl<>(List.of(restricted), PAGE_10, 1));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(VIEWER_ID, List.of(999L)))
                .willReturn(List.of());

        Page<DevMyRecipeSummaryDto> result = service.getUserRecipesDev(TARGET_USER_ID, VIEWER_ID, null, PAGE_10);

        DevMyRecipeSummaryDto dto = result.getContent().get(0);
        assertThat(dto.getVisibility()).isEqualTo("RESTRICTED");
        assertThat(dto.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(dto.getSource()).isEqualTo("AI");
        assertThat(dto.getImageStatus()).isEqualTo("FAILED");
        assertThat(dto.getId()).isEqualTo(999L);
        assertThat(dto.isAiGenerated()).isTrue();
        assertThat(dto.isRemix()).isFalse();
    }

    @Test
    @DisplayName("[remix] originRecipe가 채워진 레시피 → DTO.isRemix=true (/me/recipes 와 /users/{userId}/recipes 모두 동일 매핑)")
    void remix_recipeMapsIsRemixTrue() {
        given(userRepository.findById(TARGET_USER_ID)).willReturn(Optional.of(mockUser(TARGET_USER_ID)));
        Recipe origin = recipeFor(500L, "origin");
        Recipe remix = Recipe.builder()
                .user(mockUser(TARGET_USER_ID))
                .title("remix-of-500")
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.UNLISTED)  // link-only — remix 분기 UI 핵심 케이스
                .source(RecipeSourceType.USER)
                .build();
        ReflectionTestUtils.setField(remix, "id", 501L);
        ReflectionTestUtils.setField(remix, "originRecipe", origin);  // ManyToOne 직접 세팅 — entity update 메서드 회피
        given(devUserRecipesQueryRepository.findUserRecipesAccessible(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(new PageImpl<>(List.of(remix), PAGE_10, 1));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(VIEWER_ID, List.of(501L)))
                .willReturn(List.of());

        Page<DevMyRecipeSummaryDto> result = service.getUserRecipesDev(TARGET_USER_ID, VIEWER_ID, null, PAGE_10);

        DevMyRecipeSummaryDto dto = result.getContent().get(0);
        assertThat(dto.isRemix()).isTrue();
        assertThat(dto.getVisibility()).isEqualTo("PUBLIC");
    }

    // ---------- helpers ----------

    private User mockUser(Long id) {
        User user = User.builder().nickname("u-" + id).provider("test").oauthId("oid-" + id).build();
        ReflectionTestUtils.setField(user, "id", id);
        return user;
    }

    private Recipe recipeFor(Long id, String title) {
        Recipe recipe = Recipe.builder()
                .user(mockUser(TARGET_USER_ID))
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)
                .build();
        ReflectionTestUtils.setField(recipe, "id", id);
        return recipe;
    }
}
