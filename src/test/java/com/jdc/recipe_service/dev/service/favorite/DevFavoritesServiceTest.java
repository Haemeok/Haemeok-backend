package com.jdc.recipe_service.dev.service.favorite;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleDto;
import com.jdc.recipe_service.dev.repository.favorite.DevFavoritesQueryRepository;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevFavoritesService 단위 검증.
 *
 *  1. dev repository 결과를 DTO로 변환 + likedByCurrentUser batch 매핑
 *  2. 빈 결과 → like repo 호출 안 함 (불필요 query 차단)
 *  3. favoriteByCurrentUser=true 항상 (즐겨찾기 목록이라)
 *  4. 4 enum 매핑 (visibility/listingStatus/lifecycleStatus/source)
 */
@ExtendWith(MockitoExtension.class)
class DevFavoritesServiceTest {

    @Mock DevFavoritesQueryRepository devFavoritesQueryRepository;
    @Mock RecipeLikeRepository recipeLikeRepository;

    private DevFavoritesService service;

    private static final Long USER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        service = new DevFavoritesService(devFavoritesQueryRepository, recipeLikeRepository);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    @Test
    @DisplayName("정상: dev repo 결과를 DTO로 변환 + liked batch 매핑")
    void happyPath_mapsLikedFlag() {
        Recipe r1 = recipeFor(101L, "r1", RecipeVisibility.PUBLIC);
        Recipe r2 = recipeFor(102L, "r2", RecipeVisibility.PUBLIC);
        given(devFavoritesQueryRepository.findFavoritesAccessible(USER_ID, PAGE_10))
                .willReturn(new PageImpl<>(List.of(r1, r2), PAGE_10, 2));

        // user가 r2만 like
        RecipeLike like = mock(RecipeLike.class);
        given(like.getRecipe()).willReturn(r2);
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L, 102L)))
                .willReturn(List.of(like));

        Page<DevRecipeSimpleDto> result = service.getMyFavoritesDev(USER_ID, PAGE_10);

        assertThat(result.getContent()).hasSize(2);
        assertThat(result.getContent().get(0).isLikedByCurrentUser()).isFalse();
        assertThat(result.getContent().get(1).isLikedByCurrentUser()).isTrue();
    }

    @Test
    @DisplayName("빈 결과: like repo 호출 안 함 (불필요 query 차단)")
    void emptyResult_skipsLikeQuery() {
        given(devFavoritesQueryRepository.findFavoritesAccessible(USER_ID, PAGE_10))
                .willReturn(new PageImpl<>(List.of(), PAGE_10, 0));

        Page<DevRecipeSimpleDto> result = service.getMyFavoritesDev(USER_ID, PAGE_10);

        assertThat(result.getContent()).isEmpty();
        verify(recipeLikeRepository, never()).findByUserIdAndRecipeIdIn(any(), anyList());
    }

    @Test
    @DisplayName("favoriteByCurrentUser=true 항상 (즐겨찾기 목록이라 — 운영 V2의 default false 누락 보강)")
    void favoriteByCurrentUser_alwaysTrue() {
        Recipe r1 = recipeFor(101L, "r1", RecipeVisibility.PUBLIC);
        given(devFavoritesQueryRepository.findFavoritesAccessible(USER_ID, PAGE_10))
                .willReturn(new PageImpl<>(List.of(r1), PAGE_10, 1));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(USER_ID), anyList()))
                .willReturn(List.of());

        Page<DevRecipeSimpleDto> result = service.getMyFavoritesDev(USER_ID, PAGE_10);

        assertThat(result.getContent().get(0).isFavoriteByCurrentUser()).isTrue();
    }

    @Test
    @DisplayName("4 enum 매핑: viewer 자기 RESTRICTED 시나리오 — visibility=RESTRICTED, listingStatus=UNLISTED, lifecycleStatus=ACTIVE, source=USER")
    void dtoMaps4EnumFields() {
        Recipe restricted = recipeFor(999L, "own-restricted", RecipeVisibility.RESTRICTED);
        // RESTRICTED는 listingStatus=UNLISTED, applyVisibility 시맨틱
        ReflectionTestUtils.setField(restricted, "listingStatus", RecipeListingStatus.UNLISTED);

        given(devFavoritesQueryRepository.findFavoritesAccessible(USER_ID, PAGE_10))
                .willReturn(new PageImpl<>(List.of(restricted), PAGE_10, 1));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(USER_ID), anyList()))
                .willReturn(List.of());

        Page<DevRecipeSimpleDto> result = service.getMyFavoritesDev(USER_ID, PAGE_10);

        DevRecipeSimpleDto dto = result.getContent().get(0);
        assertThat(dto.getVisibility()).isEqualTo("RESTRICTED");
        assertThat(dto.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(dto.getSource()).isEqualTo("USER");
        assertThat(dto.isRemix()).isFalse();
    }

    @Test
    @DisplayName("[remix] favorited 레시피가 PUBLIC+UNLISTED remix면 isRemix=true (link-only remix 보존이 핵심)")
    void favoritedRemix_mapsIsRemixTrue() {
        Recipe origin = recipeFor(700L, "origin", RecipeVisibility.PUBLIC);
        Recipe remix = recipeFor(701L, "remix-of-700", RecipeVisibility.PUBLIC);
        ReflectionTestUtils.setField(remix, "listingStatus", RecipeListingStatus.UNLISTED);
        ReflectionTestUtils.setField(remix, "originRecipe", origin);  // ManyToOne 직접 세팅

        given(devFavoritesQueryRepository.findFavoritesAccessible(USER_ID, PAGE_10))
                .willReturn(new PageImpl<>(List.of(remix), PAGE_10, 1));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(USER_ID), anyList()))
                .willReturn(List.of());

        Page<DevRecipeSimpleDto> result = service.getMyFavoritesDev(USER_ID, PAGE_10);

        DevRecipeSimpleDto dto = result.getContent().get(0);
        assertThat(dto.isRemix()).isTrue();
        assertThat(dto.getVisibility()).isEqualTo("PUBLIC");
    }

    // ---------- helpers ----------

    private User mockUser(Long id) {
        User user = User.builder().nickname("u-" + id).provider("test").oauthId("oid-" + id).build();
        ReflectionTestUtils.setField(user, "id", id);
        return user;
    }

    private Recipe recipeFor(Long id, String title, RecipeVisibility visibility) {
        Recipe recipe = Recipe.builder()
                .user(mockUser(USER_ID))
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(visibility)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)
                .build();
        ReflectionTestUtils.setField(recipe, "id", id);
        return recipe;
    }
}
