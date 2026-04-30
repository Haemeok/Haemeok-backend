package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;

/**
 * DevRecipeAccessValidator 정책 매트릭스.
 *
 *  1. recipe 없음 → RECIPE_NOT_FOUND
 *  2. owner의 ACTIVE PUBLIC/PRIVATE/RESTRICTED → 통과
 *  3. non-owner의 ACTIVE PUBLIC+LISTED → 통과
 *  4. non-owner의 ACTIVE PRIVATE/RESTRICTED → RECIPE_PRIVATE_ACCESS_DENIED (403)
 *  5. non-ACTIVE는 owner도 차단 → RECIPE_NOT_FOUND (404, V2 detail 가드 정합)
 *  6. anonymous → PUBLIC+LISTED+ACTIVE만 통과
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeAccessValidatorTest {

    @Mock RecipeRepository recipeRepository;

    @InjectMocks DevRecipeAccessValidator validator;

    private static final Long RECIPE_ID = 100L;
    private static final Long OWNER_ID = 1L;
    private static final Long OTHER_USER_ID = 99L;
    private User owner;

    @BeforeEach
    void setUp() {
        owner = User.builder().nickname("owner").provider("test").oauthId("oid").build();
        ReflectionTestUtils.setField(owner, "id", OWNER_ID);
    }

    @Test
    @DisplayName("recipe 없음 → RECIPE_NOT_FOUND")
    void recipeNotFound_throws() {
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> validator.loadAndCheckInteractable(RECIPE_ID, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("owner ACTIVE PUBLIC → 통과 (entity 반환)")
    void ownerPublic_returnsRecipe() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        Recipe result = validator.loadAndCheckInteractable(RECIPE_ID, OWNER_ID);

        assertThat(result).isSameAs(recipe);
    }

    @Test
    @DisplayName("owner ACTIVE PRIVATE → 통과 (자기 비공개에 자기 상호작용 가능)")
    void ownerPrivate_returnsRecipe() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        Recipe result = validator.loadAndCheckInteractable(RECIPE_ID, OWNER_ID);

        assertThat(result).isSameAs(recipe);
    }

    @Test
    @DisplayName("owner ACTIVE RESTRICTED → 통과")
    void ownerRestricted_returnsRecipe() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        Recipe result = validator.loadAndCheckInteractable(RECIPE_ID, OWNER_ID);

        assertThat(result).isSameAs(recipe);
    }

    @Test
    @DisplayName("non-owner ACTIVE PUBLIC+LISTED → 통과")
    void nonOwnerPublicListed_returnsRecipe() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        Recipe result = validator.loadAndCheckInteractable(RECIPE_ID, OTHER_USER_ID);

        assertThat(result).isSameAs(recipe);
    }

    @Test
    @DisplayName("non-owner ACTIVE PRIVATE → RECIPE_PRIVATE_ACCESS_DENIED (403)")
    void nonOwnerPrivate_throwsAccessDenied() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> validator.loadAndCheckInteractable(RECIPE_ID, OTHER_USER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    @Test
    @DisplayName("non-owner ACTIVE RESTRICTED → RECIPE_PRIVATE_ACCESS_DENIED (운영보다 엄격 — 핵심 invariant)")
    void nonOwnerRestricted_throwsAccessDenied() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> validator.loadAndCheckInteractable(RECIPE_ID, OTHER_USER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    @Test
    @DisplayName("non-ACTIVE는 owner도 RECIPE_NOT_FOUND (admin 우회 방지, 존재하지 않는 것처럼)")
    void nonActive_evenOwner_throwsNotFound() {
        Recipe hidden = recipe(RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(hidden));

        assertThatThrownBy(() -> validator.loadAndCheckInteractable(RECIPE_ID, OWNER_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("anonymous (viewer=null) ACTIVE RESTRICTED → RECIPE_PRIVATE_ACCESS_DENIED")
    void anonymousRestricted_throwsAccessDenied() {
        Recipe recipe = recipe(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));

        assertThatThrownBy(() -> validator.loadAndCheckInteractable(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    private Recipe recipe(RecipeLifecycleStatus lifecycle, RecipeVisibility visibility, RecipeListingStatus listing) {
        Recipe r = Recipe.builder()
                .user(owner)
                .title("test")
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .build();
        ReflectionTestUtils.setField(r, "id", RECIPE_ID);
        return r;
    }
}
