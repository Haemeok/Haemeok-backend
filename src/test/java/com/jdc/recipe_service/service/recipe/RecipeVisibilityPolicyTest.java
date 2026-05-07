package com.jdc.recipe_service.service.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * RecipeVisibilityPolicy.applyFromDto 단위 테스트.
 *
 * <p>RESTRICTED 차단 + dto 입력 → 의미별 helper(applyPrivate/applyPublicListed/applyPublicUnlisted) 분기 검증.
 * 분기 우선순위:
 * <ol>
 *   <li>RESTRICTED 입력 → 즉시 거부</li>
 *   <li>visibility=PRIVATE 또는 dto.isPrivate=true → applyPrivate</li>
 *   <li>listingStatus=UNLISTED → applyPublicUnlisted</li>
 *   <li>그 외 → applyPublicListed</li>
 * </ol>
 */
class RecipeVisibilityPolicyTest {

    private Recipe newRecipe() {
        // entity builder default — visibility=PUBLIC, listingStatus=LISTED, isPrivate=false (V1 단일 setter 시절 default).
        // 이 default에 의존하면 isPrivate=true 단독 set 시 PUBLIC+LISTED+isPrivate=true 깨진 row가 생기는 게 본 정책의 핵심 risk.
        return Recipe.builder().id(1L).build();
    }

    @Test
    @DisplayName("RESTRICTED 입력은 INVALID_INPUT_VALUE로 즉시 거부 — entity 상태 변경 없음")
    void rejectsRestricted() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.RESTRICTED);

        assertThatThrownBy(() -> RecipeVisibilityPolicy.applyFromDto(recipe, dto))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
    }

    @Test
    @DisplayName("visibility=PRIVATE → applyPrivate 트리플 (PRIVATE+UNLISTED+isPrivate=true)")
    void privateVisibility_appliesPrivate() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PRIVATE);

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("visibility 미지정 + isPrivate=true → applyPrivate (USER 단순 입력 케이스)")
    void isPrivateTrueOnly_appliesPrivate() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setIsPrivate(true);

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        // entity default(PUBLIC+LISTED)에 의존했다면 isPrivate=true이지만 visibility=PUBLIC, listingStatus=LISTED인 깨진 조합.
        // helper가 트리플로 정규화하는지 확인.
        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isTrue();
    }

    @Test
    @DisplayName("visibility=PUBLIC + listingStatus=UNLISTED → applyPublicUnlisted (link-only)")
    void publicUnlisted_appliesPublicUnlisted() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PUBLIC);
        dto.setListingStatus(RecipeListingStatus.UNLISTED);

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("visibility=PUBLIC + listingStatus=LISTED → applyPublicListed (검색/추천 노출)")
    void publicListed_appliesPublicListed() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PUBLIC);
        dto.setListingStatus(RecipeListingStatus.LISTED);

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("visibility/isPrivate/listingStatus 모두 미지정 → applyPublicListed (기본값)")
    void allUnspecified_defaultsToPublicListed() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
        assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
        assertThat(recipe.getIsPrivate()).isFalse();
    }

    @Test
    @DisplayName("visibility=PRIVATE + isPrivate=false 충돌 입력은 INVALID_INPUT_VALUE로 거부 (클라이언트 버그 명시)")
    void privateVsIsPrivateFalse_isRejected() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PRIVATE);
        dto.setIsPrivate(false);

        // 묵묵히 한쪽으로 정규화하면 클라이언트가 보낸 모순 입력이 디버깅 어려워짐 — 명시 거부가 더 안전.
        assertThatThrownBy(() -> RecipeVisibilityPolicy.applyFromDto(recipe, dto))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
    }

    @Test
    @DisplayName("visibility=PUBLIC + isPrivate=true 충돌 입력도 INVALID_INPUT_VALUE로 거부")
    void publicVsIsPrivateTrue_isRejected() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PUBLIC);
        dto.setIsPrivate(true);

        assertThatThrownBy(() -> RecipeVisibilityPolicy.applyFromDto(recipe, dto))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
    }

    @Test
    @DisplayName("visibility=PRIVATE + isPrivate=true (일관 입력) — 정상 적용")
    void privateAndIsPrivateTrueConsistent_appliesPrivate() {
        Recipe recipe = newRecipe();
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setVisibility(RecipeVisibility.PRIVATE);
        dto.setIsPrivate(true);

        RecipeVisibilityPolicy.applyFromDto(recipe, dto);

        assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
        assertThat(recipe.getIsPrivate()).isTrue();
    }
}
