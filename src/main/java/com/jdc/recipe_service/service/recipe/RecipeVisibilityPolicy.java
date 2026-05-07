package com.jdc.recipe_service.service.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;

/**
 * Recipe 생성 흐름의 visibility/listingStatus/isPrivate 단일 정규화 지점.
 *
 * <p>USER/AI/YOUTUBE/admin bulk 등 모든 create 진입점이 이 헬퍼만 호출하도록 강제 — 단일 setter
 * ({@code updateVisibility}/{@code updateListingStatus}/{@code updateIsPrivate}) 직접 호출은 트리플 invariant를
 * 깨뜨릴 수 있으므로 이 클래스 외에서 사용 금지.
 *
 * <p>정책 (V1.x):
 * <ul>
 *   <li>RESTRICTED 입력 → 즉시 거부 (ACL 기능 부재로 신규 사용 중지)</li>
 *   <li>PRIVATE 의도 (visibility=PRIVATE 또는 dto.isPrivate=true) → {@link Recipe#applyPrivate}</li>
 *   <li>PUBLIC + listingStatus=UNLISTED → {@link Recipe#applyPublicUnlisted} (link-only)</li>
 *   <li>그 외 PUBLIC 의도 → {@link Recipe#applyPublicListed} (검색/추천 노출)</li>
 * </ul>
 *
 * <p>리믹스(originRecipe != null)는 이 헬퍼 호출 후 {@link Recipe#applyPublicUnlisted}로 다시 강제되므로
 * 호출자가 별도로 처리해야 한다 — 이 헬퍼는 dto 입력만 본다.
 */
public final class RecipeVisibilityPolicy {

    private RecipeVisibilityPolicy() {}

    public static void applyFromDto(Recipe recipe, RecipeCreateRequestDto dto) {
        RecipeVisibility v = dto.getVisibility();
        if (v == RecipeVisibility.RESTRICTED) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "RESTRICTED는 더 이상 사용할 수 없는 visibility 값입니다. PUBLIC 또는 PRIVATE를 사용해주세요.");
        }
        // visibility와 isPrivate이 명시적으로 충돌하면 클라이언트 버그 신호 — 묵묵히 한쪽으로 정규화하면 디버깅이 어려워진다.
        // (visibility=PUBLIC + isPrivate=true) 또는 (visibility=PRIVATE + isPrivate=false) 두 케이스를 명시 거부.
        // visibility 미지정(null) 케이스는 정상 — dto.isPrivate으로 분기한다.
        if (v == RecipeVisibility.PUBLIC && Boolean.TRUE.equals(dto.getIsPrivate())) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "visibility=PUBLIC과 isPrivate=true는 동시에 지정할 수 없습니다.");
        }
        if (v == RecipeVisibility.PRIVATE && Boolean.FALSE.equals(dto.getIsPrivate())) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "visibility=PRIVATE과 isPrivate=false는 동시에 지정할 수 없습니다.");
        }
        if (v == RecipeVisibility.PRIVATE || Boolean.TRUE.equals(dto.getIsPrivate())) {
            recipe.applyPrivate();
            return;
        }
        // 여기 도달 시 PUBLIC 의도. listingStatus가 UNLISTED면 link-only, 그 외엔 LISTED 기본.
        if (dto.getListingStatus() == RecipeListingStatus.UNLISTED) {
            recipe.applyPublicUnlisted();
        } else {
            recipe.applyPublicListed();
        }
    }
}
