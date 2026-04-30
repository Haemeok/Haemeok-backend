package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Dev V3 가시성 변경 응답 — 적용 후 트리플 상태 노출.
 * 프론트는 세 필드 모두 일관되게 갱신되었는지 확인 가능 (visibility 트리플 동기화 검증).
 *
 * 트리플 매핑:
 *  - PUBLIC     → listingStatus=LISTED,   isPrivate=false
 *  - PRIVATE    → listingStatus=UNLISTED, isPrivate=true
 *  - RESTRICTED → listingStatus=UNLISTED, isPrivate=false
 */
public record DevVisibilityUpdateResponse(
        @Schema(description = "갱신된 visibility (source of truth)",
                allowableValues = {"PUBLIC", "PRIVATE", "RESTRICTED"})
        RecipeVisibility visibility,

        @Schema(description = "트리플 동기화된 listingStatus (PUBLIC → LISTED, PRIVATE/RESTRICTED → UNLISTED)")
        RecipeListingStatus listingStatus,

        @Schema(description = "트리플 동기화된 legacy isPrivate (PRIVATE → true, PUBLIC/RESTRICTED → false)")
        Boolean isPrivate
) {}
