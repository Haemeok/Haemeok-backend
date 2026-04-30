package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

/**
 * Dev V3 가시성 변경 요청.
 *
 * 허용 값: PUBLIC | PRIVATE | RESTRICTED.
 *  - PUBLIC: 일반 공개 (listingStatus=LISTED, isPrivate=false)
 *  - PRIVATE: 비공개 (listingStatus=UNLISTED, isPrivate=true)
 *  - RESTRICTED: 링크로만 접근 가능 (listingStatus=UNLISTED, isPrivate=false)
 *    → dev 검색/목록(A2/A3)은 listingStatus=LISTED만 노출하므로 RESTRICTED는 자연 차단되고
 *      direct URL 접근만 허용됨 (owner의 detail 조회는 분리된 정책으로 허용)
 */
public record DevVisibilityUpdateRequest(
        @Schema(description = "전환할 가시성. PUBLIC=공개+검색노출, PRIVATE=비공개, RESTRICTED=공개이지만 검색에서 빠짐(링크 공유 전용).",
                requiredMode = Schema.RequiredMode.REQUIRED,
                example = "PRIVATE",
                allowableValues = {"PUBLIC", "PRIVATE", "RESTRICTED"})
        @NotNull RecipeVisibility visibility
) {}
