package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

/**
 * Dev V3 가시성 변경 요청.
 *
 * <p>허용 값: PUBLIC | PRIVATE.
 * <ul>
 *   <li><b>PUBLIC</b>: 일반 원본은 LISTED(검색/추천 노출), 리믹스는 UNLISTED(link-only). 둘 다 isPrivate=false.</li>
 *   <li><b>PRIVATE</b>: 비공개 — 트리플 (visibility=PRIVATE, listingStatus=UNLISTED, isPrivate=true)</li>
 * </ul>
 *
 * <p><b>RESTRICTED는 신규 입력으로 더 이상 허용되지 않음</b> — ACL 기반 권한 제어용으로 도입됐으나 ACL 기능 미보유로 사용 중지.
 * 기존 DB row 디시리얼라이즈 호환을 위해 enum 값 자체는 보존되지만, 외부 입력으로 들어오면 INVALID_INPUT_VALUE로 거부된다.
 * "공개+검색에서 빠짐"은 이제 visibility=PUBLIC + 자동으로 listingStatus=UNLISTED 적용되는 리믹스 케이스로 충분.
 */
public record DevVisibilityUpdateRequest(
        @Schema(description = "전환할 가시성. PUBLIC=공개(원본은 검색 노출, 리믹스는 link-only), PRIVATE=비공개. RESTRICTED는 deprecated.",
                requiredMode = Schema.RequiredMode.REQUIRED,
                example = "PRIVATE",
                allowableValues = {"PUBLIC", "PRIVATE"})
        @NotNull RecipeVisibility visibility
) {}
