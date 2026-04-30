package com.jdc.recipe_service.dev.opensearch.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Dev V3 OpenSearch 문서 모델.
 *
 * 운영 {@link com.jdc.recipe_service.opensearch.dto.RecipeDocument}의 모든 필드를 그대로 복제하여
 * A2/A3에서 title/tag/dishType/ingredient/range/type filter 같은 기존 검색 기능을 dev alias 위에서도
 * 동일하게 쓸 수 있게 한다.
 *
 * 추가 필드 (4-enum + ownership):
 *  - {@code visibility}      — PUBLIC / PRIVATE / RESTRICTED
 *  - {@code listingStatus}   — LISTED / UNLISTED
 *  - {@code lifecycleStatus} — ACTIVE / HIDDEN / BANNED / DELETED
 *  - {@code source}          — USER / AI / YOUTUBE / REELS
 *  - {@code userId}          — owner id (DevRecipeSearchFilters.accessibleByFilter용)
 *
 * 필드명은 {@link com.jdc.recipe_service.dev.opensearch.service.DevRecipeSearchFilters} 상수와 일치 유지.
 * 운영 RecipeDocument 매핑/직렬화에는 영향 없음 (별도 클래스).
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DevRecipeDocument {

    // === 운영 RecipeDocument와 동일 필드 (A2 검색 기능 유지) ===
    private Long id;
    private String title;
    private List<String> tags;
    private String dishType;
    private String createdAt;
    private int cookingTime;
    private String imageUrl;
    private String youtubeUrl;
    private Boolean isAiGenerated;
    private Boolean isPrivate;
    private List<Long> ingredientIds;
    private List<String> ingredientNames;
    private Integer ingredientCount;
    private String youtubeChannelName;

    private Integer totalIngredientCost;
    private Float totalCalories;
    private Float protein;
    private Float carbohydrate;
    private Float fat;
    private Float sugar;
    private Float sodium;

    // === Dev V3 신규 enum/ownership 필드 ===
    private String visibility;
    private String listingStatus;
    private String lifecycleStatus;
    private String source;
    private Long userId;
}
