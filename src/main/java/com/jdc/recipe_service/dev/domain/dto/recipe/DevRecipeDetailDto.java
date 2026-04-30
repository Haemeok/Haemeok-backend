package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Dev V3 레시피 상세 응답.
 *
 * 운영 V2 응답(`RecipeDetailStaticDto`)을 그대로 unwrap해서 모든 필드를 top-level에 노출 +
 * dev V3 신규 필드 추가:
 *  - youtubeInfo       : RecipeYoutubeInfo(분리 테이블) 데이터. 있으면 우선 사용, 없으면 base.youtube* fallback.
 *  - extractionInfo    : RecipeYoutubeExtractionInfo(추출 근거) — evidenceLevel, signals, usedGemini, tokenCost.
 *  - imageGenerationModel : Recipe.image_generation_model (gemini/gpt-image 식별자).
 *  - visibility, lifecycleStatus, listingStatus, source : Recipe entity 직접 노출 (V2 base에는 isPrivate만).
 *
 * 호환성: V2 응답 shape는 그대로 유지(unwrap), 신규 필드는 추가 — 프론트는 점진적 migration 가능.
 */
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DevRecipeDetailDto {

    /** V2 base 응답 — 모든 필드가 unwrap되어 top-level에 노출됨. */
    @JsonUnwrapped
    private RecipeDetailStaticDto base;

    @Schema(description = "이미지 생성 모델 식별자 (gemini-2.5-flash-image | gpt-image-2-low/medium/high). null이면 사용자 직접 업로드.")
    private String imageGenerationModel;

    @Schema(description = "공개 정책 — visibility/listingStatus/isPrivate 트리플의 source of truth.")
    private RecipeVisibility visibility;

    @Schema(description = "레시피 라이프사이클 상태 (ACTIVE/HIDDEN/BANNED/DELETED).")
    private RecipeLifecycleStatus lifecycleStatus;

    @Schema(description = "리스팅 상태 (LISTED/UNLISTED).")
    private RecipeListingStatus listingStatus;

    @Schema(description = "레시피 출처 (USER/AI/YOUTUBE/REELS).")
    private RecipeSourceType source;

    @Schema(description = "분리된 YouTube 메타정보. 존재 시 우선 사용 (legacy youtube_* 필드는 fallback).")
    private YoutubeInfoDto youtubeInfo;

    @Schema(description = "YouTube 추출 근거 정보 (signals + evidence + cost). YouTube 출처 + dev V3 추출만 존재.")
    private ExtractionInfoDto extractionInfo;

    @Schema(description = "재료 라인 정규화 기반 영양/원가 요약. base.totalCalories/totalIngredientCost는 V2 legacy 계산값 그대로 유지하고, 이 필드는 raw-first/MAPPED + per-g 기반의 새 계산 결과 + 4-state 카운트 + calculated/pending 분리를 노출한다.")
    private IngredientCalculationSummary ingredientCalculationSummary;

    /**
     * RecipeYoutubeInfo entity와 1:1 대응. 존재 시 frontend는 이 데이터를 우선 사용.
     */
    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class YoutubeInfoDto {
        @Schema(description = "YouTube video ID (11자 base64url)")
        private String videoId;

        @Schema(description = "원본 YouTube URL")
        private String youtubeUrl;

        @Schema(description = "채널명")
        private String channelName;

        @Schema(description = "채널 ID")
        private String channelId;

        @Schema(description = "영상 제목")
        private String videoTitle;

        @Schema(description = "썸네일 URL")
        private String thumbnailUrl;

        @Schema(description = "채널 프로필 URL")
        private String channelProfileUrl;

        @Schema(description = "구독자 수")
        private Long subscriberCount;

        @Schema(description = "조회수")
        private Long videoViewCount;

        @Schema(description = "영상 길이 (초)")
        private Long durationSeconds;

        @Schema(description = "추출자 user ID (HashID 인코딩)")
        @JsonSerialize(using = HashIdSerializer.class)
        private Long extractorId;
    }

    /**
     * 재료 라인 정규화 기반 영양/원가 요약.
     *
     * <p>의미: base.totalCalories/totalIngredientCost는 V2 legacy 라인 계산의 합으로, 단위/grams
     * 미해결 라인이 0으로 흘려 들어갈 수 있다. 이 객체는 dev 계산 정책으로 다시 계산하여
     * "포함된 라인의 합 + pending 카운트"를 명시 노출한다 (0 미반영을 사용자에게 드러낸다).
     */
    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class IngredientCalculationSummary {
        @Schema(description = "포함된 라인 기준 총 칼로리 (kcal)")
        private BigDecimal totalCalories;

        @Schema(description = "포함된 라인 기준 총 재료 원가 (원, integer rounding)")
        private long totalIngredientCost;

        @Schema(description = "포함된 라인 기준 총 탄수화물 (g)")
        private BigDecimal totalCarbohydrate;

        @Schema(description = "포함된 라인 기준 총 단백질 (g)")
        private BigDecimal totalProtein;

        @Schema(description = "포함된 라인 기준 총 지방 (g)")
        private BigDecimal totalFat;

        @Schema(description = "포함된 라인 기준 총 당류 (g)")
        private BigDecimal totalSugar;

        @Schema(description = "포함된 라인 기준 총 나트륨 (mg)")
        private BigDecimal totalSodium;

        @Schema(description = "MAPPED 라인 수 (ingredient + unit + grams 모두 확정)")
        private int mappedCount;

        @Schema(description = "PARTIAL 라인 수 (이름 매칭 + unit miss / C' bypass row 포함)")
        private int partialCount;

        @Schema(description = "UNRESOLVED 라인 수 (이름 매칭 실패)")
        private int unresolvedCount;

        @Schema(description = "CUSTOM 라인 수 (사용자 의도적 마스터 미매핑)")
        private int customCount;

        @Schema(description = "계산에 포함된 라인 수 (MAPPED + bypass via unit + custom 명시값)")
        private int calculatedCount;

        @Schema(description = "계산 보류 라인 수 — 0이 아닌 \"계산 불가\". UI에서 미반영 표시 권장.")
        private int pendingCount;
    }

    /**
     * RecipeYoutubeExtractionInfo entity와 1:1 대응. 추출 시 신호/근거/비용을 프론트에 노출.
     */
    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ExtractionInfoDto {
        @Schema(description = "자막이 충분히 있었는지")
        private boolean hasSubtitle;

        @Schema(description = "설명에 재료 키워드 신호가 있었는지")
        private boolean hasDescriptionIngredient;

        @Schema(description = "댓글에 재료 키워드 신호가 있었는지")
        private boolean hasCommentIngredient;

        @Schema(description = "Gemini 멀티모달 분석 fallback이 사용되었는지")
        private boolean usedGeminiAnalysis;

        @Schema(description = "추출 근거 신뢰도 — HIGH(자막 기반) | MEDIUM(설명/댓글 기반) | LOW(Gemini 분석)")
        private EvidenceLevel evidenceLevel;

        @Schema(description = "차감된 quota 토큰 (2 = 일반 추출, 5 = Gemini 분석 추출)")
        private int tokenCost;
    }
}
