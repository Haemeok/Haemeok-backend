package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Dev V3 user/me recipes 응답 DTO.
 *
 * 운영 {@link com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto} 필드를 그대로 + dev V3 4-enum
 * (visibility/listingStatus/lifecycleStatus/source) 추가 노출. RESTRICTED 활성화 후 프론트가 UI 분기할 때 사용.
 *
 * 운영 DTO는 zero touch — 별도 클래스.
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Dev V3 user/me 레시피 요약 DTO (운영 base + 4 enum)")
public class DevMyRecipeSummaryDto {

    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String title;
    private String imageUrl;
    private String dishType;
    private String type;
    private boolean isAiGenerated;
    private boolean isPrivate;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    private LocalDateTime createdAt;

    @Builder.Default
    private boolean likedByCurrentUser = false;

    // === Dev V3 신규 4-enum 노출 ===

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "listing 상태", example = "LISTED")
    private String listingStatus;

    @Schema(description = "lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "source", example = "USER")
    private String source;
}
