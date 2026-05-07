package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/** Dev V3 user/me recipes 응답 DTO. */
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

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "source", example = "USER")
    private String source;

    @Schema(description = "이미지 생성 상태. owner 목록에서는 PENDING/FAILED도 내려올 수 있음", example = "READY")
    private String imageStatus;

    // 필드명을 'remix'로 둔 이유: Lombok @Getter의 isRemix() 메서드를 Jackson이 "remix"로 별도 등록해
    // JSON 키 두 개("isRemix" + "remix")가 나가는 것을 막기 위함. boolean 'remix'에 대한 getter는 그대로 isRemix().
    @Schema(description = "remix(클론) 레시피 여부", example = "true")
    @JsonProperty("isRemix")
    private boolean remix;
}
