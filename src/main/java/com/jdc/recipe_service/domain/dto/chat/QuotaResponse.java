package com.jdc.recipe_service.domain.dto.chat;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "챗봇 일일 쿼터 상태")
public class QuotaResponse {

    @Schema(description = "유저당 일일 한도 (chat_config.daily_quota_per_user)", example = "20")
    private int dailyLimit;

    @Schema(description = "오늘 사용한 호출 수", example = "5")
    private int used;

    @Schema(description = "남은 호출 수", example = "15")
    private int remaining;

    @Schema(description = "쿼터 리셋 시각 (KST 다음날 00:00)", example = "2026-04-27T00:00:00")
    private LocalDateTime resetAt;
}
