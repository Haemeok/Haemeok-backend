package com.jdc.recipe_service.domain.dto.user;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserResponseDTO {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String nickname;
    private String profileImage;
    private String introduction;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    private LocalDateTime createdAt;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    private LocalDateTime updatedAt;

    private String provider;

    private boolean surveyCompleted;

    private boolean hasFirstRecord;

    private int remainingAiQuota;
    private int remainingYoutubeQuota;

    private int aiToken;
    private int youtubeToken;

    private int subscriptionCredit;
    private int cashCredit;

    public void updateQuotas(int aiQuota, int youtubeQuota) {
        this.remainingAiQuota = aiQuota;
        this.remainingYoutubeQuota = youtubeQuota;
    }

    public void updateTokens(int youtubeToken, int aiToken) {
        this.youtubeToken = youtubeToken;
        this.aiToken = aiToken;
    }

    public void updateCredits(int sub, int cash) {
        this.subscriptionCredit = sub;
        this.cashCredit = cash;
    }
}
