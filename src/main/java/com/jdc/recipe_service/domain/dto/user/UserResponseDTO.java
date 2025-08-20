package com.jdc.recipe_service.domain.dto.user;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.jdc.recipe_service.domain.entity.User;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserResponseDTO {
    private Long id;
    private String nickname;
    private String profileImage;
    private String introduction;
    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime createdAt;
    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime updatedAt;

    private String provider;

    private boolean surveyCompleted;

    private int remainingAiQuota;

    public void updateAiQuota(int quota) {
        this.remainingAiQuota = quota;
    }
}
