package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;


/**
 * 내가 작성한 레시피 목록 조회용
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MyRecipeSummaryDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String title;
    private String imageUrl;
    private String dishType;
    private boolean isAiGenerated;
    private boolean isPrivate;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    private LocalDateTime createdAt;

    @Builder.Default
    private boolean likedByCurrentUser = false;
}
