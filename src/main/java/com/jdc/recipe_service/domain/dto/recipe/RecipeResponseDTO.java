package com.jdc.recipe_service.domain.dto.recipe;

import lombok.Data;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class RecipeResponseDTO {
    private Long id;
    private Long userId;
    private String title;
    private String description;
    private String dishType;
    private Integer cookingTime;
    private BigDecimal avgRating;
    private String imageUrl;
    private String youtubeUrl;
    private List<String> cookingTools;
    private boolean isAiGenerated;

    // 작성자 정보
    private String nickname;
    private String profileImage;
    private String introduction;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
