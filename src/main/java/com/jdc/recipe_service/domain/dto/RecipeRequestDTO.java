package com.jdc.recipe_service.domain.dto;


import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class RecipeRequestDTO {
    @NotNull
    private Long userId;

    @NotBlank
    private String title;

    private String description;
    private String dishType;
    private Integer cookingTime;
    private String imageUrl;
    private String youtubeUrl;
    private String cookingTools;

    //private boolean isAiGenerated;
}
