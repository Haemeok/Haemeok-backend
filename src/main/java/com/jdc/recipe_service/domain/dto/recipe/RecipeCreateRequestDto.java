    package com.jdc.recipe_service.domain.dto.recipe;

    import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
    import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
    import jakarta.validation.constraints.NotBlank;
    import lombok.AllArgsConstructor;
    import lombok.Builder;
    import lombok.Getter;
    import lombok.NoArgsConstructor;

    import java.util.List;

    /**
     *
     *  레시피 생성용
     */

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public class RecipeCreateRequestDto {

        @NotBlank(message = "레시피 제목은 필수입니다.")
        private String title;

        private String description;

        @NotBlank(message = "요리 유형은 필수입니다.")
        private String dishType;

        private Integer cookingTime;
        private String imageKey; // 🔄 imageUrl → imageKey (S3 key 저장용)
        private List<String> stepImageKeys;
        private String youtubeUrl;
        private List<String> cookingTools;

        private Integer servings;
        private Integer totalIngredientCost;
        private Integer marketPrice;

        private List<RecipeIngredientRequestDto> ingredients;
        private List<RecipeStepRequestDto> steps;
        private List<String> tagNames;

    }
