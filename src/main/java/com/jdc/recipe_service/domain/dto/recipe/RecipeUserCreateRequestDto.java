    package com.jdc.recipe_service.domain.dto.recipe;

    import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
    import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
    import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepUserRequestDto;
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
    public class RecipeUserCreateRequestDto {
        private String title;
        private String description;
        private String dishType;
        private Integer cookingTime;
        private String imageKey; // 🔄 imageUrl → imageKey (S3 key 저장용)
        private List<String> stepImageKeys;
        private List<String> cookingTools;
        private Integer servings;

        private List<RecipeIngredientRequestDto> ingredients;
        private List<RecipeStepUserRequestDto> steps;
        private List<String> tagNames;

    }
