    package com.jdc.recipe_service.domain.dto.recipe;

    import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
    import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
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
        private int cookingTime;
        private String imageUrl;
        private String youtubeUrl;
        private String cookingTools;
       // private boolean isAiGenerated;

        private Integer servings;
        private Integer totalIngredientCost;
        private Integer marketPrice;

        private List<RecipeIngredientRequestDto> ingredients;
        private List<RecipeStepRequestDto> steps;
        private List<String> tagNames;

    }
