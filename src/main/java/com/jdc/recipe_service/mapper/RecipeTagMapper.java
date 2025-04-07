package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.TagDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.type.TagType;

import java.util.List;

public class RecipeTagMapper {
    public static RecipeTag toEntity(Recipe recipe, String displayName) {
        TagType tagType = TagType.fromDisplayName(displayName);
        return RecipeTag.builder()
                .recipe(recipe)
                .tag(tagType)
                .build();
    }

    public static TagDto toDto(RecipeTag recipeTag) {
        return new TagDto(recipeTag.getTag().name(), recipeTag.getTag().getDisplayName());

    }

    public static List<TagDto> toDtoList(List<RecipeTag> recipeTags) {
        return recipeTags.stream().map(RecipeTagMapper::toDto).toList();
    }
}
