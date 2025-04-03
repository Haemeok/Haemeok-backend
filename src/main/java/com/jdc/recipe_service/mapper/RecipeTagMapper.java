package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.TagDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.entity.Tag;

import java.util.List;

public class RecipeTagMapper {

    public static RecipeTag toEntity(Recipe recipe, Tag tag) {
        return RecipeTag.builder()
                .recipe(recipe)
                .tag(tag)
                .build();
    }

    public static TagDto toDto(RecipeTag recipeTag) {
        Tag tag = recipeTag.getTag();
        return new TagDto(tag.getId(), tag.getName());
    }

    public static List<TagDto> toDtoList(List<RecipeTag> recipeTags) {
        return recipeTags.stream().map(RecipeTagMapper::toDto).toList();
    }
}
