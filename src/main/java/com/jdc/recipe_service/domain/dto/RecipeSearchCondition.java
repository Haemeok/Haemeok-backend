package com.jdc.recipe_service.domain.dto;

import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeSearchCondition {
    private String title;
    private String dishType;
    private List<String> tagNames;

    public DishType getDishTypeEnum() {
        if (dishType == null || dishType.isBlank()) return null;
        return DishType.fromCode(dishType);
    }

    public List<TagType> getTagEnums() {
        if (tagNames == null || tagNames.isEmpty()) return List.of();
        return tagNames.stream()
                .map(TagType::fromCode)
                .toList();
    }
}