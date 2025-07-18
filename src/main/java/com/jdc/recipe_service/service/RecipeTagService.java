package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.repository.RecipeTagRepository;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeTagService {

    private final RecipeTagRepository recipeTagRepository;

    public void saveAll(Recipe recipe, List<String> tagDisplayNames) {
        if (tagDisplayNames == null || tagDisplayNames.isEmpty()) {
            return;
        }

        List<TagType> tagTypes = tagDisplayNames.stream()
                .map(TagType::fromDisplayName)
                .distinct()
                .toList();

        List<RecipeTag> recipeTags = tagTypes.stream()
                .map(tagType -> RecipeTag.builder()
                        .recipe(recipe)
                        .tag(tagType)
                        .build())
                .toList();

        recipeTagRepository.saveAll(recipeTags);
    }

    public void updateTags(Recipe recipe, List<String> tagDisplayNames) {
        if (tagDisplayNames == null) {
            tagDisplayNames = Collections.emptyList();
        }

        Set<TagType> newTagTypes = tagDisplayNames.stream()
                .map(TagType::fromDisplayName)
                .collect(Collectors.toSet());

        List<RecipeTag> existingTags = recipeTagRepository.findByRecipeId(recipe.getId());
        Set<TagType> existingTypes = existingTags.stream()
                .map(RecipeTag::getTag)
                .collect(Collectors.toSet());

        recipeTagRepository.deleteAll(
                existingTags.stream()
                        .filter(tag -> !newTagTypes.contains(tag.getTag()))
                        .toList()
        );

        List<RecipeTag> tagsToAdd = newTagTypes.stream()
                .filter(tag -> !existingTypes.contains(tag))
                .map(tag -> RecipeTag.builder()
                        .recipe(recipe)
                        .tag(tag)
                        .build())
                .toList();

        recipeTagRepository.saveAll(tagsToAdd);
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        recipeTagRepository.deleteByRecipeId(recipeId);
    }
}
