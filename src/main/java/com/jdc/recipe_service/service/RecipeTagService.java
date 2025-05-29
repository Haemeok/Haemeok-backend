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
        List<TagType> tagTypes = tagDisplayNames.stream()
                .distinct() // 중복 방지
                .map(TagType::fromDisplayName)
                .collect(Collectors.toList());

        List<RecipeTag> recipeTags = tagTypes.stream()
                .map(tagType -> RecipeTag.builder()
                        .recipe(recipe)
                        .tag(tagType)
                        .build())
                .toList();

        recipeTagRepository.saveAll(recipeTags);
    }

    public void updateTags(Recipe recipe, List<String> tagDisplayNames) {
        List<RecipeTag> existingTags = recipeTagRepository.findByRecipeId(recipe.getId());
        Set<TagType> newTagTypes = tagDisplayNames.stream()
                .map(TagType::fromDisplayName)
                .collect(Collectors.toSet());

        List<RecipeTag> tagsToRemove = existingTags.stream()
                .filter(tag -> !newTagTypes.contains(tag.getTag()))
                .toList();

        recipeTagRepository.deleteAll(tagsToRemove);

        Set<TagType> existingTypes = existingTags.stream()
                .map(RecipeTag::getTag)
                .collect(Collectors.toSet());

        for (TagType newType : newTagTypes) {
            if (!existingTypes.contains(newType)) {
                RecipeTag newTag = RecipeTag.builder()
                        .recipe(recipe)
                        .tag(newType)
                        .build();
                recipeTagRepository.save(newTag);
            }
        }
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        recipeTagRepository.deleteByRecipeId(recipeId);
    }
}
