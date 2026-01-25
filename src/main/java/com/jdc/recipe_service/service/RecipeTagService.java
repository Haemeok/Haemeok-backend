package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.repository.RecipeTagRepository;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeTagService {

    private final RecipeTagRepository recipeTagRepository;

    public void saveAll(Recipe recipe, List<String> tagDisplayNames) {
        if (tagDisplayNames == null || tagDisplayNames.isEmpty()) {
            return;
        }

        List<RecipeTag> recipeTags = tagDisplayNames.stream()
                .map(this::safeConvertToTagType)
                .filter(Objects::nonNull)
                .distinct()
                .map(tagType -> RecipeTag.builder()
                        .recipe(recipe)
                        .tag(tagType)
                        .build())
                .toList();

        recipeTagRepository.saveAll(recipeTags);
    }

    @Transactional
    public void updateTags(Recipe recipe, List<String> tagDisplayNames) {
        if (tagDisplayNames == null) {
            tagDisplayNames = Collections.emptyList();
        }

        Set<TagType> newTagTypes = tagDisplayNames.stream()
                .map(this::safeConvertToTagType)
                .filter(Objects::nonNull)
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

    private TagType safeConvertToTagType(String displayName) {
        try {
            return TagType.fromDisplayName(displayName);
        } catch (IllegalArgumentException e) {
            log.warn("⚠️ AI가 생성한 유효하지 않은 태그 무시됨: [{}]", displayName);
            return null;
        }
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        recipeTagRepository.deleteByRecipeId(recipeId);
    }
}
