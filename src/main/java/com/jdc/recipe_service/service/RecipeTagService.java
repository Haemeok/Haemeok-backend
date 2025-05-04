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

    /**
     * 새 레시피에 태그 저장
     */
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

    /**
     * 레시피 수정 시 태그 업데이트
     */
    public void updateTags(Recipe recipe, List<String> tagDisplayNames) {
        List<RecipeTag> existingTags = recipeTagRepository.findByRecipeId(recipe.getId());
        Set<TagType> newTagTypes = tagDisplayNames.stream()
                .map(TagType::fromDisplayName)
                .collect(Collectors.toSet());

        // 기존 태그 중 삭제할 것
        for (RecipeTag tag : existingTags) {
            if (!newTagTypes.contains(tag.getTag())) {
                recipeTagRepository.delete(tag);
            }
        }

        // 새로운 태그 중 추가할 것
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
