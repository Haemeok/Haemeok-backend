package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.entity.Tag;
import com.jdc.recipe_service.domain.repository.RecipeTagRepository;
import com.jdc.recipe_service.domain.repository.TagRepository;
import com.jdc.recipe_service.mapper.RecipeTagMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeTagService {
    private final TagRepository tagRepository;
    private final RecipeTagRepository recipeTagRepository;

    public void saveAll(Recipe recipe, List<String> tagNames) {
        for (String tagName : tagNames) {
//            Tag tag = tagRepository.findByNameIgnoreCase(tagName)
//                    .orElseThrow(() -> new RuntimeException("태그가 존재하지 않습니다: " + tagName));
            Tag tag = tagRepository.findByNameIgnoreCase(tagName)
                    .orElseGet(() -> tagRepository.save(Tag.builder().name(tagName).build()));
            RecipeTag recipeTag = RecipeTagMapper.toEntity(recipe, tag);
            recipeTagRepository.save(recipeTag);
        }
    }

    public void updateTags(Recipe recipe, List<String> tagNames) {
        List<RecipeTag> existing = recipeTagRepository.findByRecipeId(recipe.getId());
        Map<String, RecipeTag> existingMap = existing.stream()
                .collect(Collectors.toMap(
                        t -> t.getTag().getName().toLowerCase().trim(),
                        Function.identity()
                ));

        Set<String> newNames = tagNames.stream().map(s -> s.toLowerCase().trim()).collect(Collectors.toSet());

        for (RecipeTag tag : existing) {
            if (!newNames.contains(tag.getTag().getName().toLowerCase().trim())) {
                recipeTagRepository.delete(tag);
            }
        }

        for (String name : newNames) {
            if (!existingMap.containsKey(name)) {
                Tag tag = tagRepository.findByNameIgnoreCase(name)
                        .orElseThrow(() -> new RuntimeException("태그가 존재하지 않습니다: " + name));
                RecipeTag entity = RecipeTagMapper.toEntity(recipe, tag);
                recipeTagRepository.save(entity);
            }
        }
    }
}