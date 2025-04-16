package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;

    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        int totalCost = 0;
        Set<Long> insertedIngredientIds = new HashSet<>();

        for (RecipeIngredientRequestDto dto : dtos) {
            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName().trim())
                    .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + dto.getName()));

            if (!insertedIngredientIds.add(ingredient.getId())) {
                continue; // 중복 방지
            }

            int quantity = Integer.parseInt(dto.getQuantity());
            int unitPrice = ingredient.getPrice() != null ? ingredient.getPrice() : 0;
            totalCost += quantity * unitPrice;

            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient);
            recipeIngredientRepository.save(entity);
        }

        return totalCost; // 총 원가 반환
    }

    public void updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos) {
        // 1. 기존 재료 삭제
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush(); // 💡 중복 insert 방지용 flush

        // 2. 새로 저장
        saveAll(recipe, ingredientDtos);
    }


//    public void saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
//        for (RecipeIngredientRequestDto dto : dtos) {
////            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName())
////                    .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + dto.getName()));
//            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName())
//                    .orElseGet(() -> {
//                        Ingredient newIngredient = Ingredient.builder()
//                                .name(dto.getName())
//                                .unit(dto.getUnit() != null ? dto.getUnit() : "개")
//                                .price(0)
//                                .build();
//                        return ingredientRepository.save(newIngredient); // ✅ 여기서 DB에 저장됨!
//                    });
//            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient);
//            recipeIngredientRepository.save(entity);
//
//        }
//    }

//    public void updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos) {
//        List<RecipeIngredient> existingIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
//        Map<String, RecipeIngredient> existingMap = existingIngredients.stream()
//                .collect(Collectors.toMap(
//                        ri -> ri.getIngredient().getName().toLowerCase(),
//                        Function.identity()
//                ));
//
//        Set<String> newNames = ingredientDtos.stream()
//                .map(i -> i.getName().toLowerCase())
//                .collect(Collectors.toSet());
//
//        List<RecipeIngredient> toDelete = existingIngredients.stream()
//                .filter(ri -> !newNames.contains(ri.getIngredient().getName().toLowerCase()))
//                .toList();
//        recipeIngredientRepository.deleteAll(toDelete);
//
//        List<RecipeIngredient> toSave = new ArrayList<>();
//        for (RecipeIngredientRequestDto dto : ingredientDtos) {
//            String key = dto.getName().toLowerCase();
//            RecipeIngredient existing = existingMap.get(key);
//
//            if (existing != null) {
//                if (!existing.getQuantity().equals(dto.getQuantity()) || !existing.getUnit().equals(dto.getUnit())) {
//                    existing.updateQuantityAndUnit(dto.getQuantity(), dto.getUnit());
//                    toSave.add(existing);
//                }
//            } else {
//                Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName())
//                        .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + dto.getName()));
//                RecipeIngredient newEntity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient);
//                toSave.add(newEntity);
//            }
//        }
//        recipeIngredientRepository.saveAll(toSave);
//    }

}