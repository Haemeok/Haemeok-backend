package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.RecipeTestService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/test/ai-recipe")
@RequiredArgsConstructor
public class AiTestController {

    private final RecipeTestService recipeService;

    @PostMapping
    public ResponseEntity<RecipeCreateRequestDto> generateTextRecipe(
            @RequestParam RobotType robotType,
            @RequestBody AiRecipeRequestDto aiRequest) {

        RecipeCreateRequestDto result = recipeService.generateRecipeTextOnly(
                robotType,
                aiRequest
        );
        return ResponseEntity.ok(result);
    }
}