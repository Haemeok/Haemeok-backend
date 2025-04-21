package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.DishTypeDto;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api")
public class RecipeEnumController {

    @GetMapping("/tags")
    public List<Map<String, String>> getAllTags() {
        return Arrays.stream(TagType.values())
                .map(tag -> Map.of(
                        "code", tag.name(),
                        "name", tag.getDisplayName()
                ))
                .collect(Collectors.toList());
    }

    @GetMapping("/dish-types")
    public ResponseEntity<List<DishTypeDto>> getAllDishTypes() {
        var types = Arrays.stream(DishType.values())
                .map(t -> new DishTypeDto(t.name(), t.getDisplayName()))
                .toList();
        return ResponseEntity.ok(types);
    }
}
