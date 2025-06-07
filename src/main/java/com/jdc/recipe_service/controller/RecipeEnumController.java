package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.DishTypeDto;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "태그/디시타입 목록 검색 API", description = "태그와 디사타입의 전체 목록을 조회하는 API입니다.")
public class RecipeEnumController {

    @GetMapping("/tags")
    @Operation(summary = "태그 목록 조회", description = "태그 목록을 조회합니다.")
    public List<Map<String, String>> getAllTags() {
        return Arrays.stream(TagType.values())
                .map(tag -> Map.of(
                        "code", tag.name(),
                        "name", tag.getDisplayName()
                ))
                .collect(Collectors.toList());
    }

    @GetMapping("/dish-types")
    @Operation(summary = "디사타입 목록 조회", description = "디사타입 목록을 조회합니다.")
    public ResponseEntity<List<DishTypeDto>> getAllDishTypes() {
        var types = Arrays.stream(DishType.values())
                .map(t -> new DishTypeDto(t.name(), t.getDisplayName()))
                .toList();
        return ResponseEntity.ok(types);
    }
}
