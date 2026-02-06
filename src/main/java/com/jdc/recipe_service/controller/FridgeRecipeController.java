package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.FridgeRecipeService;
import com.jdc.recipe_service.security.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/me/fridge/recipes")
@RequiredArgsConstructor
public class FridgeRecipeController {

    private final FridgeRecipeService service;

    @GetMapping
    public ResponseEntity<Slice<FridgeRecipeDto>> findByFridge(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @RequestParam(name = "types", required = false) List<RecipeType> types
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        List<RecipeType> filterTypes = (types == null || types.isEmpty())
                ? List.of(RecipeType.USER, RecipeType.YOUTUBE)
                : types;

        Long userId = userDetails.getUser().getId();

        Slice<FridgeRecipeDto> result =
                service.searchByFridge(userId, pageable, filterTypes);

        return ResponseEntity.ok(result);
    }
}
