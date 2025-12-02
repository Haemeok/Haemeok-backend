package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import com.jdc.recipe_service.opensearch.dto.FridgeRecipeDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.FridgeRecipeSearchService;
import com.jdc.recipe_service.security.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/me/fridge/recipes")
@RequiredArgsConstructor
public class FridgeRecipeController {

    private final FridgeRecipeSearchService service;

    @GetMapping
    public ResponseEntity<Page<FridgeRecipeDto>> findByFridge(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @RequestParam(name = "aiFilter", defaultValue = "USER_ONLY")
            AiRecipeFilter aiFilter
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        Page<FridgeRecipeDto> result =
                service.searchByFridge(userId, pageable, aiFilter);
        return ResponseEntity.ok(result);
    }

    @GetMapping("/query")
    public ResponseEntity<Page<FridgeRecipeDto>> findByFridgeQueryOnly(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @RequestParam(name = "aiFilter", defaultValue = "USER_ONLY")
            AiRecipeFilter aiFilter
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        Page<FridgeRecipeDto> result =
                service.searchByFridgeQueryOnly(userId, pageable, aiFilter);
        return ResponseEntity.ok(result);
    }
}
