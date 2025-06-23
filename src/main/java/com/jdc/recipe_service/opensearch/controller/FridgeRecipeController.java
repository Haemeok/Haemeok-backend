package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.opensearch.dto.FridgeRecipeDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.FridgeRecipeSearchService;
import com.jdc.recipe_service.security.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
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
            @RequestParam(defaultValue = "0")  int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt,desc") String sort) {

        Long userId = userDetails.getUser().getId();

        String[] parts = sort.split(",");
        Sort.Direction dir = parts.length > 1 && parts[1].equalsIgnoreCase("asc")
                ? Sort.Direction.ASC
                : Sort.Direction.DESC;
        Sort sortObj = Sort.by(dir, parts[0]);
        Pageable pageable = PageRequest.of(page, size, sortObj);

        try {
            Page<FridgeRecipeDto> result = service.searchByFridge(userId, pageable);
            return ResponseEntity.ok(result);
        } catch (CustomException ce) {
            throw ce;
        } catch (Exception e) {
            throw new CustomException(
                    ErrorCode.FRIDGE_RECIPE_SEARCH_ERROR,
                    "냉장고 기반 레시피 조회 실패: " + e.getMessage()
            );
        }
    }
}
