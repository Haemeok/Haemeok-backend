package com.jdc.recipe_service.dev.controller.search;

import com.jdc.recipe_service.dev.service.ingredient.DevIngredientSearchService;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/api/dev/search")
@RequiredArgsConstructor
@Tag(name = "Dev V3 search API", description = "Dev V3 search mirrors with ingredient normalization semantics")
public class DevSearchController {

    private static final int MAX_PAGE_SIZE = 50;

    private final DevIngredientSearchService ingredientSearchService;

    @GetMapping("/ingredients")
    @Operation(
            summary = "Dev V3 ingredient search",
            description = """
                    Operational `/api/search/ingredients` mirror.
                    Response shape is unchanged, but `unit` is selected from `ingredient_units.is_default=true`
                    when available, then falls back to legacy `ingredients.unit`.
                    Frontend can use this `unit` as the initial recipe form unit and fetch `/api/ingredients/{id}/units`
                    when the user wants to choose a different unit.
                    """
    )
    public ResponseEntity<Page<IngredientSummaryDto>> searchIngredients(
            @Parameter(description = "Search keyword") @RequestParam(required = false) String q,
            @Parameter(description = "Ingredient category code") @RequestParam(required = false) String category,
            @Parameter(description = "Sort value, currently kept for operational parity") @RequestParam(required = false) String sort,
            @Parameter(description = "Page number") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "Page size, max 50") @RequestParam(defaultValue = "20") int size,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        int safeSize = Math.min(size, MAX_PAGE_SIZE);
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;

        String koCategory = null;
        if (category != null && !category.isBlank()) {
            try {
                koCategory = IngredientType.fromCode(category).getKor();
            } catch (IllegalArgumentException e) {
                log.debug("Invalid category code: {}", category);
            }
        }

        Pageable pageable = PageRequest.of(page, safeSize);
        Page<IngredientSummaryDto> result = ingredientSearchService.search(q, koCategory, userId, pageable);
        return ResponseEntity.ok(result);
    }
}
