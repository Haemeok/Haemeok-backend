package com.jdc.recipe_service.domain.dto.recipe.sitemap;

import java.time.LocalDateTime;

public record RecipeSitemapResponseDto(
        String id,
        LocalDateTime updatedAt
) {}
