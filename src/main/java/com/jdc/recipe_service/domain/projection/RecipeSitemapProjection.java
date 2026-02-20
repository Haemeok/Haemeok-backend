package com.jdc.recipe_service.domain.projection;

import java.time.LocalDateTime;

public interface RecipeSitemapProjection {
    Long getId();
    LocalDateTime getUpdatedAt();
}
