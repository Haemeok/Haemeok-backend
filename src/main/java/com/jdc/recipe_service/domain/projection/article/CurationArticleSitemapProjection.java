package com.jdc.recipe_service.domain.projection.article;

import java.time.LocalDateTime;

public interface CurationArticleSitemapProjection {
    String getSlug();
    LocalDateTime getUpdatedAt();
}
