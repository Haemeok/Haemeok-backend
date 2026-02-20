package com.jdc.recipe_service.service.sitemap;

import com.jdc.recipe_service.domain.dto.recipe.sitemap.RecipeSitemapResponseDto;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class RecipeSitemapService {

    private final RecipeRepository recipeRepository;
    private final Hashids hashids;

    @Transactional(readOnly = true)
    public List<RecipeSitemapResponseDto> getSitemapData() {
        return recipeRepository.findAllForSitemap().stream()
                .map(projection -> new RecipeSitemapResponseDto(
                        hashids.encode(projection.getId()),
                        projection.getUpdatedAt()
                ))
                .collect(Collectors.toList());
    }
}