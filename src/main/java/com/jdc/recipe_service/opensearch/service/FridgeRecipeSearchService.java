package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.FridgeRecipeDto;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class FridgeRecipeSearchService {

    private static final String INDEX = "recipes";

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final RefrigeratorItemRepository fridgeRepo;
    private final IngredientRepository ingredientRepo;
    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;

    public Page<FridgeRecipeDto> searchByFridge(Long userId, Pageable pageable) {
        List<Long> fridgeIds = fridgeRepo.findAllByUserId(userId).stream()
                .map(fi -> fi.getIngredient().getId())
                .collect(Collectors.toList());

        SearchSourceBuilder ssb = new SearchSourceBuilder()
                .from((int) pageable.getOffset())
                .size(pageable.getPageSize());

        if (fridgeIds.isEmpty()) {
            ssb.query(QueryBuilders.matchAllQuery());
        } else {
            String json = "{\"terms_set\":{\"ingredientIds\":{"
                    + "\"terms\":" + fridgeIds + ","
                    + "\"minimum_should_match_field\":\"ingredientCount\""
                    + "}}}";
            ssb.query(QueryBuilders.wrapperQuery(json));
        }

        pageable.getSort().forEach(order ->
                ssb.sort(order.getProperty(),
                        order.isAscending() ? SortOrder.ASC : SortOrder.DESC)
        );

        SearchResponse resp;
        try {
            resp = client.search(new SearchRequest(INDEX).source(ssb), RequestOptions.DEFAULT);
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.FRIDGE_RECIPE_SEARCH_ERROR,
                    "냉장고 기반 레시피 조회 실패: " + e.getMessage()
            );
        }

        List<RecipeDocument> docs = Arrays.stream(resp.getHits().getHits())
                .map(hit -> {
                    try {
                        return objectMapper.readValue(hit.getSourceAsString(), RecipeDocument.class);
                    } catch (IOException ex) {
                        throw new CustomException(
                                ErrorCode.FRIDGE_RECIPE_SEARCH_ERROR,
                                "검색 결과 문서 파싱 실패: " + ex.getMessage()
                        );
                    }
                })
                .collect(Collectors.toList());

        List<Long> recipeIds = docs.stream()
                .map(RecipeDocument::getId)
                .collect(Collectors.toList());

        if (recipeIds.isEmpty()) {
            return new PageImpl<>(Collections.emptyList(), pageable, 0);
        }

        Map<Long, Recipe> recipeMap = recipeRepository.findAllByIdInAndIsPrivateFalse(recipeIds).stream()
                .collect(Collectors.toMap(Recipe::getId, r -> r));

        Map<Long, Long> likeCountsMap = recipeRepository.findLikeCountsMapByIds(recipeIds);
        Map<Long, Double> avgRatingsMap = recipeRepository.findAvgRatingsByIdsRaw(recipeIds).stream()
                .collect(Collectors.toMap(a -> (Long) a[0], a -> (Double) a[1])); // findAvgRatingsByIdsRaw 사용 가정
        Map<Long, Long> ratingCountsMap = recipeRepository.findRatingCountsMapByIds(recipeIds);

        Set<Long> likedSet = recipeLikeRepository
                .findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        Set<Long> allMatchedIngredientIds = docs.stream()
                .flatMap(doc -> doc.getIngredientIds().stream())
                .filter(fridgeIds::contains)
                .collect(Collectors.toSet());

        Map<Long, String> ingredientNameMap = ingredientRepo.findAllById(allMatchedIngredientIds).stream()
                .collect(Collectors.toMap(Ingredient::getId, Ingredient::getName));


        List<FridgeRecipeDto> content = new ArrayList<>();
        for (RecipeDocument doc : docs) {
            Recipe r = recipeMap.get(doc.getId());
            if (r == null) continue;

            long likeCount = likeCountsMap.getOrDefault(doc.getId(), 0L);
            BigDecimal avgRating = BigDecimal.valueOf(avgRatingsMap.getOrDefault(doc.getId(), 0.0d));
            long ratingCount = ratingCountsMap.getOrDefault(doc.getId(), 0L);

            RecipeSimpleDto simple = new RecipeSimpleDto(
                    doc.getId(),
                    doc.getTitle(),
                    doc.getImageUrl(),
                    r.getUser().getId(),
                    r.getUser().getNickname(),
                    r.getUser().getProfileImage(),
                    LocalDateTime.parse(doc.getCreatedAt()),
                    likeCount,
                    likedSet.contains(doc.getId()),
                    doc.getCookingTime(),
                    avgRating,
                    ratingCount
            );

            List<String> matched = doc.getIngredientIds().stream()
                    .filter(fridgeIds::contains)
                    .map(ingredientNameMap::get)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            content.add(new FridgeRecipeDto(simple, matched));
        }
        long total = resp.getHits().getTotalHits().value;
        return new PageImpl<>(content, pageable, total);
    }
}
