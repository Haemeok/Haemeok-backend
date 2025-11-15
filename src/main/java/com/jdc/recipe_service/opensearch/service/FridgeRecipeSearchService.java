package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import com.jdc.recipe_service.opensearch.dto.FridgeRecipeDto;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.index.query.TermsSetQueryBuilder;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class FridgeRecipeSearchService {

    private static final String INDEX = "recipes";

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final RefrigeratorItemRepository fridgeRepo;
    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;

    @Transactional(readOnly = true)
    public Page<FridgeRecipeDto> searchByFridge(Long userId,
                                                Pageable pageable,
                                                AiRecipeFilter aiFilter) {

        List<Long> fridgeIds = fridgeRepo.findAllByUserId(userId).stream()
                .map(fi -> fi.getIngredient().getId())
                .toList();

        try {
            return searchWithOpenSearch(userId, pageable, aiFilter, fridgeIds);
        } catch (Exception e) {
            log.warn("OpenSearch 냉장고 검색 실패, DB Fallback 실행. cause={}", e.getMessage());
            return searchWithDbFallback(userId, pageable, aiFilter, fridgeIds);
        }
    }

    @Transactional(readOnly = true)
    public Page<FridgeRecipeDto> searchByFridgeQueryOnly(
            Long userId,
            Pageable pageable,
            AiRecipeFilter aiFilter
    ) {
        List<Long> fridgeIds = fridgeRepo.findAllByUserId(userId).stream()
                .map(fi -> fi.getIngredient().getId())
                .toList();

        return searchWithDbFallback(userId, pageable, aiFilter, fridgeIds);
    }

    private Page<FridgeRecipeDto> searchWithOpenSearch(Long userId,
                                                       Pageable pageable,
                                                       AiRecipeFilter aiFilter,
                                                       List<Long> fridgeIds) throws IOException {

        SearchSourceBuilder ssb = new SearchSourceBuilder()
                .from((int) pageable.getOffset())
                .size(pageable.getPageSize());

        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();
        boolQuery.filter(QueryBuilders.termQuery("isPrivate", false));

        if (aiFilter == AiRecipeFilter.USER_ONLY) {
            boolQuery.filter(QueryBuilders.termQuery("isAiGenerated", false));
        } else if (aiFilter == AiRecipeFilter.AI_ONLY) {
            boolQuery.filter(QueryBuilders.termQuery("isAiGenerated", true));
        }

        if (fridgeIds.isEmpty()) {
            boolQuery.must(QueryBuilders.matchAllQuery());
        } else {
            TermsSetQueryBuilder termsSetQuery =
                    new TermsSetQueryBuilder("ingredientIds", fridgeIds)
                            .setMinimumShouldMatchField("ingredientCount");

            boolQuery.must(termsSetQuery);
        }

        ssb.query(boolQuery);

        pageable.getSort().forEach(order ->
                ssb.sort(order.getProperty(),
                        order.isAscending() ? SortOrder.ASC : SortOrder.DESC)
        );

        SearchResponse resp =
                client.search(new SearchRequest(INDEX).source(ssb), RequestOptions.DEFAULT);

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
                .toList();

        List<Long> recipeIds = docs.stream()
                .map(RecipeDocument::getId)
                .toList();

        if (recipeIds.isEmpty()) {
            return Page.empty(pageable);
        }

        Map<Long, Recipe> recipeMap = recipeRepository
                .findAllByIdInAndIsPrivateFalseFetchUser(recipeIds).stream()
                .collect(Collectors.toMap(Recipe::getId, r -> r));

        Map<Long, Long> likeCountsMap = recipeRepository.findLikeCountsMapByIds(recipeIds);
        Map<Long, Double> avgRatingsMap = recipeRepository.findAvgRatingsMapByIds(recipeIds);
        Map<Long, Long> ratingCountsMap = recipeRepository.findRatingCountsMapByIds(recipeIds);

        Set<Long> likedSet = recipeLikeRepository
                .findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

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

            List<String> matched = new ArrayList<>();
            List<Long> ids = doc.getIngredientIds();
            List<String> names = doc.getIngredientNames();

            if (ids != null && names != null && ids.size() == names.size()) {
                for (int i = 0; i < ids.size(); i++) {
                    if (fridgeIds.contains(ids.get(i))) {
                        matched.add(names.get(i));
                    }
                }
            }

            content.add(new FridgeRecipeDto(simple, matched));
        }

        long total = resp.getHits().getTotalHits().value;
        return new PageImpl<>(content, pageable, total);
    }

    private Page<FridgeRecipeDto> searchWithDbFallback(Long userId,
                                                       Pageable pageable,
                                                       AiRecipeFilter aiFilter,
                                                       List<Long> fridgeIds) {

        if (fridgeIds.isEmpty()) return Page.empty(pageable);

        Page<Recipe> recipePage =
                recipeRepository.findByFridgeFallback(fridgeIds, aiFilter, pageable);

        List<Recipe> recipes = recipePage.getContent();
        if (recipes.isEmpty()) return Page.empty(pageable);

        List<Long> recipeIds = recipes.stream()
                .map(Recipe::getId)
                .toList();

        Map<Long, Long> likeCounts = recipeRepository.findLikeCountsMapByIds(recipeIds);
        Map<Long, Double> avgRatings = recipeRepository.findAvgRatingsMapByIds(recipeIds);
        Map<Long, Long> ratingCounts = recipeRepository.findRatingCountsMapByIds(recipeIds);

        Set<Long> liked = recipeLikeRepository
                .findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(l -> l.getRecipe().getId())
                .collect(Collectors.toSet());

        Set<Long> fridgeSet = new HashSet<>(fridgeIds);

        List<FridgeRecipeDto> dtos = recipes.stream().map(r -> {

            List<String> matched = r.getIngredients().stream()
                    .map(ri -> ri.getIngredient())
                    .filter(ing -> fridgeSet.contains(ing.getId()))
                    .filter(ing -> !RecipeIndexingService.PANTRY_IDS.contains(ing.getId()))
                    .map(Ingredient::getName)
                    .toList();

            RecipeSimpleDto simple = new RecipeSimpleDto(
                    r.getId(),
                    r.getTitle(),
                    r.getImageKey(),
                    r.getUser().getId(),
                    r.getUser().getNickname(),
                    r.getUser().getProfileImage(),
                    r.getCreatedAt(),
                    likeCounts.getOrDefault(r.getId(), 0L),
                    liked.contains(r.getId()),
                    Optional.ofNullable(r.getCookingTime()).orElse(0),
                    BigDecimal.valueOf(avgRatings.getOrDefault(r.getId(), 0.0)),
                    ratingCounts.getOrDefault(r.getId(), 0L)
            );

            return new FridgeRecipeDto(simple, matched);
        }).toList();

        return new PageImpl<>(dtos, pageable, recipePage.getTotalElements());
    }
}
