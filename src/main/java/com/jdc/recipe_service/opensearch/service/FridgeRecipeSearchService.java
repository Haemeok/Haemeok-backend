package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
import org.springframework.beans.factory.annotation.Value;
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

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    @Transactional(readOnly = true)
    public Page<FridgeRecipeDto> searchByFridge(Long userId,
                                                Pageable pageable,
                                                List<RecipeType> types) {

        List<Long> fridgeIds = fridgeRepo.findAllByUserId(userId).stream()
                .map(fi -> fi.getIngredient().getId())
                .toList();

        if (fridgeIds.isEmpty()) {
            return Page.empty(pageable);
        }

        try {
            return searchWithOpenSearch(userId, pageable, types, fridgeIds);
        } catch (Exception e) {
            log.warn("OpenSearch 냉장고 검색 실패, DB Fallback 실행. cause={}", e.getMessage());
            return searchWithDbFallback(userId, pageable, types, fridgeIds);
        }
    }

    @Transactional(readOnly = true)
    public Page<FridgeRecipeDto> searchByFridgeQueryOnly(
            Long userId,
            Pageable pageable,
            List<RecipeType> types
    ) {
        List<Long> fridgeIds = fridgeRepo.findAllByUserId(userId).stream()
                .map(fi -> fi.getIngredient().getId())
                .toList();

        return searchWithDbFallback(userId, pageable, types, fridgeIds);
    }

    private Page<FridgeRecipeDto> searchWithOpenSearch(Long userId,
                                                       Pageable pageable,
                                                       List<RecipeType> types,
                                                       List<Long> fridgeIds) throws IOException {

        SearchSourceBuilder ssb = new SearchSourceBuilder()
                .from((int) pageable.getOffset())
                .size(pageable.getPageSize());

        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();
        boolQuery.filter(QueryBuilders.termQuery("isPrivate", false));

        if (types != null && !types.isEmpty() && types.size() != RecipeType.values().length) {
            BoolQueryBuilder typeQuery = QueryBuilders.boolQuery();

            for (RecipeType type : types) {
                switch (type) {
                    case AI:
                        typeQuery.should(QueryBuilders.termQuery("isAiGenerated", true));
                        break;
                    case YOUTUBE:
                        BoolQueryBuilder youtubeCondition = QueryBuilders.boolQuery()
                                .must(QueryBuilders.existsQuery("youtubeUrl"))
                                .mustNot(QueryBuilders.termQuery("youtubeUrl", ""));
                        typeQuery.should(youtubeCondition);
                        break;
                    case USER:
                        BoolQueryBuilder userCondition = QueryBuilders.boolQuery()
                                .must(QueryBuilders.termQuery("isAiGenerated", false))
                                .must(QueryBuilders.boolQuery()
                                        .should(QueryBuilders.boolQuery().mustNot(QueryBuilders.existsQuery("youtubeUrl")))
                                        .should(QueryBuilders.termQuery("youtubeUrl", ""))
                                );
                        typeQuery.should(userCondition);
                        break;
                }
            }
            boolQuery.filter(typeQuery);
        }

        TermsSetQueryBuilder termsSetQuery =
                new TermsSetQueryBuilder("ingredientIds", fridgeIds)
                        .setMinimumShouldMatchField("ingredientCount");

        boolQuery.must(termsSetQuery);

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

        Set<Long> likedSet = recipeLikeRepository
                .findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        List<FridgeRecipeDto> content = new ArrayList<>();
        for (RecipeDocument doc : docs) {

            Recipe r = recipeMap.get(doc.getId());
            if (r == null) continue;

            BigDecimal avgRating = Optional.ofNullable(r.getAvgRating()).orElse(BigDecimal.ZERO);
            long ratingCount = Optional.ofNullable(r.getRatingCount()).orElse(0L);

            RecipeSimpleDto simple = new RecipeSimpleDto(
                    doc.getId(),
                    doc.getTitle(),
                    r.getImageKey() == null ? null : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, r.getImageKey()),
                    r.getUser().getId(),
                    r.getUser().getNickname(),
                    r.getUser().getProfileImage(),
                    LocalDateTime.parse(doc.getCreatedAt()),
                    r.getFavoriteCount(),
                    r.getLikeCount(),
                    likedSet.contains(doc.getId()),
                    doc.getCookingTime(),
                    r.getYoutubeChannelName(),
                    r.getYoutubeChannelId(),
                    r.getYoutubeVideoTitle(),
                    r.getYoutubeThumbnailUrl(),
                    r.getYoutubeChannelProfileUrl(),
                    r.getYoutubeSubscriberCount(),
                    r.getYoutubeVideoViewCount(),
                    avgRating,
                    ratingCount,
                    r.getYoutubeUrl(),
                    r.isAiGenerated()
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
                                                       List<RecipeType> types,
                                                       List<Long> fridgeIds) {

        if (fridgeIds.isEmpty()) return Page.empty(pageable);

        List<Long> effectiveFridgeIds = fridgeIds.stream()
                .filter(id -> !RecipeIndexingService.PANTRY_IDS.contains(id))
                .toList();

        if (effectiveFridgeIds.isEmpty()) {
            return Page.empty(pageable);
        }

        Page<Recipe> recipePage =
                recipeRepository.findByFridgeFallback(effectiveFridgeIds, types, pageable);

        List<Recipe> recipes = recipePage.getContent();
        if (recipes.isEmpty()) return Page.empty(pageable);

        List<Long> recipeIds = recipes.stream()
                .map(Recipe::getId)
                .toList();

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

            BigDecimal avgRating = Optional.ofNullable(r.getAvgRating()).orElse(BigDecimal.ZERO);
            long ratingCount = Optional.ofNullable(r.getRatingCount()).orElse(0L);

            RecipeSimpleDto simple = new RecipeSimpleDto(
                    r.getId(),
                    r.getTitle(),
                    r.getImageKey() == null ? null : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, r.getImageKey()),
                    r.getUser().getId(),
                    r.getUser().getNickname(),
                    r.getUser().getProfileImage(),
                    r.getCreatedAt(),
                    r.getFavoriteCount(),
                    r.getLikeCount(),
                    liked.contains(r.getId()),
                    Optional.ofNullable(r.getCookingTime()).orElse(0),
                    r.getYoutubeChannelName(),
                    r.getYoutubeChannelId(),
                    r.getYoutubeVideoTitle(),
                    r.getYoutubeThumbnailUrl(),
                    r.getYoutubeChannelProfileUrl(),
                    r.getYoutubeSubscriberCount(),
                    r.getYoutubeVideoViewCount(),
                    avgRating,
                    ratingCount,
                    r.getYoutubeUrl(),
                    r.isAiGenerated()
            );

            return new FridgeRecipeDto(simple, matched);
        }).toList();

        return new PageImpl<>(dtos, pageable, recipePage.getTotalElements());
    }
}
