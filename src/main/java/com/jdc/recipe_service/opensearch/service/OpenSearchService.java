package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import com.jdc.recipe_service.opensearch.keyword.KeywordService;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.index.query.RangeQueryBuilder;
import org.opensearch.search.SearchHits;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
public class OpenSearchService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RestHighLevelClient client;
    private final RefrigeratorItemRepository fridgeRepo;
    private final KeywordService keywordService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;


    public Page<RecipeSimpleDto> searchRecipes(
            RecipeSearchCondition cond,
            Pageable pg,
            Long uid) {

        try {
            BoolQueryBuilder bool = buildCommonQuery(cond);

            var src = new SearchSourceBuilder()
                    .query(bool)
                    .from((int) pg.getOffset())
                    .size(pg.getPageSize());
            pg.getSort().forEach(o ->
                    src.sort(o.getProperty(),
                            o.isAscending() ? SortOrder.ASC : SortOrder.DESC)
            );

            SearchResponse resp = client.search(
                    new SearchRequest("recipes").source(src),
                    RequestOptions.DEFAULT
            );
            SearchHits hits = resp.getHits();

            List<Long> ids = Arrays.stream(hits.getHits())
                    .map(h -> Long.valueOf(h.getId()))
                    .collect(Collectors.toList());
            if (ids.isEmpty()) {
                return new PageImpl<>(Collections.emptyList(), pg, 0);
            }

            List<RecipeSimpleDto> listWithCounts = recipeRepository.findAllSimpleDtoWithCountsByIdIn(ids);

            Map<Long, RecipeSimpleDto> dtoMap = listWithCounts.stream()
                    .collect(Collectors.toMap(RecipeSimpleDto::getId, Function.identity()));

            List<RecipeSimpleDto> list = ids.stream()
                    .filter(dtoMap::containsKey)
                    .map(dtoMap::get)
                    .collect(Collectors.toList());

            return new PageImpl<>(list, pg, hits.getTotalHits().value);

        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "OpenSearch 조회 실패: " + e.getMessage()
            );
        }
    }

    public Page<RecipeSimpleStaticDto> searchRecipesV2(RecipeSearchCondition cond, Pageable pg, Long uid) {
        try {
            BoolQueryBuilder bool = buildCommonQuery(cond);

            SearchSourceBuilder src = new SearchSourceBuilder().query(bool).from((int) pg.getOffset()).size(pg.getPageSize());
            pg.getSort().forEach(o -> src.sort(o.getProperty(), o.isAscending() ? SortOrder.ASC : SortOrder.DESC));

            SearchResponse resp = client.search(new SearchRequest("recipes").source(src), RequestOptions.DEFAULT);
            SearchHits hits = resp.getHits();

            List<Long> ids = Arrays.stream(hits.getHits()).map(h -> Long.valueOf(h.getId())).collect(Collectors.toList());
            if (ids.isEmpty()) {
                return Page.empty(pg);
            }

            List<RecipeSimpleStaticDto> dtos = recipeRepository.findAllSimpleStaticByIds(ids);

            Map<Long, RecipeSimpleStaticDto> dtoMap = dtos.stream()
                    .collect(Collectors.toMap(RecipeSimpleStaticDto::getId, Function.identity()));

            List<RecipeSimpleStaticDto> list = ids.stream()
                    .filter(dtoMap::containsKey)
                    .map(dtoMap::get)
                    .peek(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())))
                    .collect(Collectors.toList());

            return new PageImpl<>(list, pg, hits.getTotalHits().value);

        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, "OpenSearch 조회 실패: " + e.getMessage());
        }
    }

    private BoolQueryBuilder buildCommonQuery(RecipeSearchCondition cond) {
        BoolQueryBuilder bool = QueryBuilders.boolQuery();

        bool.filter(QueryBuilders.termQuery("isPrivate", false));

        if (cond.getTitle() != null && !cond.getTitle().isBlank()) {
            String title = cond.getTitle().trim();
            keywordService.record(title);
            var titleQuery = QueryBuilders.boolQuery()
                    .should(QueryBuilders.matchPhrasePrefixQuery("title.prefix", title))
                    .should(QueryBuilders.matchQuery("title.infix", title));
            bool.must(titleQuery);
        }

        applyRangeFilter(bool, "totalIngredientCost", cond.getMinCost(), cond.getMaxCost());
        applyRangeFilter(bool, "totalCalories", cond.getMinCalories(), cond.getMaxCalories());
        applyRangeFilter(bool, "protein", cond.getMinProtein(), cond.getMaxProtein());
        applyRangeFilter(bool, "carbohydrate", cond.getMinCarb(), cond.getMaxCarb());
        applyRangeFilter(bool, "fat", cond.getMinFat(), cond.getMaxFat());
        applyRangeFilter(bool, "sugar", cond.getMinSugar(), cond.getMaxSugar());
        applyRangeFilter(bool, "sodium", cond.getMinSodium(), cond.getMaxSodium());

        if (cond.getDishType() != null && !cond.getDishType().isBlank()) {
            bool.filter(QueryBuilders.termQuery("dishType", cond.getDishType()));
        }

        if (cond.getTags() != null && !cond.getTags().isEmpty()) {
            for (String tag : cond.getTags()) {
                bool.filter(QueryBuilders.termQuery("tags", tag));
            }
        }

        AiRecipeFilter filter = cond.getAiFilter();
        if (filter == AiRecipeFilter.USER_ONLY) {
            bool.filter(QueryBuilders.termQuery("isAiGenerated", false));
        } else if (filter == AiRecipeFilter.AI_ONLY) {
            bool.filter(QueryBuilders.termQuery("isAiGenerated", true));
        }

        if (cond.getIsYoutubeRecipe() != null) {
            if (cond.getIsYoutubeRecipe()) {
                BoolQueryBuilder youtubeFilter = QueryBuilders.boolQuery()
                        .must(QueryBuilders.existsQuery("youtubeUrl"))
                        .mustNot(QueryBuilders.termQuery("youtubeUrl", ""));
                bool.filter(youtubeFilter);
            } else {
                BoolQueryBuilder noYoutubeFilter = QueryBuilders.boolQuery()
                        .should(QueryBuilders.boolQuery().mustNot(QueryBuilders.existsQuery("youtubeUrl")))
                        .should(QueryBuilders.termQuery("youtubeUrl", ""));
                bool.filter(noYoutubeFilter);
            }
        }

        if (bool.must().isEmpty() && bool.filter().isEmpty()) {
            bool.must(QueryBuilders.matchAllQuery());
        }

        return bool;
    }

    private void applyRangeFilter(BoolQueryBuilder bool, String fieldName, Integer min, Integer max) {
        if (min == null && max == null) {
            return;
        }

        RangeQueryBuilder rangeQuery = QueryBuilders.rangeQuery(fieldName);

        if (min != null) {
            rangeQuery.gte(min);
        } else {
            rangeQuery.gte(0);
        }

        if (max != null) {
            rangeQuery.lte(max);
        }

        bool.filter(rangeQuery);
    }

    private String generateImageUrl(String key) {
        if (key == null || key.isBlank()) {
            return null;
        }

        if (key.startsWith("http")) {
            return key;
        }

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }
}
