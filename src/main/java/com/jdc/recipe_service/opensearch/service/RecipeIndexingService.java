package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import lombok.RequiredArgsConstructor;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.client.indices.CreateIndexResponse;
import org.opensearch.action.delete.DeleteRequest;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.update.UpdateRequest;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RecipeIndexingService {

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final RecipeLikeRepository likeRepository;
    private final RecipeRepository recipeRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    /**
     * 한 번만 호출해서 'recipes' 인덱스를 생성합니다.
     */
    public boolean createRecipeIndex() throws IOException {
        var request = new CreateIndexRequest("recipes");

        request.settings("""
        {
          "index": {
            "max_ngram_diff": 18
          },
          "analysis": {
            "filter": {
              "infix_ngram": {
                "type":     "ngram",
                "min_gram": 2,
                "max_gram": 20
              },
              "my_synonym": {
                "type":     "synonym",
                "synonyms": ["감자,포테이토", "김치,kimchi"]
              }
            },
            "tokenizer": {
              "edge_ngram_tokenizer": {
                "type":       "edge_ngram",
                "min_gram":   1,
                "max_gram":   20,
                "token_chars":["letter","digit"]
              }
            },
            "analyzer": {
              "autocomplete_analyzer": {
                "tokenizer": "edge_ngram_tokenizer",
                "filter":    ["lowercase","my_synonym"]
              },
              "infix_analyzer": {
                "tokenizer": "standard",
                "filter":    ["lowercase","infix_ngram"]
              }
            }
          }
        }
        """, XContentType.JSON);

        request.mapping("""
        {
          "properties": {
            "title": {
              "type": "text",
              "fields": {
                "prefix": {
                  "type":     "text",
                  "analyzer": "autocomplete_analyzer"
                },
                "infix": {
                  "type":            "text",
                  "analyzer":        "infix_analyzer",
                  "search_analyzer": "standard"
                }
              }
            },
            "description": { "type":"text" },
            "dishType":    { "type":"keyword" },
            "ingredients": { "type":"text", "analyzer":"autocomplete_analyzer" },
            "tags":        { "type":"keyword" },
            "createdAt":   { "type":"date" },
            "likeCount":   { "type":"integer" },
            "cookingTime": { "type":"integer" },
            "imageUrl":    { "type":"keyword" }
          }
        }
        """, XContentType.JSON);

        try {
            CreateIndexResponse res = client.indices().create(request, RequestOptions.DEFAULT);
            return res.isAcknowledged();
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "OpenSearch 인덱스 생성 실패: " + e.getMessage()
            );
        }
    }

    public void indexRecipe(Recipe recipe) {
        indexRecipe(recipe.getId());
    }

    public void updateRecipe(Recipe recipe) {
        updateRecipe(recipe.getId());
    }

    public void indexRecipe(Long recipeId) {
        Recipe recipe = recipeRepository
                .findWithAllRelationsById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        doIndex(recipe);
    }

    public void updateRecipe(Long recipeId) {
        Recipe recipe = recipeRepository
                .findWithAllRelationsById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        doUpdate(recipe);
    }

    /**
     * 레시피 삭제 시 색인도 삭제합니다.
     */
    public void deleteRecipe(Long recipeId) {
        try {
            DeleteRequest request = new DeleteRequest("recipes", recipeId.toString());
            client.delete(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "OpenSearch 색인 삭제 실패: " + e.getMessage()
            );
        }
    }

    /**
     * 문서에 들어갈 데이터를 Recipe → RecipeDocument 로 변환합니다.
     */
    private RecipeDocument buildDocument(Recipe recipe) {
        List<String> ingredientNames = Optional.ofNullable(recipe.getIngredients())
                .orElse(Collections.emptyList())
                .stream()
                .map(i -> {
                    if (i.getIngredient() != null && i.getIngredient().getName() != null) {
                        return i.getIngredient().getName();
                    }
                    return i.getCustomName();
                })
                .filter(name -> name != null && !name.isBlank())
                .map(String::trim)
                .distinct()
                .toList();

        List<String> tagNames = Optional.ofNullable(recipe.getTags())
                .orElse(Collections.emptySet())
                .stream()
                .map(t -> t.getTag().name())
                .filter(name -> name != null && !name.isBlank())
                .toList();

        int likeCount = likeRepository.countByRecipeId(recipe.getId());

        return RecipeDocument.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .description(recipe.getDescription())
                .ingredients(ingredientNames)
                .tags(tagNames)
                .dishType(recipe.getDishType().name())
                .createdAt(recipe.getCreatedAt().toString())
                .likeCount(likeCount)
                .cookingTime(Optional.ofNullable(recipe.getCookingTime()).orElse(0))
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .build();
    }

    private void doIndex(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            IndexRequest req = new IndexRequest("recipes")
                    .id(recipe.getId().toString())
                    .source(objectMapper.writeValueAsString(doc), XContentType.JSON);
            client.index(req, RequestOptions.DEFAULT);
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "레시피 색인 실패: " + e.getMessage()
            );        }
    }

    private void doUpdate(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            UpdateRequest req = new UpdateRequest("recipes", recipe.getId().toString())
                    .doc(objectMapper.writeValueAsString(doc), XContentType.JSON);
            client.update(req, RequestOptions.DEFAULT);
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "레시피 색인 업데이트 실패: " + e.getMessage()
            );
        }
    }

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }
}
