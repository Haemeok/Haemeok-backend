package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
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
        CreateIndexRequest request = new CreateIndexRequest("recipes");
        request.settings("""
        {
          "analysis": {
            "filter": {
              "my_synonym": {
                "type": "synonym",
                "synonyms": ["감자,포테이토", "김치,kimchi"]
              }
            },
            "tokenizer": {
              "autocomplete_tokenizer": {
                "type": "edge_ngram",
                "min_gram": 1,
                "max_gram": 20,
                "token_chars": ["letter"]
              }
            },
            "analyzer": {
              "autocomplete_analyzer": {
                "tokenizer": "autocomplete_tokenizer",
                "filter": ["lowercase", "my_synonym"]
              }
            }
          }
        }
        """, XContentType.JSON);
        request.mapping("""
        {
          "properties": {
            "title":       { "type":"text", "analyzer":"autocomplete_analyzer" },
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

        CreateIndexResponse res = client.indices().create(request, RequestOptions.DEFAULT);
        log.info("✅ recipes 인덱스 생성 완료: acknowledged={}", res.isAcknowledged());
        return res.isAcknowledged();
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
                .orElseThrow(() -> new RuntimeException("레시피를 찾을 수 없습니다: " + recipeId));
        doIndex(recipe);
    }

    public void updateRecipe(Long recipeId) {
        Recipe recipe = recipeRepository
                .findWithAllRelationsById(recipeId)
                .orElseThrow(() -> new RuntimeException("레시피를 찾을 수 없습니다: " + recipeId));
        doUpdate(recipe);
    }

    /**
     * 레시피 삭제 시 색인도 삭제합니다.
     */
    public void deleteRecipe(Long recipeId) {
        try {
            DeleteRequest request = new DeleteRequest("recipes", recipeId.toString());
            client.delete(request, RequestOptions.DEFAULT);
            log.info("✅ 레시피 색인 삭제 완료: id={}", recipeId);
        } catch (IOException e) {
            log.error("OpenSearch 색인 삭제 실패: {}", e.getMessage(), e);
        }
    }

    /**
     * 문서에 들어갈 데이터를 Recipe → RecipeDocument 로 변환합니다.
     */
    private RecipeDocument buildDocument(Recipe recipe) {
        // 재료 이름
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
                .distinct()    // ← 중복 제거
                .toList();

        // 태그 이름
        List<String> tagNames = Optional.ofNullable(recipe.getTags())
                .orElse(Collections.emptySet())
                .stream()
                .map(t -> t.getTag().getDisplayName())
                .filter(name -> name != null && !name.isBlank())
                .toList();

        // 좋아요 개수는 DB에서 직접 조회
        int likeCount = likeRepository.countByRecipeId(recipe.getId());

        return RecipeDocument.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .description(recipe.getDescription())
                .ingredients(ingredientNames)
                .tags(tagNames)
                .dishType(recipe.getDishType().getDisplayName())
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
            log.info("✅ 레시피 색인 완료: id={}", recipe.getId());
        } catch (IOException e) {
            log.error("색인 실패: {}", e.getMessage(), e);
            throw new RuntimeException(e);
        }
    }

    private void doUpdate(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            UpdateRequest req = new UpdateRequest("recipes", recipe.getId().toString())
                    .doc(objectMapper.writeValueAsString(doc), XContentType.JSON);
            client.update(req, RequestOptions.DEFAULT);
            log.info("✅ 레시피 색인 업데이트 완료: id={}", recipe.getId());
        } catch (IOException e) {
            log.error("색인 업데이트 실패: {}", e.getMessage(), e);
            throw new RuntimeException(e);
        }
    }

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }
}
