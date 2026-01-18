package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import com.jdc.recipe_service.opensearch.indexingfailure.IndexingFailureLogService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.action.bulk.BulkRequest;
import org.opensearch.action.support.WriteRequest;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.client.indices.CreateIndexResponse;
import org.opensearch.action.delete.DeleteRequest;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.update.UpdateRequest;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeIndexingService {

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final RecipeLikeRepository likeRepository;
    private final RecipeRepository recipeRepository;
    private final IndexingFailureLogService failureLogService;

    private static final int MAX_RETRY_ATTEMPTS = 3;
    private static final long RETRY_DELAY_MS = 2000;

    public static final Set<Long> PANTRY_IDS = Set.of(
            18L, 25L, 26L, 27L, 28L, 31L, 35L, 42L, 43L, 51L,
            56L, 57L, 59L, 60L, 63L, 64L, 82L, 95L, 97L, 100L,
            113L, 116L, 118L, 119L, 123L, 124L, 129L, 131L,
            134L, 138L, 149L, 151L, 152L, 155L, 163L, 165L,
            179L, 190L, 202L, 208L, 212L, 213L, 217L, 227L,
            236L, 237L, 238L, 239L, 247L, 248L, 252L, 260L,
            268L, 270L, 271L, 275L, 277L, 283L, 293L, 302L,
            305L, 319L, 322L, 325L, 331L, 332L, 335L, 339L,
            342L, 344L, 346L, 353L, 354L, 355L, 356L, 360L,
            370L, 379L, 384L, 388L, 391L, 403L, 405L, 410L,
            416L, 420L, 423L, 426L, 428L, 444L, 447L, 463L,
            469L, 482L, 485L, 488L, 498L, 500L, 502L, 518L,
            519L, 520L, 529L, 545L, 547L, 549L, 551L, 555L,
            556L, 584L, 588L, 592L, 597L, 603L, 614L, 623L,
            629L, 642L, 644L, 652L, 662L, 673L, 676L, 684L,
            687L, 691L, 694L, 695L, 702L, 703L, 717L, 722L,
            735L, 736L, 740L, 746L, 752L, 755L, 759L, 760L,
            762L, 766L, 768L,
            148L, 45L
    );

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
                    "number_of_shards": 1,
                    "number_of_replicas": 0,
                    "refresh_interval": "1s",
                    "max_ngram_diff": 18
                  },
                  "analysis": {
                    "tokenizer": {
                      "nori_user_dict": {
                        "type": "nori_tokenizer",
                        "decompound_mode": "mixed"
                      },
                      "edge_ngram_tokenizer": {
                        "type": "edge_ngram",
                        "min_gram": 1,
                        "max_gram": 20,
                        "token_chars": ["letter", "digit"]
                      }
                    },
                    "analyzer": {
                      "korean_analyzer": { 
                        "type": "custom",
                        "tokenizer": "nori_user_dict",
                        "filter": ["lowercase", "my_synonym"] 
                      },
                      "autocomplete_analyzer": {
                        "tokenizer": "edge_ngram_tokenizer",
                        "filter": ["lowercase", "my_synonym"]
                      },
                      "infix_analyzer": {
                        "tokenizer": "standard",
                        "filter": ["lowercase", "infix_ngram"]
                      }
                    },
                    "filter": {
                      "infix_ngram": {
                        "type": "ngram",
                        "min_gram": 2,
                        "max_gram": 20
                      },
                      "my_synonym": {
                        "type": "synonym",
                        "synonyms": ["감자,포테이토", "김치,kimchi"]
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
                      "analyzer": "korean_analyzer",
                      "fields": {
                        "keyword": { 
                          "type": "keyword" 
                        },
                        "prefix": {
                          "type": "text",
                          "analyzer": "autocomplete_analyzer"
                        },
                        "infix": {
                          "type": "text",
                          "analyzer": "infix_analyzer",
                          "search_analyzer": "standard"
                        }
                      }
                    },
                    "youtubeChannelName": {
                      "type": "text",
                      "analyzer": "korean_analyzer",
                      "fields": {
                        "keyword": { "type": "keyword" }
                      }
                    },
                    "dishType": { "type": "keyword" },
                    "tags": { "type": "keyword" },
                    "createdAt": { "type": "date" },
                    "cookingTime": { "type": "integer" },
                    "imageUrl": { "type": "keyword" },
                    "youtubeUrl": { "type": "keyword" },
                    "isAiGenerated": { "type": "boolean" },
                    "isPrivate": { "type": "boolean" },
                    "ingredientIds": { "type": "long" },
                    "ingredientCount": { "type": "integer" },
                    "totalIngredientCost": { "type": "integer" },
                    "totalCalories": { "type": "float" },
                    "protein": { "type": "float" },
                    "carbohydrate": { "type": "float" },
                    "fat": { "type": "float" },
                    "sugar": { "type": "float" },
                    "sodium": { "type": "float" }
                  }
                }
                """, XContentType.JSON);

        try {
            CreateIndexResponse res = client.indices().create(request, RequestOptions.DEFAULT);
            return res.isAcknowledged();
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, "OpenSearch 인덱스 생성 실패: " + e.getMessage());
        }
    }

    public void indexRecipe(Recipe recipe) {
        indexRecipe(recipe.getId());
    }

    public void indexAllRecipes() {
        int page = 0;
        int size = 1000;

        while (true) {
            Page<Recipe> recipePage = recipeRepository.findAll(PageRequest.of(page, size));
            if (!recipePage.hasContent()) {
                break;
            }

            BulkRequest bulkRequest = new BulkRequest();

            for (Recipe recipe : recipePage.getContent()) {
                try {
                    RecipeDocument doc = buildDocument(recipe);

                    bulkRequest.add(new IndexRequest("recipes")
                            .id(recipe.getId().toString())
                            .source(objectMapper.writeValueAsString(doc), XContentType.JSON)
                    );
                } catch (Exception e) {
                    log.error("Bulk 색인 중 문서 변환 오류 ID: {}", recipe.getId(), e);
                }
            }
            try {
                if (bulkRequest.numberOfActions() > 0) {
                    client.bulk(bulkRequest, RequestOptions.DEFAULT);
                    log.info("레시피 Bulk 색인 진행 중: {}건 완료", (page * size) + bulkRequest.numberOfActions());
                }
            } catch (IOException e) {
                log.error("레시피 Bulk 색인 실패 (페이지: {})", page, e);
            }
            page++;
        }
        log.info("모든 레시피 Bulk 색인 완료.");
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

        var filteredIngredients = Optional.ofNullable(recipe.getIngredients())
                .orElse(List.of())
                .stream()
                .filter(ri -> ri.getIngredient() != null)
                .filter(ri -> !PANTRY_IDS.contains(ri.getIngredient().getId()))
                .toList();

        List<Long> ids = filteredIngredients.stream()
                .map(ri -> ri.getIngredient().getId())
                .toList();

        List<String> names = filteredIngredients.stream()
                .map(ri -> ri.getIngredient().getName())
                .toList();

        List<String> tags = Optional.ofNullable(recipe.getTags())
                .orElse(Collections.emptySet())
                .stream()
                .map(t -> t.getTag().name())
                .filter(name -> name != null && !name.isBlank())
                .toList();

        Integer cost = recipe.getTotalIngredientCost() != null ? recipe.getTotalIngredientCost() : 0;

        Float calories = recipe.getTotalCalories() != null ? recipe.getTotalCalories().floatValue() : 0.0f;
        Float protein = recipe.getProtein() != null ? recipe.getProtein().floatValue() : 0.0f;
        Float carb = recipe.getCarbohydrate() != null ? recipe.getCarbohydrate().floatValue() : 0.0f;
        Float fat = recipe.getFat() != null ? recipe.getFat().floatValue() : 0.0f;
        Float sugar = recipe.getSugar() != null ? recipe.getSugar().floatValue() : 0.0f;
        Float sodium = recipe.getSodium() != null ? recipe.getSodium().floatValue() : 0.0f;

        return RecipeDocument.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .youtubeChannelName(recipe.getYoutubeChannelName())
                .tags(tags)
                .dishType(recipe.getDishType().name())
                .createdAt(recipe.getCreatedAt().toString())
                .cookingTime(Optional.ofNullable(recipe.getCookingTime()).orElse(0))
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .youtubeUrl(recipe.getYoutubeUrl())
                .isAiGenerated(recipe.isAiGenerated())
                .isPrivate(recipe.getIsPrivate())
                .ingredientIds(ids)
                .ingredientNames(names)
                .ingredientCount(ids.size())
                .totalIngredientCost(cost)
                .totalCalories(calories)
                .protein(protein)
                .carbohydrate(carb)
                .fat(fat)
                .sugar(sugar)
                .sodium(sodium)
                .build();
    }

    @Async
    @Transactional(propagation = Propagation.NEVER)
    public void indexRecipeSafelyWithRetry(Long recipeId) {
        Recipe recipe = recipeRepository
                .findWithAllRelationsById(recipeId)
                .orElse(null);

        if (recipe == null) {
            log.warn("인덱싱 대상 레시피(ID: {})를 찾을 수 없습니다. (DB 미존재)", recipeId);
            return;
        }

        for (int attempt = 1; attempt <= MAX_RETRY_ATTEMPTS; attempt++) {
            try {
                doIndex(recipe);
                log.info("레시피 인덱싱 성공: ID {} (시도 횟수: {})", recipeId, attempt);

                failureLogService.deleteByRecipeId(recipeId);
                return;
            } catch (CustomException e) {
                log.error("OpenSearch 인덱싱 실패 (시도 {}/{}), 레시피 ID: {}",
                        attempt, MAX_RETRY_ATTEMPTS, recipeId, e.getMessage());

                if (attempt == MAX_RETRY_ATTEMPTS) {
                    log.error("최종 인덱싱 실패. ID {}를 실패 로그에 기록합니다.", recipeId);
                    failureLogService.createLog(recipeId);
                    return;
                }

                try {
                    Thread.sleep(RETRY_DELAY_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
    }


    @Async
    @Transactional(propagation = Propagation.NEVER)
    public void updatePrivacyStatusSafely(Long recipeId, boolean isPrivate) {
        for (int attempt = 1; attempt <= MAX_RETRY_ATTEMPTS; attempt++) {
            try {
                Map<String, Object> updateFields = Map.of("isPrivate", isPrivate);
                UpdateRequest req = new UpdateRequest("recipes", recipeId.toString())
                        .doc(objectMapper.writeValueAsString(updateFields), XContentType.JSON)
                        .setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE);

                client.update(req, RequestOptions.DEFAULT);
                log.info("OpenSearch Privacy Status 업데이트 성공: ID {}, isPrivate: {} (시도 {})", recipeId, isPrivate, attempt);

                failureLogService.deleteByRecipeId(recipeId);
                return;

            } catch (IOException e) {
                log.error("OpenSearch Privacy Status 업데이트 실패 (시도 {}/{}), ID: {}",
                        attempt, MAX_RETRY_ATTEMPTS, recipeId, e.getMessage());

                if (attempt == MAX_RETRY_ATTEMPTS) {
                    log.error("최종 업데이트 실패. ID {}를 실패 로그에 기록합니다.", recipeId);
                    failureLogService.createLog(recipeId);
                    return;
                }

                try {
                    Thread.sleep(RETRY_DELAY_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    return;
                }
            } catch (Exception e) {
                log.error("OpenSearch 업데이트 중 예상치 못한 오류 발생, ID: {}", recipeId, e);
                return;
            }
        }
    }

    @Async
    @Transactional(propagation = Propagation.NEVER)
    public void deleteRecipeSafelyWithRetry(Long recipeId) {
        for (int attempt = 1; attempt <= MAX_RETRY_ATTEMPTS; attempt++) {
            try {
                deleteRecipe(recipeId);
                log.info("레시피 OpenSearch 삭제 성공: ID {} (시도 {})", recipeId, attempt);

                failureLogService.deleteByRecipeId(recipeId);
                return;
            } catch (CustomException e) {
                log.error("OpenSearch 삭제 실패 (시도 {}/{}), ID: {}", attempt, MAX_RETRY_ATTEMPTS, recipeId);

                if (attempt == MAX_RETRY_ATTEMPTS) {
                    log.error("최종 삭제 실패. ID {}를 실패 로그에 기록합니다.", recipeId);
                    failureLogService.createLog(recipeId);
                    return;
                }

                try {
                    Thread.sleep(RETRY_DELAY_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
    }

    private void doIndex(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            IndexRequest req = new IndexRequest("recipes")
                    .id(recipe.getId().toString())
                    .source(objectMapper.writeValueAsString(doc), XContentType.JSON)
                    .setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE);
            client.index(req, RequestOptions.DEFAULT);
        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "레시피 색인 실패: " + e.getMessage()
            );
        }
    }

    private void doUpdate(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            UpdateRequest req = new UpdateRequest("recipes", recipe.getId().toString())
                    .doc(objectMapper.writeValueAsString(doc), XContentType.JSON)
                    .setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE);
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