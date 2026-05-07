package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.dev.opensearch.service.DevRecipeIndexingService;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import com.jdc.recipe_service.opensearch.indexingfailure.IndexingFailureLogService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.OpenSearchStatusException;
import org.opensearch.action.admin.indices.alias.IndicesAliasesRequest;
import org.opensearch.action.admin.indices.alias.IndicesAliasesRequest.AliasActions;
import org.opensearch.action.admin.indices.alias.get.GetAliasesRequest;
import org.opensearch.action.bulk.BulkItemResponse;
import org.opensearch.action.bulk.BulkRequest;
import org.opensearch.action.bulk.BulkResponse;
import org.opensearch.action.support.WriteRequest;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.client.indices.CreateIndexResponse;
import org.opensearch.cluster.metadata.AliasMetadata;
import org.opensearch.core.rest.RestStatus;
import org.opensearch.action.delete.DeleteRequest;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.update.UpdateRequest;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
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
    /**
     * Dev V3 mirror — flag off (default)면 모든 호출 no-op, 실패는 swallow.
     * 절대 운영 indexing path에 throw하지 않는다는 invariant에 의존한다.
     */
    private final DevRecipeIndexingService devRecipeIndexingService;

    private static final int MAX_RETRY_ATTEMPTS = 3;
    private static final long RETRY_DELAY_MS = 2000;

    /** 일상 read/write가 사용하는 logical 이름. alias로 등록되면 alias swap으로 무중단 재색인 가능. */
    public static final String RECIPE_INDEX_ALIAS = "recipes";

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

    public boolean createRecipeIndex() throws IOException {
        return createRecipeIndex(RECIPE_INDEX_ALIAS);
    }

    // 명시 mapping 사용 — dynamic mapping에 기대면 string field가 text+keyword subfield로 잡혀 termQuery 정확도가 깨진다.
    public boolean createRecipeIndex(String indexName) throws IOException {
        var request = new CreateIndexRequest(indexName);

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
                    "sodium": { "type": "float" },
                    "visibility": { "type": "keyword" },
                    "listingStatus": { "type": "keyword" },
                    "lifecycleStatus": { "type": "keyword" }
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
        doIndex(recipe);
    }

    public ReindexResult indexAllRecipes() {
        return indexAllRecipes(RECIPE_INDEX_ALIAS);
    }

    /**
     * DB → toDocument() 기반 bulk 색인. OpenSearch native {@code _reindex}는 구 인덱스에 새 enum 필드가
     * 없거나 dynamic mapping으로 잘못 잡혀 있을 수 있어 새 인덱스에는 반드시 DB에서 새로 색인한다.
     *
     * <p>호출자는 {@link ReindexResult#hasFailures()}가 true면 alias swap을 진행하면 안 된다.
     */
    public ReindexResult indexAllRecipes(String indexName) {
        int page = 0;
        int size = 1000;
        long totalIndexed = 0;
        List<Long> failedIds = new ArrayList<>();
        log.info("레시피 Bulk 재색인 시작 — target index: {}", indexName);

        while (true) {
            // 정렬 없으면 페이지 사이에 row 누락/중복 가능 — alias swap용 전체 색인이라 id ASC 명시.
            Page<Recipe> recipePage = recipeRepository.findAll(
                    PageRequest.of(page, size, Sort.by("id").ascending()));
            if (!recipePage.hasContent()) {
                break;
            }

            BulkRequest bulkRequest = new BulkRequest();
            List<Long> pageIds = new ArrayList<>(recipePage.getContent().size());

            for (Recipe recipe : recipePage.getContent()) {
                try {
                    RecipeDocument doc = buildDocument(recipe);

                    bulkRequest.add(new IndexRequest(indexName)
                            .id(recipe.getId().toString())
                            .source(objectMapper.writeValueAsString(doc), XContentType.JSON)
                    );
                    pageIds.add(recipe.getId());
                } catch (Exception e) {
                    log.error("Bulk 색인 중 문서 변환 오류 ID: {}", recipe.getId(), e);
                    failedIds.add(recipe.getId());
                }
            }
            try {
                if (bulkRequest.numberOfActions() > 0) {
                    BulkResponse response = client.bulk(bulkRequest, RequestOptions.DEFAULT);
                    if (response.hasFailures()) {
                        for (BulkItemResponse item : response.getItems()) {
                            if (item.isFailed()) {
                                long failedId = Long.parseLong(item.getId());
                                failedIds.add(failedId);
                                log.error("Bulk item 실패 ID: {}, message: {}", failedId,
                                        item.getFailureMessage());
                            }
                        }
                    }
                    totalIndexed += countSucceeded(response);
                    log.info("레시피 Bulk 색인 진행 중 ({}): 누적 성공 {}건, 누적 실패 {}건",
                            indexName, totalIndexed, failedIds.size());
                }
            } catch (IOException e) {
                log.error("레시피 Bulk 색인 실패 (페이지: {}, target: {}, 영향받은 row 수: {})",
                        page, indexName, pageIds.size(), e);
                failedIds.addAll(pageIds);
            }
            page++;
        }
        boolean hasFailures = !failedIds.isEmpty();
        if (hasFailures) {
            log.error("레시피 Bulk 색인 완료 — target {}: 성공 {}건 / 실패 {}건. **alias swap 금지**",
                    indexName, totalIndexed, failedIds.size());
        } else {
            log.info("모든 레시피 Bulk 색인 완료 — target index: {}, 총 {}건", indexName, totalIndexed);
        }
        return new ReindexResult(indexName, totalIndexed, failedIds, hasFailures);
    }

    private static long countSucceeded(BulkResponse response) {
        long count = 0;
        for (BulkItemResponse item : response.getItems()) {
            if (!item.isFailed()) count++;
        }
        return count;
    }

    /** Bulk 재색인 결과. {@code hasFailures()}가 true면 alias swap 금지. */
    public record ReindexResult(String indexName, long totalIndexed, List<Long> failedIds, boolean hasFailures) {}

    /**
     * Atomic alias swap. 단일 작업이라 검색 클라이언트가 "alias 미존재" 순간을 보지 않는다.
     *
     * <p>alias name과 동일한 concrete index가 있으면 alias 등록 자체가 충돌하므로 운영자가 사전에 구 인덱스를 정리해야 한다.
     *
     * @return swap 전 alias가 가리키던 인덱스 이름들 (구 인덱스 정리 참고용)
     */
    public Set<String> swapRecipeAlias(String newIndexName) throws IOException {
        // controller 가드 우회 차단 — service 직접 호출 경로에서도 alias 자기 자신 swap을 거부.
        if (RECIPE_INDEX_ALIAS.equals(newIndexName)) {
            throw new IllegalArgumentException("newIndexName='" + RECIPE_INDEX_ALIAS
                    + "'은 alias 자기 자신을 swap target으로 지정한 것 — 새 인덱스 이름(예: recipes_v2)을 사용하세요.");
        }

        // alias 자체가 없는 첫 등록 시점에는 OpenSearch가 404를 던진다 — empty로 처리하고 add만 수행.
        Set<String> previousIndices;
        try {
            GetAliasesRequest getReq = new GetAliasesRequest(RECIPE_INDEX_ALIAS);
            Map<String, Set<AliasMetadata>> aliasMap = client.indices().getAlias(getReq, RequestOptions.DEFAULT)
                    .getAliases();
            previousIndices = aliasMap.keySet();
        } catch (OpenSearchStatusException e) {
            if (e.status() == RestStatus.NOT_FOUND) {
                log.info("alias '{}'가 아직 등록돼있지 않음 — 첫 등록으로 처리 (previousIndices=[])",
                        RECIPE_INDEX_ALIAS);
                previousIndices = Set.of();
            } else {
                throw e;
            }
        }

        // 2) atomic swap: 기존 alias 제거 + 새 인덱스에 alias add
        IndicesAliasesRequest swapReq = new IndicesAliasesRequest();
        for (String prev : previousIndices) {
            swapReq.addAliasAction(AliasActions.remove()
                    .index(prev)
                    .alias(RECIPE_INDEX_ALIAS));
        }
        swapReq.addAliasAction(AliasActions.add()
                .index(newIndexName)
                .alias(RECIPE_INDEX_ALIAS));

        client.indices().updateAliases(swapReq, RequestOptions.DEFAULT);
        log.info("alias swap 완료 — alias '{}' : {} → {}",
                RECIPE_INDEX_ALIAS, previousIndices, newIndexName);
        return previousIndices;
    }

    public void updateRecipe(Recipe recipe) {
        doUpdate(recipe);
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

    public void deleteRecipe(Long recipeId) {
        try {
            DeleteRequest request = new DeleteRequest(RECIPE_INDEX_ALIAS, recipeId.toString());
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
                .filter(Objects::nonNull)
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
                .visibility(recipe.getVisibility() != null ? recipe.getVisibility().name() : null)
                .listingStatus(recipe.getListingStatus() != null ? recipe.getListingStatus().name() : null)
                .lifecycleStatus(recipe.getLifecycleStatus() != null ? recipe.getLifecycleStatus().name() : null)
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

                // Dev V3 mirror — flag off면 no-op, 실패는 swallow (운영 path 영향 없음)
                devRecipeIndexingService.mirrorIndex(recipe);

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
            } catch (Exception e) {
                log.error("인덱싱 중 예상치 못한 오류 발생 ID: {}", recipeId, e);
                return;
            }
        }
    }


    /** isPrivate만 partial update — visibility/listingStatus/lifecycleStatus는 여기서 갱신되지 않는다. */
    @Async
    @Transactional(propagation = Propagation.NEVER)
    public void updatePrivacyStatusSafely(Long recipeId, boolean isPrivate) {
        for (int attempt = 1; attempt <= MAX_RETRY_ATTEMPTS; attempt++) {
            try {
                Map<String, Object> updateFields = Map.of("isPrivate", isPrivate);
                UpdateRequest req = new UpdateRequest(RECIPE_INDEX_ALIAS, recipeId.toString())
                        .doc(objectMapper.writeValueAsString(updateFields), XContentType.JSON)
                        .setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE);

                client.update(req, RequestOptions.DEFAULT);
                log.info("OpenSearch Privacy Status 업데이트 성공: ID {}, isPrivate: {} (시도 {})", recipeId, isPrivate, attempt);

                // Dev V3 mirror — visibility 변경은 4-enum 모두 갱신되어야 하므로 partial update가 아닌 full reindex.
                // flag off면 no-op, 실패는 swallow.
                devRecipeIndexingService.mirrorReindex(recipeId);

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

                // Dev V3 mirror — flag off면 no-op, 실패는 swallow
                devRecipeIndexingService.mirrorDelete(recipeId);

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
            IndexRequest req = new IndexRequest(RECIPE_INDEX_ALIAS)
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
            UpdateRequest req = new UpdateRequest(RECIPE_INDEX_ALIAS, recipe.getId().toString())
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