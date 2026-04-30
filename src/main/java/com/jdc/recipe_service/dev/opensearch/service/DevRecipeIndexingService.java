package com.jdc.recipe_service.dev.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.dev.opensearch.dto.DevRecipeDocument;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.util.SearchProperties;
import io.micrometer.core.instrument.MeterRegistry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.OpenSearchStatusException;
import org.opensearch.action.admin.indices.alias.IndicesAliasesRequest;
import org.opensearch.action.admin.indices.alias.get.GetAliasesRequest;
import org.opensearch.action.delete.DeleteRequest;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.support.WriteRequest;
import org.opensearch.client.GetAliasesResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.common.xcontent.XContentType;
import org.opensearch.core.rest.RestStatus;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Dev V3 mirror indexing service.
 *
 * 역할: 운영 {@link RecipeIndexingService}가 OpenSearch primary write에 성공한 후, 같은 document를
 * 별도 dev alias({@code recipes_v3_dev})에 mirror write한다. dev 검증 환경에서 visibility/listingStatus
 * 같은 신규 필드를 시험할 수 있게 하면서 운영 검색 인덱스는 손대지 않는다.
 *
 * 핵심 invariant:
 *  1. {@code search.dev-index.enabled = false} (default) → 모든 mirror 호출은 no-op (DAO 호출 없음)
 *  2. dev mirror 실패는 절대 운영 path에 throw되지 않음 → 모든 예외를 swallow + log + counter
 *  3. dev alias / concrete index는 코드가 자동 생성하지 않음
 *     → docs/dev/v3-search-index.md의 수동 절차 또는 {@link #createDevRecipeIndex(String)} 명시 호출
 *
 * 메트릭: {@code dev_index_mirror_total} counter (tags: operation=index|delete|reindex,
 *   result=success|failure|disabled). 카디널리티는 3×3 = 9 조합으로 제한.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeIndexingService {

    static final String METRIC_NAME = "dev_index_mirror_total";
    static final String TAG_OPERATION = "operation";
    static final String TAG_RESULT = "result";
    static final String OP_INDEX = "index";
    static final String OP_DELETE = "delete";
    static final String OP_REINDEX = "reindex";
    static final String RESULT_SUCCESS = "success";
    static final String RESULT_FAILURE = "failure";
    static final String RESULT_DISABLED = "disabled";

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final SearchProperties searchProperties;
    private final RecipeRepository recipeRepository;
    private final MeterRegistry meterRegistry;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    // -----------------------------------------------------------------------
    // mirror operations — RecipeIndexingService에서 운영 write 성공 후 호출
    // -----------------------------------------------------------------------

    /** 운영 doIndex 성공 후 호출. recipe는 호출자가 이미 fetch한 것을 그대로 전달. */
    public void mirrorIndex(Recipe recipe) {
        if (!isEnabled()) {
            recordResult(OP_INDEX, RESULT_DISABLED);
            return;
        }
        doMirrorIndex(recipe, OP_INDEX);
    }

    /** 운영 deleteRecipe 성공 후 호출. recipe 객체 없이 id만으로 dev alias에서 삭제. */
    public void mirrorDelete(Long recipeId) {
        if (!isEnabled()) {
            recordResult(OP_DELETE, RESULT_DISABLED);
            return;
        }
        try {
            DeleteRequest req = new DeleteRequest(aliasName(), recipeId.toString());
            client.delete(req, RequestOptions.DEFAULT);
            recordResult(OP_DELETE, RESULT_SUCCESS);
            log.debug("[DevIdx] mirror delete OK: recipeId={}", recipeId);
        } catch (Exception e) {
            recordResult(OP_DELETE, RESULT_FAILURE);
            log.warn("[DevIdx] mirror delete FAILED (swallowed): recipeId={}, error={}",
                    recipeId, safeMsg(e));
        }
    }

    /**
     * visibility 변경 시 호출. 운영의 isPrivate-only partial update와 달리 dev document는
     * visibility/listingStatus/lifecycleStatus 모두 갱신되어야 하므로 full document 재색인.
     */
    public void mirrorReindex(Long recipeId) {
        if (!isEnabled()) {
            recordResult(OP_REINDEX, RESULT_DISABLED);
            return;
        }
        Recipe recipe = recipeRepository.findWithAllRelationsById(recipeId).orElse(null);
        if (recipe == null) {
            log.warn("[DevIdx] mirror reindex skipped — recipe not found: recipeId={}", recipeId);
            recordResult(OP_REINDEX, RESULT_FAILURE);
            return;
        }
        doMirrorIndex(recipe, OP_REINDEX);
    }

    private void doMirrorIndex(Recipe recipe, String operation) {
        try {
            DevRecipeDocument doc = buildDocument(recipe);
            IndexRequest req = new IndexRequest(aliasName())
                    .id(recipe.getId().toString())
                    .source(objectMapper.writeValueAsString(doc), XContentType.JSON)
                    .setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE);
            client.index(req, RequestOptions.DEFAULT);
            recordResult(operation, RESULT_SUCCESS);
            log.debug("[DevIdx] mirror {} OK: recipeId={}", operation, recipe.getId());
        } catch (Exception e) {
            recordResult(operation, RESULT_FAILURE);
            log.warn("[DevIdx] mirror {} FAILED (swallowed): recipeId={}, error={}",
                    operation, recipe.getId(), safeMsg(e));
        }
    }

    // -----------------------------------------------------------------------
    // 수동 admin operation — concrete index 생성과 alias 책임을 분리
    //
    // 책임 분리 이유: createDevRecipeIndex가 alias도 함께 붙이면 swap 시 (새 index를 만들 때마다)
    // alias가 양쪽 index에 모두 붙어 multi-index alias가 됨 → dev 검색 결과 중복 + write index 모호.
    // 분리하면 호출자가 "최초 bootstrap이면 attach, 매핑 변경이면 swap"을 명시적으로 선택.
    // -----------------------------------------------------------------------

    /**
     * dev concrete index를 생성한다 (alias는 붙이지 않음).
     * 호출 절차/예시는 docs/dev/v3-search-index.md 참고. 수동 실행을 가정 (startup hook 없음).
     *
     * @param concreteIndexName 예: {@code recipes_v3_dev_20260426_001} — 버전 suffix 권장
     */
    public boolean createDevRecipeIndex(String concreteIndexName) throws IOException {
        CreateIndexRequest req = new CreateIndexRequest(concreteIndexName);
        req.settings(indexSettingsJson(), XContentType.JSON);
        req.mapping(indexMappingJson(), XContentType.JSON);
        return client.indices().create(req, RequestOptions.DEFAULT).isAcknowledged();
    }

    /**
     * 최초 bootstrap용 — alias가 어떤 index에도 붙어있지 않을 때만 attach.
     *
     * 이미 다른 index에 같은 alias가 붙어 있으면 multi-index alias 위험을 막기 위해 거부 (warn log + false 반환).
     * 매핑 변경 시 alias 이동은 {@link #swapDevAlias(String, String)}을 사용해야 한다.
     */
    public boolean attachDevAlias(String concreteIndexName) throws IOException {
        if (aliasExists()) {
            log.warn("[DevIdx] attachDevAlias skipped — alias '{}' already exists. Use swapDevAlias to move it.",
                    aliasName());
            return false;
        }
        IndicesAliasesRequest req = new IndicesAliasesRequest();
        req.addAliasAction(IndicesAliasesRequest.AliasActions.add()
                .index(concreteIndexName)
                .alias(aliasName()));
        return client.indices().updateAliases(req, RequestOptions.DEFAULT).isAcknowledged();
    }

    /**
     * Atomic alias swap — old에서 떼고 new에 붙이기를 한 번의 _aliases 호출로 처리.
     * zero-downtime 매핑 변경 시 사용 (수정 사이 alias가 끊긴 순간이 없도록 OpenSearch가 보장).
     */
    public boolean swapDevAlias(String oldConcreteIndex, String newConcreteIndex) throws IOException {
        IndicesAliasesRequest req = new IndicesAliasesRequest();
        req.addAliasAction(IndicesAliasesRequest.AliasActions.remove()
                .index(oldConcreteIndex)
                .alias(aliasName()));
        req.addAliasAction(IndicesAliasesRequest.AliasActions.add()
                .index(newConcreteIndex)
                .alias(aliasName()));
        return client.indices().updateAliases(req, RequestOptions.DEFAULT).isAcknowledged();
    }

    /**
     * alias가 어떤 index에든 붙어있는지 확인.
     *
     * OpenSearch는 alias가 전혀 없으면 빈 응답이 아니라 404 ({@link OpenSearchStatusException})로 응답한다.
     * 최초 bootstrap 시점에는 항상 이 케이스이므로 NOT_FOUND를 false로 swallow해야 attachDevAlias가 동작한다.
     */
    private boolean aliasExists() throws IOException {
        GetAliasesRequest req = new GetAliasesRequest(aliasName());
        try {
            GetAliasesResponse resp = client.indices().getAlias(req, RequestOptions.DEFAULT);
            return resp.getAliases() != null && !resp.getAliases().isEmpty();
        } catch (OpenSearchStatusException e) {
            if (e.status() == RestStatus.NOT_FOUND) {
                return false;
            }
            throw e;
        }
    }

    // -----------------------------------------------------------------------
    // private helpers
    // -----------------------------------------------------------------------

    private boolean isEnabled() {
        SearchProperties.DevIndex devIndex = searchProperties.getDevIndex();
        return devIndex != null && devIndex.isEnabled();
    }

    /**
     * 설정된 dev alias 이름. {@link SearchProperties.DevIndex#getAlias()}가 비어 있으면 운영 설정 실수이므로
     * 명확한 에러 메시지로 즉시 실패시킨다 (alias 없이 호출이 진행되어 OpenSearch에서 모호한 에러 나는 것 방지).
     */
    private String aliasName() {
        SearchProperties.DevIndex devIndex = searchProperties.getDevIndex();
        if (devIndex == null) {
            throw new IllegalStateException("search.dev-index config is missing");
        }
        String alias = devIndex.getAlias();
        if (alias == null || alias.isBlank()) {
            throw new IllegalStateException("search.dev-index.alias must not be blank");
        }
        return alias;
    }

    private void recordResult(String operation, String result) {
        meterRegistry.counter(METRIC_NAME,
                TAG_OPERATION, operation,
                TAG_RESULT, result).increment();
    }

    private static String safeMsg(Exception e) {
        return e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName();
    }

    /**
     * Recipe → DevRecipeDocument 변환.
     * 운영 buildDocument와 동일 base + 신규 5 필드.
     * PANTRY_IDS 필터링도 동일 적용 (검색 의미 유지).
     */
    DevRecipeDocument buildDocument(Recipe recipe) {
        var filtered = Optional.ofNullable(recipe.getIngredients())
                .orElse(List.of())
                .stream()
                .filter(Objects::nonNull)
                .filter(ri -> ri.getIngredient() != null)
                .filter(ri -> !RecipeIndexingService.PANTRY_IDS.contains(ri.getIngredient().getId()))
                .toList();

        List<Long> ids = filtered.stream().map(ri -> ri.getIngredient().getId()).toList();
        List<String> names = filtered.stream().map(ri -> ri.getIngredient().getName()).toList();

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

        return DevRecipeDocument.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .youtubeChannelName(recipe.getYoutubeChannelName())
                .tags(tags)
                .dishType(recipe.getDishType().name())
                .createdAt(recipe.getCreatedAt() != null ? recipe.getCreatedAt().toString() : null)
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
                // === Dev V3 신규 필드 ===
                .visibility(recipe.getVisibility() != null ? recipe.getVisibility().name() : null)
                .listingStatus(recipe.getListingStatus() != null ? recipe.getListingStatus().name() : null)
                .lifecycleStatus(recipe.getLifecycleStatus() != null ? recipe.getLifecycleStatus().name() : null)
                .source(recipe.getSource() != null ? recipe.getSource().name() : null)
                .userId(recipe.getUser() != null ? recipe.getUser().getId() : null)
                .build();
    }

    private String generateImageUrl(String key) {
        return key == null ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    // -----------------------------------------------------------------------
    // index settings/mapping JSON — 운영 createRecipeIndex 기반 + 신규 5 필드
    // -----------------------------------------------------------------------

    private String indexSettingsJson() {
        return """
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
                """;
    }

    private String indexMappingJson() {
        return """
                {
                  "properties": {
                    "title": {
                      "type": "text",
                      "analyzer": "korean_analyzer",
                      "fields": {
                        "keyword": { "type": "keyword" },
                        "prefix":  { "type": "text", "analyzer": "autocomplete_analyzer" },
                        "infix":   { "type": "text", "analyzer": "infix_analyzer", "search_analyzer": "standard" }
                      }
                    },
                    "youtubeChannelName": {
                      "type": "text",
                      "analyzer": "korean_analyzer",
                      "fields": { "keyword": { "type": "keyword" } }
                    },
                    "dishType":            { "type": "keyword" },
                    "tags":                { "type": "keyword" },
                    "createdAt":           { "type": "date" },
                    "cookingTime":         { "type": "integer" },
                    "imageUrl":            { "type": "keyword" },
                    "youtubeUrl":          { "type": "keyword" },
                    "isAiGenerated":       { "type": "boolean" },
                    "isPrivate":           { "type": "boolean" },
                    "ingredientIds":       { "type": "long" },
                    "ingredientCount":     { "type": "integer" },
                    "totalIngredientCost": { "type": "integer" },
                    "totalCalories":       { "type": "float" },
                    "protein":             { "type": "float" },
                    "carbohydrate":        { "type": "float" },
                    "fat":                 { "type": "float" },
                    "sugar":               { "type": "float" },
                    "sodium":              { "type": "float" },

                    "visibility":          { "type": "keyword" },
                    "listingStatus":       { "type": "keyword" },
                    "lifecycleStatus":     { "type": "keyword" },
                    "source":              { "type": "keyword" },
                    "userId":              { "type": "long" }
                  }
                }
                """;
    }
}
