package com.jdc.recipe_service.dev.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.dev.opensearch.dto.DevRecipeDocument;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.util.SearchProperties;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeIndexingService invariant 검증.
 *
 *  1. flag disabled → client 호출 없음, counter는 result=disabled
 *  2. flag enabled + 정상 → client.index 호출, counter는 result=success
 *  3. flag enabled + client throw → caller에 throw 안 됨 (swallow), counter는 result=failure
 *  4. mirrorReindex flag enabled + recipe not found → fetch만 시도하고 client 호출 없음, counter는 failure
 *
 * 메트릭 카운터는 SimpleMeterRegistry로 실측. 실제 OpenSearch index/mapping 동작은 통합 테스트 영역.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeIndexingServiceTest {

    @Mock RestHighLevelClient client;
    @Mock RecipeRepository recipeRepository;

    private SearchProperties searchProperties;
    private SimpleMeterRegistry meterRegistry;
    private DevRecipeIndexingService service;

    private static final Long RECIPE_ID = 42L;

    @BeforeEach
    void setUp() {
        searchProperties = new SearchProperties();
        meterRegistry = new SimpleMeterRegistry();

        service = new DevRecipeIndexingService(
                client,
                new ObjectMapper(),
                searchProperties,
                recipeRepository,
                meterRegistry);

        // @Value 필드 — reflection으로 주입
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    private void enable() {
        searchProperties.getDevIndex().setEnabled(true);
    }

    private double counter(String operation, String result) {
        return meterRegistry.counter(DevRecipeIndexingService.METRIC_NAME,
                DevRecipeIndexingService.TAG_OPERATION, operation,
                DevRecipeIndexingService.TAG_RESULT, result).count();
    }

    private Recipe sampleRecipe() {
        User owner = User.builder().nickname("owner").provider("test").oauthId("oid").build();
        ReflectionTestUtils.setField(owner, "id", 7L);
        Recipe recipe = Recipe.builder()
                .user(owner)
                .title("test recipe")
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)
                .build();
        ReflectionTestUtils.setField(recipe, "id", RECIPE_ID);
        // BaseTimeEntity.createdAt 은 보통 auditing이 설정. 단위 테스트에서는 reflection으로 주입.
        ReflectionTestUtils.setField(recipe, "createdAt", LocalDateTime.now());
        return recipe;
    }

    // ---------- mirrorIndex ----------

    @Test
    @DisplayName("flag disabled: mirrorIndex는 client 호출 없이 즉시 반환, counter result=disabled")
    void mirrorIndex_disabled_noClientCall() throws Exception {
        Recipe recipe = sampleRecipe();

        service.mirrorIndex(recipe);

        verifyNoInteractions(client);
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_DISABLED)).isEqualTo(1.0);
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_SUCCESS)).isZero();
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_FAILURE)).isZero();
    }

    @Test
    @DisplayName("flag enabled + client 정상: client.index 호출 + counter result=success")
    void mirrorIndex_enabled_success() throws Exception {
        enable();
        Recipe recipe = sampleRecipe();

        service.mirrorIndex(recipe);

        verify(client).index(any(IndexRequest.class), any(RequestOptions.class));
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_SUCCESS)).isEqualTo(1.0);
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_FAILURE)).isZero();
    }

    @Test
    @DisplayName("flag enabled + client.index throw: caller에 throw 안 됨 (swallow) + counter result=failure")
    void mirrorIndex_enabled_clientThrows_swallowed() throws Exception {
        enable();
        Recipe recipe = sampleRecipe();
        given(client.index(any(IndexRequest.class), any(RequestOptions.class)))
                .willThrow(new IOException("OpenSearch dev cluster down"));

        // 핵심: 호출자에게 throw되지 않아야 함 (운영 path 영향 0)
        assertThatCode(() -> service.mirrorIndex(recipe)).doesNotThrowAnyException();

        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_FAILURE)).isEqualTo(1.0);
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_SUCCESS)).isZero();
    }

    // ---------- mirrorDelete ----------

    @Test
    @DisplayName("flag disabled: mirrorDelete는 client 호출 없이 즉시 반환, counter result=disabled")
    void mirrorDelete_disabled_noClientCall() {
        service.mirrorDelete(RECIPE_ID);

        verifyNoInteractions(client);
        assertThat(counter(DevRecipeIndexingService.OP_DELETE, DevRecipeIndexingService.RESULT_DISABLED)).isEqualTo(1.0);
    }

    @Test
    @DisplayName("flag enabled + client.delete throw: caller에 throw 안 됨 + counter result=failure")
    void mirrorDelete_enabled_clientThrows_swallowed() throws Exception {
        enable();
        given(client.delete(any(), any(RequestOptions.class)))
                .willThrow(new IOException("delete failed"));

        assertThatCode(() -> service.mirrorDelete(RECIPE_ID)).doesNotThrowAnyException();

        assertThat(counter(DevRecipeIndexingService.OP_DELETE, DevRecipeIndexingService.RESULT_FAILURE)).isEqualTo(1.0);
    }

    // ---------- mirrorReindex ----------

    @Test
    @DisplayName("flag disabled: mirrorReindex는 repo 조회 없이 즉시 반환, counter result=disabled")
    void mirrorReindex_disabled_noRepoCall() {
        service.mirrorReindex(RECIPE_ID);

        verifyNoInteractions(recipeRepository);
        verifyNoInteractions(client);
        assertThat(counter(DevRecipeIndexingService.OP_REINDEX, DevRecipeIndexingService.RESULT_DISABLED)).isEqualTo(1.0);
    }

    @Test
    @DisplayName("flag enabled + recipe not found: client 호출 없이 counter result=failure (없는 ID swallow)")
    void mirrorReindex_enabled_recipeNotFound_failureCounter() {
        enable();
        given(recipeRepository.findWithAllRelationsById(RECIPE_ID)).willReturn(Optional.empty());

        service.mirrorReindex(RECIPE_ID);

        verify(recipeRepository).findWithAllRelationsById(RECIPE_ID);
        verifyNoInteractions(client);
        assertThat(counter(DevRecipeIndexingService.OP_REINDEX, DevRecipeIndexingService.RESULT_FAILURE)).isEqualTo(1.0);
    }

    @Test
    @DisplayName("flag enabled + 정상 reindex: client.index 호출 + counter operation=reindex, result=success")
    void mirrorReindex_enabled_success() throws Exception {
        enable();
        Recipe recipe = sampleRecipe();
        given(recipeRepository.findWithAllRelationsById(RECIPE_ID)).willReturn(Optional.of(recipe));

        service.mirrorReindex(RECIPE_ID);

        verify(client).index(any(IndexRequest.class), any(RequestOptions.class));
        // mirrorReindex는 OP_REINDEX 태그로 기록 (mirrorIndex의 OP_INDEX와 분리)
        assertThat(counter(DevRecipeIndexingService.OP_REINDEX, DevRecipeIndexingService.RESULT_SUCCESS)).isEqualTo(1.0);
        assertThat(counter(DevRecipeIndexingService.OP_INDEX, DevRecipeIndexingService.RESULT_SUCCESS)).isZero();
    }

    // ---------- buildDocument: dev V3 신규 5 필드 매핑 ----------

    @Test
    @DisplayName("buildDocument: PUBLIC/LISTED/ACTIVE/USER + ownerId 매핑이 dev document 5 신규 필드에 정확히 들어간다")
    void buildDocument_mapsDevV3Fields_publicListedActiveUser() {
        Recipe recipe = sampleRecipe(); // PUBLIC + LISTED + ACTIVE + USER, owner.id=7L

        DevRecipeDocument doc = service.buildDocument(recipe);

        // A2 검색 필터(DevRecipeSearchFilters)가 의존하는 핵심 5 필드
        assertThat(doc.getVisibility()).isEqualTo("PUBLIC");
        assertThat(doc.getListingStatus()).isEqualTo("LISTED");
        assertThat(doc.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(doc.getSource()).isEqualTo("USER");
        assertThat(doc.getUserId()).isEqualTo(7L);

        // 기존 필드 sanity (운영 RecipeDocument 호환)
        assertThat(doc.getId()).isEqualTo(RECIPE_ID);
        assertThat(doc.getTitle()).isEqualTo("test recipe");
        assertThat(doc.getDishType()).isEqualTo("FRYING");
    }

    @Test
    @DisplayName("buildDocument: PRIVATE/UNLISTED/HIDDEN/AI 조합도 정확히 enum.name() 문자열로 매핑")
    void buildDocument_mapsDevV3Fields_alternateCombo() {
        User owner = User.builder().nickname("alt").provider("test").oauthId("alt-oid").build();
        ReflectionTestUtils.setField(owner, "id", 99L);
        Recipe recipe = Recipe.builder()
                .user(owner)
                .title("hidden private")
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.HIDDEN)
                .visibility(RecipeVisibility.PRIVATE)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.AI)
                .build();
        ReflectionTestUtils.setField(recipe, "id", 77L);
        ReflectionTestUtils.setField(recipe, "createdAt", LocalDateTime.now());

        DevRecipeDocument doc = service.buildDocument(recipe);

        assertThat(doc.getVisibility()).isEqualTo("PRIVATE");
        assertThat(doc.getListingStatus()).isEqualTo("UNLISTED");
        assertThat(doc.getLifecycleStatus()).isEqualTo("HIDDEN");
        assertThat(doc.getSource()).isEqualTo("AI");
        assertThat(doc.getUserId()).isEqualTo(99L);
    }
}
