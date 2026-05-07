package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.dev.opensearch.service.DevRecipeIndexingService;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.opensearch.indexingfailure.IndexingFailureLogService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.opensearch.OpenSearchStatusException;
import org.opensearch.action.bulk.BulkItemResponse;
import org.opensearch.action.bulk.BulkResponse;
import org.opensearch.client.IndicesClient;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.core.rest.RestStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/** RecipeIndexingService alias swap + bulk reindex 회귀 (RestHighLevelClient mock 기반). */
@ExtendWith(MockitoExtension.class)
class RecipeIndexingServiceAliasReindexTest {

    @Mock RestHighLevelClient client;
    @Mock IndicesClient indicesClient;
    @Mock RecipeLikeRepository likeRepository;
    @Mock RecipeRepository recipeRepository;
    @Mock IndexingFailureLogService failureLogService;
    @Mock DevRecipeIndexingService devRecipeIndexingService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @InjectMocks RecipeIndexingService service;

    @Test
    @DisplayName("swapRecipeAlias: alias가 처음 등록되는 케이스(404) → previousIndices=[] + add-only swap")
    void swapAlias_firstRegistration_handlesNotFound() throws IOException {
        given(client.indices()).willReturn(indicesClient);
        given(indicesClient.getAlias(any(), any(RequestOptions.class)))
                .willThrow(new OpenSearchStatusException("alias not found", RestStatus.NOT_FOUND));
        given(indicesClient.updateAliases(any(), any(RequestOptions.class)))
                .willReturn(null);

        Set<String> previousIndices = service.swapRecipeAlias("recipes_v2");

        assertThat(previousIndices).isEmpty();
    }

    @Test
    @DisplayName("swapRecipeAlias: alias 자기 자신을 swap target으로 지정하면 IllegalArgumentException")
    void swapAlias_selfTarget_isRejected() {
        assertThatThrownBy(() -> service.swapRecipeAlias("recipes"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("recipes");
    }

    @Test
    @DisplayName("indexAllRecipes: bulk item 일부 실패 → hasFailures=true + failedIds 정확")
    void indexAllRecipes_partialBulkFailure_capturesFailedIds() throws IOException {
        ReflectionTestUtils.setField(service, "objectMapper", objectMapper);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "us-east-1");

        Recipe r10 = recipeFixture(10L);
        Recipe r20 = recipeFixture(20L);
        Page<Recipe> page1 = new PageImpl<>(List.of(r10, r20));
        // 두 번째 페이지(빈 응답)는 loop 종료용. 실제로 1회만 호출되고 끝나는 path도 합법 → strict 경고 회피용 lenient.
        lenient().when(recipeRepository.findAll(any(Pageable.class)))
                .thenReturn(page1, new PageImpl<>(List.of()));

        // production은 실패 item에서만 getId()를 부르므로 okItem.getId() stub은 의도적으로 생략.
        BulkResponse bulkResponse = mock(BulkResponse.class);
        BulkItemResponse okItem = mock(BulkItemResponse.class);
        given(okItem.isFailed()).willReturn(false);
        BulkItemResponse failItem = mock(BulkItemResponse.class);
        given(failItem.isFailed()).willReturn(true);
        given(failItem.getId()).willReturn("20");
        given(failItem.getFailureMessage()).willReturn("simulated parse error");
        given(bulkResponse.hasFailures()).willReturn(true);
        given(bulkResponse.getItems()).willReturn(new BulkItemResponse[]{okItem, failItem});

        given(client.bulk(any(), any(RequestOptions.class))).willReturn(bulkResponse);

        RecipeIndexingService.ReindexResult result = service.indexAllRecipes("recipes_v2");

        assertThat(result.indexName()).isEqualTo("recipes_v2");
        assertThat(result.hasFailures()).isTrue();
        assertThat(result.failedIds()).containsExactly(20L);
        assertThat(result.totalIndexed()).isEqualTo(1L);

        // page overlap/skip 방지 — id ASC 정렬 invariant
        ArgumentCaptor<Pageable> pageableCaptor = ArgumentCaptor.forClass(Pageable.class);
        verify(recipeRepository, atLeastOnce()).findAll(pageableCaptor.capture());
        Pageable firstCall = pageableCaptor.getAllValues().get(0);
        Sort.Order idOrder = firstCall.getSort().getOrderFor("id");
        assertThat(idOrder).isNotNull();
        assertThat(idOrder.isAscending()).isTrue();
    }

    private Recipe recipeFixture(Long id) {
        User user = User.builder().nickname("u" + id).build();
        ReflectionTestUtils.setField(user, "id", id);
        Recipe recipe = Recipe.builder()
                .user(user)
                .title("recipe-" + id)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .isPrivate(false)
                .cookingTime(10)
                .build();
        ReflectionTestUtils.setField(recipe, "id", id);
        ReflectionTestUtils.setField(recipe, "createdAt", java.time.LocalDateTime.now());
        ReflectionTestUtils.setField(recipe, "tags", Collections.emptySet());
        ReflectionTestUtils.setField(recipe, "ingredients", Collections.emptyList());
        return recipe;
    }
}
