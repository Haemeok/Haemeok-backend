package com.jdc.recipe_service.dev.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.util.SearchProperties;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.opensearch.OpenSearchStatusException;
import org.opensearch.action.admin.indices.alias.IndicesAliasesRequest;
import org.opensearch.action.admin.indices.alias.get.GetAliasesRequest;
import org.opensearch.action.support.master.AcknowledgedResponse;
import org.opensearch.client.GetAliasesResponse;
import org.opensearch.client.IndicesClient;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.client.indices.CreateIndexResponse;
import org.opensearch.cluster.metadata.AliasMetadata;
import org.opensearch.core.rest.RestStatus;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeIndexingService의 alias 관리 메서드(createDevRecipeIndex / attachDevAlias / swapDevAlias) 검증.
 *
 * 핵심 invariant:
 *  1. createDevRecipeIndex는 index만 생성, alias는 안 붙임 (CreateIndexRequest.aliases() 빈 set)
 *  2. attachDevAlias: alias가 어디에도 없으면 attach (OpenSearch 404 응답을 false로 swallow하여 bootstrap 동작)
 *  3. attachDevAlias: alias가 이미 존재하면 거부 (multi-index alias 사고 차단)
 *  4. swapDevAlias: 한 번의 _aliases 호출에 remove + add 두 액션이 함께 들어감 (atomic transition)
 *
 * 별도 파일인 이유: client.indices() 체인 mock이 다른 mirror 테스트와 setup이 다름. 분리해서 stub strict-mode
 * 충돌을 피하고 alias 관리 의도를 명확히 보여준다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeIndexingServiceAliasManagementTest {

    @Mock RestHighLevelClient client;
    @Mock IndicesClient indicesClient;
    @Mock RecipeRepository recipeRepository;

    private SearchProperties searchProperties;
    private SimpleMeterRegistry meterRegistry;
    private DevRecipeIndexingService service;

    private static final String ALIAS = "recipes_v3_dev";
    private static final String INDEX_OLD = "recipes_v3_dev_20260426_001";
    private static final String INDEX_NEW = "recipes_v3_dev_20260501_002";

    @BeforeEach
    void setUp() {
        searchProperties = new SearchProperties();
        searchProperties.getDevIndex().setEnabled(true);
        searchProperties.getDevIndex().setAlias(ALIAS);
        meterRegistry = new SimpleMeterRegistry();

        service = new DevRecipeIndexingService(
                client, new ObjectMapper(), searchProperties, recipeRepository, meterRegistry);

        // alias 관리 메서드는 모두 client.indices() 체인 사용 → 한 곳에 stub
        given(client.indices()).willReturn(indicesClient);
    }

    // ---------- createDevRecipeIndex ----------

    @Test
    @DisplayName("createDevRecipeIndex: index만 생성, alias는 안 붙임 (CreateIndexRequest.aliases() 비어 있음)")
    void createDevRecipeIndex_indexOnly_noAliasOnRequest() throws Exception {
        CreateIndexResponse ack = mock(CreateIndexResponse.class);
        given(ack.isAcknowledged()).willReturn(true);
        given(indicesClient.create(any(CreateIndexRequest.class), any(RequestOptions.class))).willReturn(ack);

        boolean result = service.createDevRecipeIndex(INDEX_NEW);

        assertThat(result).isTrue();

        ArgumentCaptor<CreateIndexRequest> captor = ArgumentCaptor.forClass(CreateIndexRequest.class);
        verify(indicesClient).create(captor.capture(), any(RequestOptions.class));
        CreateIndexRequest captured = captor.getValue();

        assertThat(captured.index()).isEqualTo(INDEX_NEW);
        // 핵심: alias가 자동으로 붙지 않음 (swap 시 multi-index alias 사고 방지)
        assertThat(captured.aliases())
                .as("createDevRecipeIndex가 alias를 자동 추가하면 swap 시 multi-index alias 사고")
                .isEmpty();
    }

    // ---------- attachDevAlias ----------

    @Test
    @DisplayName("attachDevAlias + alias 없음 (정상 응답): updateAliases 호출 + true 반환")
    void attachDevAlias_aliasMissing_normalResponse_attaches() throws Exception {
        // alias 조회 — 정상 응답이지만 빈 map (alias가 어떤 index에도 없음)
        GetAliasesResponse aliasResp = mock(GetAliasesResponse.class);
        given(aliasResp.getAliases()).willReturn(Map.of());
        given(indicesClient.getAlias(any(GetAliasesRequest.class), any(RequestOptions.class))).willReturn(aliasResp);

        AcknowledgedResponse updateResp = mock(AcknowledgedResponse.class);
        given(updateResp.isAcknowledged()).willReturn(true);
        given(indicesClient.updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class)))
                .willReturn(updateResp);

        boolean result = service.attachDevAlias(INDEX_NEW);

        assertThat(result).isTrue();
        verify(indicesClient).updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class));
    }

    @Test
    @DisplayName("attachDevAlias + alias 없음 (OpenSearch 404 throw): NOT_FOUND swallow → attach (bootstrap 핵심)")
    void attachDevAlias_aliasMissing_404Status_attaches() throws Exception {
        // OpenSearch는 alias가 전혀 없으면 빈 응답이 아닌 404로 응답 — bootstrap에서 항상 발생.
        // 이걸 swallow 안 하면 최초 attachDevAlias가 영원히 실패함 (MUST FIX의 핵심).
        given(indicesClient.getAlias(any(GetAliasesRequest.class), any(RequestOptions.class)))
                .willThrow(new OpenSearchStatusException("alias not found", RestStatus.NOT_FOUND));

        AcknowledgedResponse updateResp = mock(AcknowledgedResponse.class);
        given(updateResp.isAcknowledged()).willReturn(true);
        given(indicesClient.updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class)))
                .willReturn(updateResp);

        boolean result = service.attachDevAlias(INDEX_NEW);

        assertThat(result).isTrue();
        verify(indicesClient).updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class));
    }

    @Test
    @DisplayName("attachDevAlias + alias 이미 다른 index에 존재: updateAliases 호출 안 함 + false 반환 (multi-index alias 차단)")
    void attachDevAlias_aliasAlreadyExists_refuses() throws Exception {
        // alias가 이미 다른 index에 붙어 있는 응답 — 정상 attach하면 multi-index alias가 됨
        GetAliasesResponse aliasResp = mock(GetAliasesResponse.class);
        AliasMetadata existingAlias = AliasMetadata.builder(ALIAS).build();
        given(aliasResp.getAliases()).willReturn(Map.of(INDEX_OLD, Set.of(existingAlias)));
        given(indicesClient.getAlias(any(GetAliasesRequest.class), any(RequestOptions.class))).willReturn(aliasResp);

        boolean result = service.attachDevAlias(INDEX_NEW);

        assertThat(result).isFalse();
        // 핵심 invariant: updateAliases 절대 호출 안 됨 → multi-index alias로 빠지지 않음
        verify(indicesClient, never()).updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class));
    }

    @Test
    @DisplayName("attachDevAlias + 404 외 OpenSearchStatusException: 그대로 propagate (운영자에게 가시화)")
    void attachDevAlias_nonNotFoundStatus_throws() throws Exception {
        // INTERNAL_SERVER_ERROR 같은 진짜 장애는 swallow하면 안 됨 — 운영자가 봐야 함
        given(indicesClient.getAlias(any(GetAliasesRequest.class), any(RequestOptions.class)))
                .willThrow(new OpenSearchStatusException("cluster broken", RestStatus.INTERNAL_SERVER_ERROR));

        try {
            service.attachDevAlias(INDEX_NEW);
            assertThat(false).as("non-404 status는 throw되어야 함").isTrue();
        } catch (OpenSearchStatusException e) {
            assertThat(e.status()).isEqualTo(RestStatus.INTERNAL_SERVER_ERROR);
        }

        verify(indicesClient, never()).updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class));
    }

    // ---------- swapDevAlias ----------

    @Test
    @DisplayName("swapDevAlias: 한 번의 _aliases 호출에 remove(old) + add(new) 두 액션이 함께 들어감 (atomic)")
    void swapDevAlias_emitsRemoveAndAddInOneCall() throws Exception {
        AcknowledgedResponse updateResp = mock(AcknowledgedResponse.class);
        given(updateResp.isAcknowledged()).willReturn(true);
        given(indicesClient.updateAliases(any(IndicesAliasesRequest.class), any(RequestOptions.class)))
                .willReturn(updateResp);

        boolean result = service.swapDevAlias(INDEX_OLD, INDEX_NEW);

        assertThat(result).isTrue();

        ArgumentCaptor<IndicesAliasesRequest> captor = ArgumentCaptor.forClass(IndicesAliasesRequest.class);
        verify(indicesClient).updateAliases(captor.capture(), any(RequestOptions.class));
        List<IndicesAliasesRequest.AliasActions> actions = captor.getValue().getAliasActions();

        // 핵심: 정확히 2개 action — remove old + add new가 한 호출에 들어가야 atomic 보장
        assertThat(actions).hasSize(2);

        IndicesAliasesRequest.AliasActions remove = actions.get(0);
        assertThat(remove.actionType()).isEqualTo(IndicesAliasesRequest.AliasActions.Type.REMOVE);
        assertThat(remove.indices()).contains(INDEX_OLD);
        assertThat(remove.aliases()).contains(ALIAS);

        IndicesAliasesRequest.AliasActions add = actions.get(1);
        assertThat(add.actionType()).isEqualTo(IndicesAliasesRequest.AliasActions.Type.ADD);
        assertThat(add.indices()).contains(INDEX_NEW);
        assertThat(add.aliases()).contains(ALIAS);
    }
}
