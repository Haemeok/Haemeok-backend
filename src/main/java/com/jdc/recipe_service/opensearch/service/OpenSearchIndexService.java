package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.bulk.BulkRequest;
import org.opensearch.action.bulk.BulkResponse;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.client.Request;
import org.opensearch.client.Response;
import org.opensearch.client.ResponseException;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.common.settings.Settings;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class OpenSearchIndexService {

    private final RestHighLevelClient client;
    private final IngredientRepository ingredientRepo;

    /**
     * 'ingredients' 인덱스가 존재하면 HTTP DELETE로 삭제
     */
    public boolean deleteIngredientIndex() {
        try {
            // Low-level REST Client 사용
            Request req = new Request("DELETE", "/ingredients");
            Response resp = client.getLowLevelClient().performRequest(req);
            return resp.getStatusLine().getStatusCode() == 200;
        } catch (ResponseException e) {
            // 404 Not Found: 인덱스가 없는 경우
            if (e.getResponse().getStatusLine().getStatusCode() == 404) {
                return false;
            }
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        }
    }

    /**
     * 'ingredients' 인덱스를 생성하고 모든 재료를 Bulk 인덱싱
     * 매핑: name(text + keyword), category(keyword)
     */
    public boolean createIngredientIndex() throws IOException {
        // 1) 인덱스 설정
        CreateIndexRequest request = new CreateIndexRequest("ingredients");
        request.settings(Settings.builder()
                .put("index.number_of_shards", 1)
                .put("index.number_of_replicas", 1)
        );

        String mapping = "{\n" +
                "  \"properties\": {\n" +
                "    \"name\":     {\"type\": \"text\",    \"fields\": {\"keyword\": {\"type\":\"keyword\"}}},\n" +
                "    \"category\": {\"type\": \"keyword\"},\n" +
                "    \"imageUrl\": {\"type\": \"keyword\"},\n" +
                "    \"unit\":     {\"type\": \"keyword\"},\n" +
                "    \"name_suggest\": {\"type\": \"completion\"}  \n" +
                "  }\n" +
                "}";
        request.mapping(mapping, XContentType.JSON);
        client.indices().create(request, RequestOptions.DEFAULT);

        // 3) DB에서 재료 전량 조회 후 Bulk 요청 생성
        List<Ingredient> all = ingredientRepo.findAll();
        BulkRequest bulkRequest = new BulkRequest();
        for (Ingredient ing : all) {
            Map<String, Object> doc = new HashMap<>();
            doc.put("name", ing.getName());
            doc.put("category", ing.getCategory());
            doc.put("imageUrl", ing.getImageUrl());
            doc.put("unit",     ing.getUnit());
            doc.put("name_suggest",  ing.getName());

            bulkRequest.add(new IndexRequest("ingredients")
                    .id(ing.getId().toString())
                    .source(doc)
            );
        }

        // 4) Bulk 색인 실행 및 결과 확인
        BulkResponse bulkResponse = client.bulk(bulkRequest, RequestOptions.DEFAULT);
        if (bulkResponse.hasFailures()) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "Bulk 색인 실패: " + bulkResponse.buildFailureMessage()
            );
        }
        return true;
    }

    /**
     * 'ingredients' 인덱스를 삭제 후 재생성 및 재인덱싱
     */
    public boolean recreateIngredientIndex() {
        deleteIngredientIndex();
        try {
            return createIngredientIndex();
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        }
    }

    /**
     * 지정한 인덱스를 삭제
     */
    public boolean deleteIndex(String indexName) {
        try {
            Request req = new Request("DELETE", "/" + indexName);
            Response resp = client.getLowLevelClient().performRequest(req);
            return resp.getStatusLine().getStatusCode() == 200;
        } catch (ResponseException e) {
            if (e.getResponse().getStatusLine().getStatusCode() == 404) {
                return false;
            }
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        }
    }
}
