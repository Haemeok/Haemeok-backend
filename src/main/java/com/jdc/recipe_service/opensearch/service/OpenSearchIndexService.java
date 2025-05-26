package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.action.admin.indices.delete.DeleteIndexRequest;
import org.opensearch.action.bulk.BulkRequest;
import org.opensearch.action.bulk.BulkResponse;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.support.IndicesOptions;
import org.opensearch.action.support.master.AcknowledgedResponse;
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

@Slf4j
@Service
@RequiredArgsConstructor
public class OpenSearchIndexService {

    private final RestHighLevelClient client;
    private final IngredientRepository ingredientRepo;

    private static final String INDEX = "ingredients";


    /**
     * 'ingredients' 인덱스가 존재하면 HTTP DELETE로 삭제
     */
    public boolean deleteIngredientIndex() {
        try {
            DeleteIndexRequest req = new DeleteIndexRequest("ingredients")
                    .indicesOptions(IndicesOptions.lenientExpandOpen());
            AcknowledgedResponse resp = client.indices()
                    .delete(req, RequestOptions.DEFAULT);
            return resp.isAcknowledged();
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        }
    }

    /**
     * 'ingredients' 인덱스를 생성하고 모든 재료를 Bulk 인덱싱
     * 매핑: name(text + keyword), category(keyword)
     */
    public boolean createIngredientIndex() throws IOException {
        CreateIndexRequest request = new CreateIndexRequest("ingredients");

        //settings: 샤드 + 레플리카 + 한글 초성 분석기 + 동의어 필터 추가
        request.settings(Settings.builder()
                .put("index.number_of_shards", 1)
                .put("index.number_of_replicas", 1)

                //동의어 필터 정의
                .put("analysis.filter.synonym_filter.type", "synonym")
                .putList("analysis.filter.synonym_filter.synonyms",
                        "파프리카, 피망",
                        "햇반, 즉석밥",
                        "고춧가루, 고추가루",
                        "국간장, 조선간장",
                        "진간장, 왜간장, 양조간장, 간장",
                        "대파, 파",
                        "쪽파, 실파",
                        "부추, 정구지, 전구지",
                        "애호박, 호박",
                        "다진 마늘, 간마늘",
                        "생강, 생강가루",
                        "참기름, 들기름",
                        "멸치액젓, 까나리액젓, 액젓",
                        "새우젓, 자하젓, 추젓, 육젓, 오젓, 곤쟁이젓",
                        "된장, 집된장, 시판된장",
                        "고추장, 태양초고추장",
                        "물엿, 올리고당, 요리당, 조청, 꿀",
                        "미원, 미풍, 감치미, 맛소금",
                        "다시다, 소고기다시다, 멸치다시다, 연두, 산들애",
                        "밀가루, 부침가루, 튀김가루",
                        "전분, 전분가루, 녹말가루, 감자전분, 옥수수전분, 고구마전분",
                        "소면, 중면, 국수",
                        "당면, 잡채",
                        "김, 해우, 해의",
                        "미역, 다시마",
                        "느타리버섯, 표고버섯, 팽이버섯, 새송이버섯, 양송이버섯",
                        "두부, 연두부, 순두부",
                        "어묵, 오뎅",
                        "돼지 앞다리살, 돼지고기 앞다리살, 전지",
                        "돼지고기 삼겹살, 세겹살",
                        "소고기 등심, 안심, 채끝",
                        "닭고기, 영계, 토종닭",
                        "달걀, 계란",
                        "배추, 통배추, 봄동, 얼갈이배추",
                        "무, 조선무, 왜무, 총각무, 열무, 알타리무",
                        "깻잎, 들깻잎",
                        "참깨, 들깨",
                        "식용유, 콩기름, 옥수수유, 카놀라유, 포도씨유, 해바라기씨유, 올리브유",
                        "청주, 맛술, 미림, 정종",
                        "참치액젓, 참치액",
                        "스팸, 통조리햄",
                        "소세지, 소시지",
                        "새우, 생새우",
                        "명란, 명태알",
                        "우동면, 우동사리",
                        "굴, 생굴",
                        "초장, 초고추장",
                        "마늘쫑, 마늘종",
                        "짜장라면, 짜파게티",
                        "카레분말, 카레가루",
                        "갈아만든배, 배음료, 배음료수",
                        "알배추, 알배기배추",
                        "토마토 소스, 토마토 파스타소스, 스파게티 소스",
                        "칵테일새우, 냉동새우",
                        "닭도리, 닭볶음",
                        "케첩, 케찹"
                )

                //edge_ngram 토크나이저 정의
                .put("analysis.tokenizer.edge_ngram_tokenizer.type", "edge_ngram")
                .put("analysis.tokenizer.edge_ngram_tokenizer.min_gram", 1)
                .put("analysis.tokenizer.edge_ngram_tokenizer.max_gram", 20)
                .putList("analysis.tokenizer.edge_ngram_tokenizer.token_chars", "letter")

                // analyzer 정의 (초성 + 동의어)
                .put("analysis.analyzer.korean_edge_ngram.tokenizer", "edge_ngram_tokenizer")
                .putList("analysis.analyzer.korean_edge_ngram.filter",
                        "lowercase",
                        "synonym_filter"
                )
        );

        String mapping = """
        {
          "properties": {
            "name": {
              "type": "text",
              "analyzer": "korean_edge_ngram",
              "search_analyzer": "korean_edge_ngram",
              "fields": {
                "keyword": { "type": "keyword" }
              }
            },
            "category": { "type": "keyword" },
            "unit":      { "type": "keyword" },
            "name_suggest": { "type": "completion" }
          }
        }
        """;
        request.mapping(mapping, XContentType.JSON);

        // 인덱스 생성
        client.indices().create(request, RequestOptions.DEFAULT);

        // DB에서 재료 전량 조회 후 Bulk 인덱싱
        List<Ingredient> all = ingredientRepo.findAll();
        BulkRequest bulkRequest = new BulkRequest();
        for (Ingredient ing : all) {
            Map<String,Object> doc = new HashMap<>();
            doc.put("name", ing.getName());
            doc.put("category", ing.getCategory());
            doc.put("unit", ing.getUnit());
            doc.put("name_suggest", ing.getName());

            bulkRequest.add(new IndexRequest("ingredients")
                    .id(ing.getId().toString())
                    .source(doc)
            );
        }

        BulkResponse bulkResponse = client.bulk(bulkRequest, RequestOptions.DEFAULT);
        if (bulkResponse.hasFailures()) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "Bulk 색인 실패: " + bulkResponse.buildFailureMessage()
            );
        }

        try {
            Request refreshReq = new Request("POST", "/" + INDEX + "/_refresh");
            client.getLowLevelClient().performRequest(refreshReq);
        } catch (IOException ex) {

            log.warn("인덱스 리프레시 실패: {}", ex.getMessage());
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