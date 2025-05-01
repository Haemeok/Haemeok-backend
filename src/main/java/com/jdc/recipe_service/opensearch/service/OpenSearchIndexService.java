package com.jdc.recipe_service.opensearch.service;

import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.client.indices.CreateIndexRequest;
import org.opensearch.client.indices.CreateIndexResponse;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.stereotype.Service;

import java.io.IOException;

@Service
public class OpenSearchIndexService {

    private final RestHighLevelClient client;

    public OpenSearchIndexService(RestHighLevelClient client) {
        this.client = client;
    }

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
                "min_gram": 2,
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
            "likeCount":   { "type":"integer" }
          }
        }
        """, XContentType.JSON);
        CreateIndexResponse res = client.indices().create(request, RequestOptions.DEFAULT);
        return res.isAcknowledged();
    }
}
