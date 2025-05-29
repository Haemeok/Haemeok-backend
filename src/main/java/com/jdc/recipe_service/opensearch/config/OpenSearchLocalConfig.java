package com.jdc.recipe_service.opensearch.config;

import org.apache.http.HttpHost;
import org.opensearch.client.RestClient;
import org.opensearch.client.RestClientBuilder;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("local")
public class OpenSearchLocalConfig {

    @Bean(destroyMethod = "close")
    public RestHighLevelClient openSearchClient() {
        RestClientBuilder builder = RestClient.builder(
                new HttpHost("localhost", 9200, "http")
        );

        builder.setRequestConfigCallback(
                requestConfigBuilder -> requestConfigBuilder
                        .setConnectTimeout(5000)
                        .setSocketTimeout(10000)
                        .setConnectionRequestTimeout(1000)
        );

        return new RestHighLevelClient(builder);
    }
}