package com.jdc.recipe_service.opensearch.config;

import org.apache.http.HttpHost;
import org.opensearch.client.RestClient;
import org.opensearch.client.RestClientBuilder;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("local") // "local" 프로파일일 때 활성화
public class OpenSearchLocalConfig {

    @Bean(destroyMethod = "close")
    public RestHighLevelClient openSearchClient() {
        RestClientBuilder builder = RestClient.builder(
                new HttpHost("localhost", 9200, "http") // 로컬 OpenSearch 호스트 정보
        );

        // === 여기서 타임아웃 설정 추가 ===
        builder.setRequestConfigCallback(
                requestConfigBuilder -> requestConfigBuilder
                        .setConnectTimeout(5000)    // 연결 타임아웃 (5초)
                        .setSocketTimeout(10000)    // 소켓 타임아웃 (10초)
                        .setConnectionRequestTimeout(1000) // 커넥션 풀 타임아웃 (1초)
        );
        // === 타임아웃 설정 추가 끝 ===

        return new RestHighLevelClient(builder);
    }
}