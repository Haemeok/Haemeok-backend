package com.jdc.recipe_service.opensearch.config;

import org.apache.http.HttpHost;
import org.apache.http.HttpRequestInterceptor;
import org.opensearch.client.RestClient;
import org.opensearch.client.RestClientBuilder;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import io.github.acm19.aws.interceptor.http.AwsRequestSigningApacheInterceptor;
import org.springframework.context.annotation.Profile;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.http.auth.aws.signer.AwsV4HttpSigner;
import software.amazon.awssdk.regions.Region;

@Configuration
@Profile("!local") // "local" 프로파일이 아닐 때 활성화
public class OpenSearchConfig {

    @Value("${opensearch.host}")
    private String HOST;

    @Value("${opensearch.port:443}")
    private int PORT;

    @Value("${opensearch.scheme:https}")
    private String SCHEME;
    private static final String SERVICE_NAME = "es"; // OpenSearch Service의 경우 "aoss" 또는 "es"
    private static final Region AWS_REGION = Region.AP_NORTHEAST_2; // 사용하시는 리전으로

    @Bean(destroyMethod = "close")
    public RestHighLevelClient openSearchClient() {
        HttpRequestInterceptor interceptor = new AwsRequestSigningApacheInterceptor(
                SERVICE_NAME,
                AwsV4HttpSigner.create(),
                DefaultCredentialsProvider.create(),
                AWS_REGION
        );

        RestClientBuilder builder = RestClient.builder(new HttpHost(HOST, PORT, SCHEME))
                .setHttpClientConfigCallback(httpClientBuilder ->
                        httpClientBuilder.addInterceptorLast(interceptor)
                );

        // === 여기서 타임아웃 설정 추가 ===
        builder.setRequestConfigCallback(
                requestConfigBuilder -> requestConfigBuilder
                        .setConnectTimeout(5000)    // 연결 타임아웃 (5초)
                        .setSocketTimeout(10000)    // 소켓 타임아웃 (데이터 대기 시간, 10초)
                        .setConnectionRequestTimeout(1000) // 커넥션 풀에서 커넥션 가져오는 타임아웃 (1초)
        );
        // === 타임아웃 설정 추가 끝 ===

        return new RestHighLevelClient(builder);
    }
}