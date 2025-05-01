package com.jdc.recipe_service.opensearch.config;

import org.apache.http.HttpHost;
import org.apache.http.HttpRequestInterceptor;
import org.opensearch.client.RestClient;
import org.opensearch.client.RestClientBuilder;
import org.opensearch.client.RestHighLevelClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.github.acm19.aws.interceptor.http.AwsRequestSigningApacheInterceptor;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.http.auth.aws.signer.AwsV4HttpSigner;
import software.amazon.awssdk.regions.Region;

@Configuration
public class OpenSearchConfig {

//    @Value("${opensearch.host}") private String host;
//    @Value("${opensearch.port}") private int port;
//    @Value("${opensearch.scheme}") private String scheme;
    private static final String HOST = "vpc-haemeok-opensearch-j5fpdfk5shdxy6snew5j2rnnim.ap-northeast-2.es.amazonaws.com";
    private static final int PORT = 443;
    private static final String SCHEME = "https";

    private static final String SERVICE_NAME = "es";
    private static final Region AWS_REGION = Region.AP_NORTHEAST_2;

    @Bean(destroyMethod = "close")
    public RestHighLevelClient openSearchClient() {
        // ▶ 두 번째 인자를 AwsV4HttpSigner.create() 로 바꿉니다
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

        return new RestHighLevelClient(builder);
    }
}
