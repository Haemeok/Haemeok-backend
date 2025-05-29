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
@Profile("!local")
public class OpenSearchConfig {

    @Value("${opensearch.host}")
    private String HOST;

    @Value("${opensearch.port:443}")
    private int PORT;

    @Value("${opensearch.scheme:https}")
    private String SCHEME;
    private static final String SERVICE_NAME = "es";
    private static final Region AWS_REGION = Region.AP_NORTHEAST_2;

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

        builder.setRequestConfigCallback(
                requestConfigBuilder -> requestConfigBuilder
                        .setConnectTimeout(5000)
                        .setSocketTimeout(10000)
                        .setConnectionRequestTimeout(1000)
        );

        return new RestHighLevelClient(builder);
    }
}