package com.jdc.recipe_service.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3Configuration;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;

import java.net.URI;

@Configuration
@Profile("local")
public class LocalAwsStubConfig {

    private static final URI LOCAL_S3 = URI.create("http://localhost:4566");

    @Bean
    public S3Client s3Client() {
        return S3Client.builder()
                .region(Region.of("ap-northeast-2"))
                .credentialsProvider(DefaultCredentialsProvider.create())
                .endpointOverride(LOCAL_S3)
                .serviceConfiguration(
                        S3Configuration.builder()
                                .pathStyleAccessEnabled(true)
                                .build()
                )
                .build();
    }

    @Bean
    public S3Presigner s3Presigner() {
        return S3Presigner.builder()
                .region(Region.of("ap-northeast-2"))
                .credentialsProvider(DefaultCredentialsProvider.create())
                .endpointOverride(LOCAL_S3)
                .serviceConfiguration(
                        S3Configuration.builder()
                                .pathStyleAccessEnabled(true)
                                .build()
                )
                .build();
    }
}