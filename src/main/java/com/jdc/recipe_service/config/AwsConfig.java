package com.jdc.recipe_service.config;


import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;

import java.net.URI;

@Configuration
@Profile("prod")
public class AwsConfig {

    @Value("${cloud.aws.region.static}")
    private String region;

    @Bean
    public S3Client s3Client() {
        return S3Client.builder()
                .region(Region.of(region))
                .credentialsProvider(DefaultCredentialsProvider.create())
                .build();
    }

    @Bean
    public S3Presigner s3Presigner(@Value("${cloud.aws.s3.endpoint:#{null}}") String endpoint) {
        S3Presigner.Builder builder = S3Presigner.builder()
                .region(Region.of(region))
                .credentialsProvider(DefaultCredentialsProvider.create());

        if (endpoint != null && !endpoint.trim().isEmpty() && !"null".equalsIgnoreCase(endpoint.trim())) {
            try {
                builder.endpointOverride(URI.create(endpoint));
            } catch (Exception e) {
                throw new IllegalArgumentException("Invalid S3 endpoint: " + endpoint, e);
            }
        }

        return builder.build();
    }

}
