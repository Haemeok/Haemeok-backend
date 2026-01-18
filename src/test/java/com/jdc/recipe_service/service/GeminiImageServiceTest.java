package com.jdc.recipe_service.service;

import com.jdc.recipe_service.service.image.GeminiImageService;
import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SpringBootTest(classes = {GeminiImageService.class, GeminiImageServiceTest.RetryTestConfig.class})
class GeminiImageServiceTest {

    @Configuration
    @EnableRetry
    static class RetryTestConfig { }

    @Autowired
    private GeminiImageService geminiImageService;

    @MockBean
    private RestTemplate restTemplate;

    @MockBean
    private S3Util s3Util;

    @Test
    @DisplayName("이미지 API 호출 실패 시 최대 3번(최초1+재시도2) 시도해야 한다")
    void generateImageUrls_retriesOnFailure() {
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(Map.class)))
                .thenThrow(new RestClientException("API Error"));

        List<String> result = geminiImageService.generateImageUrls("prompt", 1L, 1L);

        // maxAttempts=3 이므로 총 3회 호출
        verify(restTemplate, times(3))
                .postForEntity(anyString(), any(HttpEntity.class), eq(Map.class));

        assertThat(result).hasSize(1);
        assertThat(result.get(0)).contains("no_image.webp");

        // (권장) Retry가 AOP 프록시로 실제 동작하는지 sanity check
        assertThat(AopUtils.isAopProxy(geminiImageService)).isTrue();
    }

    @Test
    @DisplayName("재시도까지 모두 실패하면 기본 이미지(Fallback)를 반환해야 한다")
    void generateImageUrls_recoversToDefaultImage() {
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(Map.class)))
                .thenThrow(new RestClientException("Persistent Error"));

        List<String> result = geminiImageService.generateImageUrls("prompt", 1L, 1L);

        assertThat(result).isNotEmpty();
        assertThat(result.get(0)).isEqualTo(
                "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp"
        );

        verify(restTemplate, times(3))
                .postForEntity(anyString(), any(HttpEntity.class), eq(Map.class));
    }
}
