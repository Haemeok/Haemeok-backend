package com.jdc.recipe_service.service;

import com.jdc.recipe_service.service.image.GeminiImageService;
import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class GeminiImageServiceTest {

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private S3Util s3Util;

    @InjectMocks
    private GeminiImageService geminiImageService;

    @BeforeEach
    void setUp() {
        List<String> locations = Arrays.asList("global", "us-central1", "us-east1");
        // 하나의 credential만 사용해 shuffle 랜덤성을 제거한다.
        List<String> rawCredentials = List.of("test-project:TEST_KEY");

        ReflectionTestUtils.setField(geminiImageService, "vertexLocations", locations);
        ReflectionTestUtils.setField(geminiImageService, "rawCredentials", rawCredentials);
        ReflectionTestUtils.setField(geminiImageService, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(geminiImageService, "region", "us-east-1");
        ReflectionTestUtils.setField(geminiImageService, "cooldownMs", 0L);
        geminiImageService.init();
    }

    @Test
    @DisplayName("Failover: Global 429 발생 시 us-central1으로 넘어가 성공한다")
    void testFailoverOn429() {
        given(restTemplate.postForEntity(contains("/locations/global/"), any(), eq(Map.class)))
                .willThrow(new HttpClientErrorException(HttpStatus.TOO_MANY_REQUESTS));

        given(restTemplate.postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class)))
                .willReturn(ResponseEntity.ok(createMockResponse()));

        List<String> result = geminiImageService.generateImageUrls("test prompt", 1L, 100L);

        verify(restTemplate, times(1)).postForEntity(contains("/locations/global/"), any(), eq(Map.class));
        verify(restTemplate, times(1)).postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class));
        verify(restTemplate, never()).postForEntity(contains("/locations/us-east1/"), any(), eq(Map.class));
        verify(s3Util, times(1)).upload(any(), any(), any());

        assertThat(result.get(0)).contains("test-bucket.s3.us-east-1.amazonaws.com");
    }

    @Test
    @DisplayName("네트워크 오류(Timeout) 발생 시에도 다음 리전으로 넘어간다")
    void testFailoverOnNetworkError() {
        given(restTemplate.postForEntity(contains("/locations/global/"), any(), eq(Map.class)))
                .willThrow(new ResourceAccessException("Connection timed out"));

        given(restTemplate.postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class)))
                .willReturn(ResponseEntity.ok(createMockResponse()));

        geminiImageService.generateImageUrls("prompt", 1L, 1L);

        verify(restTemplate, times(1)).postForEntity(contains("/locations/global/"), any(), eq(Map.class));
        verify(restTemplate, times(1)).postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class));
        verify(s3Util, times(1)).upload(any(), any(), any());
    }

    @Test
    @DisplayName("모든 리전이 5xx로 실패하면 safe-prompt 재시도 후 기본 이미지 URL을 반환한다")
    void allRegionsFail_returnsDefaultImage() {
        given(restTemplate.postForEntity(anyString(), any(), eq(Map.class)))
                .willThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        List<String> result = geminiImageService.generateImageUrls("prompt", 1L, 1L);

        // 1차 호출: 3 regions + safe-prompt 재시도: 3 regions = 총 6
        verify(restTemplate, times(6)).postForEntity(anyString(), any(), eq(Map.class));
        verify(s3Util, never()).upload(any(), any(), any());
        assertThat(result).hasSize(1);
        assertThat(result.get(0)).contains("no_image.webp");
    }

    @Test
    @DisplayName("@Recover 메서드는 기본 이미지(Default URL)를 반환한다")
    void recover_returnsDefaultImage() {
        List<String> result = geminiImageService.recover(
                new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR),
                "prompt", 1L, 1L
        );

        assertThat(result).isNotEmpty();
        assertThat(result.get(0)).contains("no_image.webp");
    }

    private Map<String, Object> createMockResponse() {
        String validBase64 = "R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7";
        return Map.of(
                "candidates", List.of(
                        Map.of("content", Map.of("parts", List.of(Map.of("inlineData", Map.of("data", validBase64)))))
                )
        );
    }
}
