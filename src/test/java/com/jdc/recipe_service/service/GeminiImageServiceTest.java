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
import static org.mockito.Mockito.*;

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

        ReflectionTestUtils.setField(geminiImageService, "vertexLocations", locations);
        ReflectionTestUtils.setField(geminiImageService, "geminiApiKey", "TEST_KEY");
        ReflectionTestUtils.setField(geminiImageService, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(geminiImageService, "region", "us-east-1");
        ReflectionTestUtils.setField(geminiImageService, "cooldownMs", 0L);
    }

    @Test
    @DisplayName("Failover 테스트: Global 429 에러 시 -> US-Central1으로 우회하여 성공해야 한다")
    void testFailoverLogic() {
        when(restTemplate.postForEntity(contains("/locations/global/"), any(), eq(Map.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.TOO_MANY_REQUESTS));

        Map<String, Object> successResponse = createMockResponse();
        when(restTemplate.postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class)))
                .thenReturn(ResponseEntity.ok(successResponse));

        List<String> result = geminiImageService.generateImageUrls("test prompt", 1L, 100L);

        verify(restTemplate, times(1)).postForEntity(contains("/locations/global/"), any(), eq(Map.class));

        verify(restTemplate, times(1)).postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class));

        verify(restTemplate, never()).postForEntity(contains("/locations/us-east1/"), any(), eq(Map.class));

        verify(s3Util, times(1)).upload(any(), any(), any());

        assertThat(result.get(0)).contains("test-bucket.s3.us-east-1.amazonaws.com");
    }

    @Test
    @DisplayName("Failover 실패 테스트: 모든 리전이 실패하면 예외가 발생해야 한다 (Unit Test엔 AOP 없음)")
    void generateImageUrls_throwsException_whenAllFail() {
        when(restTemplate.postForEntity(anyString(), any(), eq(Map.class)))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        org.junit.jupiter.api.Assertions.assertThrows(Exception.class, () -> {
            geminiImageService.generateImageUrls("prompt", 1L, 1L);
        });

        verify(restTemplate, times(3)).postForEntity(anyString(), any(), eq(Map.class));
    }

    @Test
    @DisplayName("Recover 테스트: recover 메서드 호출 시 기본 이미지(Default URL)를 반환해야 한다")
    void recover_returnsDefaultImage() {
        List<String> result = geminiImageService.recover(
                new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR),
                "prompt", 1L, 1L
        );

        assertThat(result).isNotEmpty();
        assertThat(result.get(0)).contains("no_image.webp");
    }

    @Test
    @DisplayName("네트워크 오류(Timeout) 발생 시에도 다음 리전으로 넘어가야 한다")
    void testFailoverOnNetworkError() {

        when(restTemplate.postForEntity(contains("/locations/global/"), any(), eq(Map.class)))
                .thenThrow(new ResourceAccessException("Connection timed out"));

        Map<String, Object> successResponse = createMockResponse();
        when(restTemplate.postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class)))
                .thenReturn(ResponseEntity.ok(successResponse));

        geminiImageService.generateImageUrls("prompt", 1L, 1L);

        verify(restTemplate, times(1)).postForEntity(contains("/locations/global/"), any(), eq(Map.class));
        verify(restTemplate, times(1)).postForEntity(contains("/locations/us-central1/"), any(), eq(Map.class));
        verify(s3Util, times(1)).upload(any(), any(), any());
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