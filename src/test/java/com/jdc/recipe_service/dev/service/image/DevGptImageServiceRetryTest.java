package com.jdc.recipe_service.dev.service.image;

import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.Base64;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * DevGptImageService의 @Retryable 동작을 Spring AOP proxy로 검증한다.
 *
 * 일반 Mockito 단위 테스트(DevGptImageServiceTest)는 service 인스턴스를 직접 호출하므로
 * @Retryable proxy가 적용되지 않는다. 5xx 재시도와 @Recover fallback은 반드시 proxy를 거쳐야
 * 동작하므로 별도 Spring context 기반 테스트로 분리.
 */
@SpringJUnitConfig(DevGptImageServiceRetryTest.RetryTestConfig.class)
@TestPropertySource(properties = {
        "app.openai.api-key=sk-test",
        "app.openai.image.size=1024x1024",
        "app.openai.image.endpoint=https://test/v1/images",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevGptImageServiceRetryTest {

    @Configuration
    @EnableRetry
    static class RetryTestConfig {
        @Bean
        RestTemplate restTemplate() { return Mockito.mock(RestTemplate.class); }

        @Bean
        S3Util s3Util() { return Mockito.mock(S3Util.class); }

        @Bean
        DevGptImageService devGptImageService(RestTemplate rt, S3Util s3) {
            return new DevGptImageService(rt, s3);
        }
    }

    @Autowired DevGptImageService service;
    @Autowired RestTemplate restTemplate;
    @Autowired S3Util s3Util;

    private static final String ENDPOINT = "https://test/v1/images";
    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    @BeforeEach
    void resetMocks() {
        Mockito.reset(restTemplate, s3Util);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private ResponseEntity<Map> okResponse() {
        String b64 = Base64.getEncoder().encodeToString("img".getBytes());
        return new ResponseEntity<>(Map.of("data", List.of(Map.of("b64_json", b64))), HttpStatus.OK);
    }

    @Test
    @DisplayName("5xx 첫 호출 실패 → @Retryable이 1회 재시도 → 두 번째 호출 성공 → 정상 webp URL 반환")
    @SuppressWarnings("rawtypes")
    void retriesOn5xxAndSucceeds() {
        // given - 첫 호출 500, 두 번째 호출 200
        given(restTemplate.postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class)))
                .willThrow(HttpServerErrorException.create(
                        HttpStatus.INTERNAL_SERVER_ERROR, "boom", null, null, null))
                .willReturn(okResponse());

        // when
        List<String> result = service.generateImageUrls("p", "gpt-image-2", "low", 1L, 2L);

        // then - 재시도가 동작했으면 두 번째 호출은 성공해서 default가 아닌 실제 webp URL이 나와야 함
        assertThat(result).hasSize(1);
        assertThat(result.get(0)).contains("images/recipes/1/2/main.webp");
        assertThat(result.get(0)).isNotEqualTo(DEFAULT_IMAGE_URL);

        // restTemplate은 정확히 2번 호출되었어야 함 (첫 실패 + 재시도)
        verify(restTemplate, times(2))
                .postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class));
        verify(s3Util, times(1)).upload(any(byte[].class), any(String.class), eq("image/png"));
    }

    @Test
    @DisplayName("5xx 2회 모두 실패 → @Recover fallback → DEFAULT_IMAGE_URL 반환, S3 호출 안 함")
    @SuppressWarnings("rawtypes")
    void exhaustsRetryAndRecovers() {
        // given - 모든 호출 500
        given(restTemplate.postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class)))
                .willThrow(HttpServerErrorException.create(
                        HttpStatus.INTERNAL_SERVER_ERROR, "boom", null, null, null));

        // when
        List<String> result = service.generateImageUrls("p", "gpt-image-2", "low", 1L, 2L);

        // then - 재시도 소진 후 @Recover가 DEFAULT_IMAGE_URL 반환
        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);

        // 정확히 maxAttempts(2)회 호출 (첫 + 재시도 1)
        verify(restTemplate, times(2))
                .postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class));
        // S3 업로드는 한 번도 호출되지 않아야 함
        verify(s3Util, Mockito.never()).upload(any(), any(), any());
    }
}
