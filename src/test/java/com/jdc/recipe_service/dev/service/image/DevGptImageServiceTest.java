package com.jdc.recipe_service.dev.service.image;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.Base64;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevGptImageService unit tests.
 *
 * 검증 포인트:
 *  1. (MUST) request body의 model/quality/size가 호출자가 넘긴 값 그대로 전송되는지 — DB와 실제 호출 모델 일치성 보장.
 *  2. 401/403 → CustomException으로 시끄럽게 throw (silent fallback 금지)
 *  3. 429 → DEFAULT_IMAGE_URL fallback (graceful degrade)
 *  4. 5xx → 다시 throw (Retryable 동작 유도)
 *  5. 응답 파싱 실패 / S3 업로드 실패 → DEFAULT_IMAGE_URL fallback (retry 가치 없는 실패)
 *  6. API key 미설정 → 즉시 fallback, HTTP 호출 안 함
 */
@ExtendWith(MockitoExtension.class)
class DevGptImageServiceTest {

    @Mock RestTemplate restTemplate;
    @Mock S3Util s3Util;

    DevGptImageService service;

    private static final String PROMPT = "kimchi stew";
    private static final String API_MODEL = "gpt-image-2";
    private static final String QUALITY = "low";
    private static final Long USER_ID = 42L;
    private static final Long RECIPE_ID = 7L;
    private static final String ENDPOINT = "https://test/v1/images";
    private static final String BUCKET = "test-bucket";
    private static final String REGION = "ap-northeast-2";
    private static final String DEFAULT_IMAGE_URL =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/icons/no_image.webp";

    @BeforeEach
    void setUp() {
        service = new DevGptImageService(restTemplate, s3Util);
        ReflectionTestUtils.setField(service, "openAiApiKey", "sk-test");
        ReflectionTestUtils.setField(service, "openAiImageSize", "1024x1024");
        ReflectionTestUtils.setField(service, "openAiImageEndpoint", ENDPOINT);
        ReflectionTestUtils.setField(service, "bucketName", BUCKET);
        ReflectionTestUtils.setField(service, "region", REGION);
    }

    private ResponseEntity<Map> okResponse(String b64) {
        Map<String, Object> body = Map.of("data", List.of(Map.of("b64_json", b64)));
        return new ResponseEntity<>(body, HttpStatus.OK);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void givenOpenAiReturns(ResponseEntity<Map> response) {
        given(restTemplate.postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class)))
                .willReturn(response);
    }

    @SuppressWarnings("rawtypes")
    private void givenOpenAiThrows(RuntimeException ex) {
        given(restTemplate.postForEntity(eq(ENDPOINT), any(HttpEntity.class), eq(Map.class)))
                .willThrow(ex);
    }

    // --- (MUST) 1. request body 컨트랙트 ---

    @Test
    @DisplayName("호출자가 넘긴 apiModel/quality/size가 OpenAI request body에 그대로 들어간다")
    @SuppressWarnings({"unchecked", "rawtypes"})
    void requestBodyContainsCorrectModelQualitySize() {
        // given
        String b64 = Base64.getEncoder().encodeToString("img".getBytes());
        givenOpenAiReturns(okResponse(b64));

        // when
        service.generateImageUrls(PROMPT, "gpt-image-2", "high", USER_ID, RECIPE_ID);

        // then - request body 캡처해서 검증
        ArgumentCaptor<HttpEntity> captor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate).postForEntity(eq(ENDPOINT), captor.capture(), eq(Map.class));

        Map<String, Object> body = (Map<String, Object>) captor.getValue().getBody();
        assertThat(body)
                .containsEntry("model", "gpt-image-2")
                .containsEntry("quality", "high")
                .containsEntry("size", "1024x1024")
                .containsEntry("prompt", PROMPT)
                .containsEntry("n", 1);

        // Authorization 헤더 (Bearer)
        assertThat(captor.getValue().getHeaders().getFirst("Authorization"))
                .isEqualTo("Bearer sk-test");
    }

    // --- 2. happy path ---

    @Test
    @DisplayName("성공 시 b64를 디코드해 S3에 업로드하고 webp URL 반환")
    void happyPath_uploadsAndReturnsWebpUrl() {
        // given
        String b64 = Base64.getEncoder().encodeToString("img-bytes".getBytes());
        givenOpenAiReturns(okResponse(b64));

        // when
        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        // then
        String expected = String.format("https://%s.s3.%s.amazonaws.com/images/recipes/%d/%d/main.webp",
                BUCKET, REGION, USER_ID, RECIPE_ID);
        assertThat(result).containsExactly(expected);

        // S3에는 original/...png 키로 업로드되었어야 함
        verify(s3Util).upload(any(byte[].class),
                eq(String.format("original/images/recipes/%d/%d/main.png", USER_ID, RECIPE_ID)),
                eq("image/png"));
    }

    // --- 3. API key 미설정 ---

    @Test
    @DisplayName("API key 미설정 시 HTTP 호출 없이 즉시 DEFAULT_IMAGE_URL")
    void noApiKey_returnsDefaultWithoutHttp() {
        // given
        ReflectionTestUtils.setField(service, "openAiApiKey", "");

        // when
        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        // then
        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
        verify(restTemplate, never()).postForEntity(any(String.class), any(), any(Class.class));
        verify(s3Util, never()).upload(any(), any(), any());
    }

    // --- 4. 4xx 분기 ---

    @Test
    @DisplayName("401 → CustomException(INTERNAL_SERVER_ERROR)으로 시끄럽게 실패 (설정 오류 알아채야 함)")
    void unauthorized401_throwsConfigError() {
        givenOpenAiThrows(HttpClientErrorException.create(
                HttpStatus.UNAUTHORIZED, "Unauthorized", null, null, null));

        assertThatThrownBy(() ->
                service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INTERNAL_SERVER_ERROR);

        verify(s3Util, never()).upload(any(), any(), any());
    }

    @Test
    @DisplayName("403 → CustomException (조직 verification 등 권한 문제)")
    void forbidden403_throwsConfigError() {
        givenOpenAiThrows(HttpClientErrorException.create(
                HttpStatus.FORBIDDEN, "Forbidden", null, null, null));

        assertThatThrownBy(() ->
                service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INTERNAL_SERVER_ERROR);
    }

    @Test
    @DisplayName("429 rate limited → DEFAULT_IMAGE_URL graceful degrade")
    void rateLimited429_returnsDefault() {
        givenOpenAiThrows(HttpClientErrorException.create(
                HttpStatus.TOO_MANY_REQUESTS, "Too Many Requests", null, null, null));

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
    }

    @Test
    @DisplayName("400 bad request → DEFAULT_IMAGE_URL (입력 문제로 추정, retry 가치 없음)")
    void badRequest400_returnsDefault() {
        givenOpenAiThrows(HttpClientErrorException.create(
                HttpStatus.BAD_REQUEST, "Bad Request", null, null, null));

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
    }

    // --- 5. 5xx 분기 ---

    @Test
    @DisplayName("5xx → HttpServerErrorException 다시 throw (Retryable이 잡아 재시도)")
    void serverError5xx_propagatesForRetry() {
        givenOpenAiThrows(HttpServerErrorException.create(
                HttpStatus.INTERNAL_SERVER_ERROR, "Server Error", null, null, null));

        assertThatThrownBy(() ->
                service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID))
                .isInstanceOf(HttpServerErrorException.class);
    }

    // --- 6. 응답 파싱 실패 ---

    @Test
    @DisplayName("응답에 data 키 자체가 없으면 DEFAULT_IMAGE_URL")
    void missingDataKey_returnsDefault() {
        ResponseEntity<Map> resp = new ResponseEntity<>(Map.of("created", 1234), HttpStatus.OK);
        givenOpenAiReturns(resp);

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
        verify(s3Util, never()).upload(any(), any(), any());
    }

    @Test
    @DisplayName("data 배열이 비어 있으면 DEFAULT_IMAGE_URL")
    void emptyDataArray_returnsDefault() {
        ResponseEntity<Map> resp = new ResponseEntity<>(Map.of("data", List.of()), HttpStatus.OK);
        givenOpenAiReturns(resp);

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
        verify(s3Util, never()).upload(any(), any(), any());
    }

    @Test
    @DisplayName("b64_json이 String이 아니면 DEFAULT_IMAGE_URL (ClassCast 차단)")
    void b64JsonNotString_returnsDefault() {
        Map<String, Object> body = Map.of("data", List.of(Map.of("b64_json", 12345)));
        givenOpenAiReturns(new ResponseEntity<>(body, HttpStatus.OK));

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
        verify(s3Util, never()).upload(any(), any(), any());
    }

    // --- 7. S3 업로드 실패 ---

    @Test
    @DisplayName("S3 업로드 실패 시 DEFAULT_IMAGE_URL (외부 API 성공해도 인프라 문제로 fallback)")
    void s3UploadFailure_returnsDefault() {
        String b64 = Base64.getEncoder().encodeToString("img".getBytes());
        givenOpenAiReturns(okResponse(b64));
        doThrow(new RuntimeException("S3 down")).when(s3Util).upload(any(), any(), any());

        List<String> result = service.generateImageUrls(PROMPT, API_MODEL, QUALITY, USER_ID, RECIPE_ID);

        assertThat(result).containsExactly(DEFAULT_IMAGE_URL);
    }
}
