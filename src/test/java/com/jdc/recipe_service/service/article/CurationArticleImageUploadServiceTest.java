package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.exception.ArticleImagesNotReadyException;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.S3Util;
import org.hashids.Hashids;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CurationArticleImageUploadServiceTest {

    @Mock private S3Util s3Util;
    @Mock private CurationArticleRepository articleRepo;

    // 실 Hashids 인스턴스 사용 — 같은 인스턴스로 expected value 동적 생성해 salt 변경에도 회귀 안 깨짐.
    // HashIdConfig가 사용하는 정적 인스턴스도 함께 초기화해 service 내부의 hashids.encode가 동일 결과를 내게 한다.
    private final Hashids hashids = new Hashids("TEST_SALT_FOR_IMAGE_UPLOAD_SERVICE", 8);

    private CurationArticleImageUploadService imageUploadService;

    @BeforeEach
    void setUp() {
        imageUploadService = new CurationArticleImageUploadService(s3Util, articleRepo, hashids);
    }

    @Test
    @DisplayName("article이 존재하면 articleHashId path를 포함한 uploadKey/imageKey가 생성되고 S3 presigned URL이 uploadKey에 발급된다")
    void issuesPresignedUrlWithArticleHashIdPath() {
        long articleId = 42L;
        String articleHashId = hashids.encode(articleId);
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.createPresignedUrl(anyString(), anyString())).willReturn("https://s3.test/upload");

        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg")
                .fileSize(245_678L)
                .build();

        ArticleImagePresignedUrlResponse resp = imageUploadService.issuePresignedUrl(articleId, req);

        // path segment에 raw Long(42)이 아니라 HashID 문자열이 박혀야 한다 — wire에 raw Long 노출 금지 정책
        String expectedUploadPrefix = "original/images/articles/" + articleHashId + "/";
        String expectedImagePrefix = "images/articles/" + articleHashId + "/";
        assertThat(resp.getUploadKey()).startsWith(expectedUploadPrefix).endsWith(".jpg");
        assertThat(resp.getImageKey()).startsWith(expectedImagePrefix).endsWith(".webp");
        // raw Long segment("/42/")가 절대 들어가서는 안 된다
        assertThat(resp.getUploadKey()).doesNotContain("/42/");
        assertThat(resp.getImageKey()).doesNotContain("/42/");

        // upload/image key UUID 동일성 — 서로 prefix만 다른 같은 자원이어야 한다
        String uploadUuid = resp.getUploadKey().substring(
                expectedUploadPrefix.length(),
                resp.getUploadKey().length() - ".jpg".length());
        String imageUuid = resp.getImageKey().substring(
                expectedImagePrefix.length(),
                resp.getImageKey().length() - ".webp".length());
        assertThat(uploadUuid).isEqualTo(imageUuid);

        // S3 presigned URL은 uploadKey에 대해 contentType과 함께 호출되어야 한다
        ArgumentCaptor<String> keyCap = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String> ctCap = ArgumentCaptor.forClass(String.class);
        verify(s3Util).createPresignedUrl(keyCap.capture(), ctCap.capture());
        assertThat(keyCap.getValue()).isEqualTo(resp.getUploadKey());
        assertThat(ctCap.getValue()).isEqualTo("image/jpeg");
        assertThat(resp.getPresignedUrl()).isEqualTo("https://s3.test/upload");
    }

    @Test
    @DisplayName("article이 없으면 ARTICLE_NOT_FOUND. S3 호출은 일어나지 않는다")
    void throwsWhenArticleMissing() {
        given(articleRepo.existsById(999L)).willReturn(false);

        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg").fileSize(1L).build();

        assertThatThrownBy(() -> imageUploadService.issuePresignedUrl(999L, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_NOT_FOUND);

        verify(s3Util, never()).createPresignedUrl(anyString(), anyString());
    }

    @Test
    @DisplayName("허용되지 않은 contentType이면 ARTICLE_IMAGE_INVALID_CONTENT_TYPE")
    void throwsWhenContentTypeInvalid() {
        given(articleRepo.existsById(1L)).willReturn(true);

        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/gif")
                .fileSize(1L)
                .build();

        assertThatThrownBy(() -> imageUploadService.issuePresignedUrl(1L, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_IMAGE_INVALID_CONTENT_TYPE);

        verify(s3Util, never()).createPresignedUrl(anyString(), anyString());
    }

    // ── finalizeImages ──

    @Test
    @DisplayName("finalize: 모든 imageKey가 S3에 존재하면 ready=true 응답 (HashID prefix 검증)")
    void finalize_allReady() {
        long articleId = 42L;
        String articleHashId = hashids.encode(articleId);
        String key1 = "images/articles/" + articleHashId + "/abc.webp";
        String key2 = "images/articles/" + articleHashId + "/def.webp";
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.isObjectPresent(key1)).willReturn(true);
        given(s3Util.isObjectPresent(key2)).willReturn(true);

        ArticleImageFinalizeResponse resp = imageUploadService.finalizeImages(
                articleId, List.of(key1, key2));

        assertThat(resp.isReady()).isTrue();
        assertThat(resp.getImageKeys()).containsExactlyInAnyOrder(key1, key2);
    }

    @Test
    @DisplayName("finalize: 일부 imageKey가 S3에 없으면 ArticleImagesNotReadyException(missing/present 분리)")
    void finalize_partialMissingThrows() {
        long articleId = 42L;
        String articleHashId = hashids.encode(articleId);
        String present = "images/articles/" + articleHashId + "/abc.webp";
        String missing = "images/articles/" + articleHashId + "/def.webp";
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.isObjectPresent(present)).willReturn(true);
        given(s3Util.isObjectPresent(missing)).willReturn(false);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of(present, missing)))
                .isInstanceOf(ArticleImagesNotReadyException.class)
                .satisfies(ex -> {
                    ArticleImagesNotReadyException notReady = (ArticleImagesNotReadyException) ex;
                    assertThat(notReady.getMissingKeys()).containsExactly(missing);
                    assertThat(notReady.getPresentKeys()).containsExactly(present);
                });
    }

    @Test
    @DisplayName("finalize: imageKey가 다른 article의 HashID prefix를 갖고 있으면 INVALID_INPUT_VALUE — S3 HEAD 호출 0")
    void finalize_wrongPrefixThrows() {
        long articleId = 42L;
        String otherHashId = hashids.encode(99L);  // 다른 article의 HashID 사칭 시도
        given(articleRepo.existsById(articleId)).willReturn(true);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/" + otherHashId + "/abc.webp")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("finalize: 키에 raw Long segment(`/42/`)가 들어 있으면 거부 — HashID prefix와 다르므로 INVALID_INPUT_VALUE")
    void finalize_rawLongSegmentRejected() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);

        // raw Long을 segment로 박은 키는 더 이상 허용되지 않는다 (HashID 정책 전환)
        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/42/abc.webp")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("finalize: imageKey가 .webp가 아니면 INVALID_INPUT_VALUE")
    void finalize_nonWebpExtensionThrows() {
        long articleId = 42L;
        String articleHashId = hashids.encode(articleId);
        given(articleRepo.existsById(articleId)).willReturn(true);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/" + articleHashId + "/abc.jpg")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("finalize: article이 없으면 ARTICLE_NOT_FOUND. 형식 검증/S3 호출은 일어나지 않는다")
    void finalize_articleMissingThrows() {
        long missingArticleId = 999L;
        String missingHashId = hashids.encode(missingArticleId);
        given(articleRepo.existsById(missingArticleId)).willReturn(false);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                missingArticleId, List.of("images/articles/" + missingHashId + "/abc.webp")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_NOT_FOUND);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("fileSize가 10MB를 초과하면 ARTICLE_IMAGE_TOO_LARGE")
    void throwsWhenFileSizeTooLarge() {
        given(articleRepo.existsById(1L)).willReturn(true);

        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg")
                .fileSize(10L * 1024 * 1024 + 1)  // 10MB + 1 byte
                .build();

        assertThatThrownBy(() -> imageUploadService.issuePresignedUrl(1L, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_IMAGE_TOO_LARGE);

        verify(s3Util, never()).createPresignedUrl(anyString(), anyString());
    }
}
