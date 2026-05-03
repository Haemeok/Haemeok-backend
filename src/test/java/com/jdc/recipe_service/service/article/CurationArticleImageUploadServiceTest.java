package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.exception.ArticleImagesNotReadyException;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
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

    @InjectMocks private CurationArticleImageUploadService imageUploadService;

    @Test
    @DisplayName("article이 존재하면 articleId path를 포함한 uploadKey/imageKey가 생성되고 S3 presigned URL이 uploadKey에 발급된다")
    void issuesPresignedUrlWithArticleIdPath() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.createPresignedUrl(anyString(), anyString())).willReturn("https://s3.test/upload");

        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg")
                .fileSize(245_678L)
                .build();

        ArticleImagePresignedUrlResponse resp = imageUploadService.issuePresignedUrl(articleId, req);

        // path에 articleId가 박힘 + ext가 contentType 매핑(jpeg→jpg) + 변환 결과는 webp 고정
        assertThat(resp.getUploadKey())
                .startsWith("original/images/articles/42/")
                .endsWith(".jpg");
        assertThat(resp.getImageKey())
                .startsWith("images/articles/42/")
                .endsWith(".webp");
        // upload/image key UUID 동일성 — 서로 prefix만 다른 같은 자원이어야 한다
        String uploadUuid = resp.getUploadKey().substring(
                "original/images/articles/42/".length(),
                resp.getUploadKey().length() - ".jpg".length());
        String imageUuid = resp.getImageKey().substring(
                "images/articles/42/".length(),
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
    @DisplayName("finalize: 모든 imageKey가 S3에 존재하면 ready=true 응답")
    void finalize_allReady() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.isObjectPresent("images/articles/42/abc.webp")).willReturn(true);
        given(s3Util.isObjectPresent("images/articles/42/def.webp")).willReturn(true);

        ArticleImageFinalizeResponse resp = imageUploadService.finalizeImages(
                articleId, List.of("images/articles/42/abc.webp", "images/articles/42/def.webp"));

        assertThat(resp.isReady()).isTrue();
        assertThat(resp.getImageKeys())
                .containsExactlyInAnyOrder("images/articles/42/abc.webp", "images/articles/42/def.webp");
    }

    @Test
    @DisplayName("finalize: 일부 imageKey가 S3에 없으면 ArticleImagesNotReadyException(missing/present 분리)")
    void finalize_partialMissingThrows() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);
        given(s3Util.isObjectPresent("images/articles/42/abc.webp")).willReturn(true);
        given(s3Util.isObjectPresent("images/articles/42/def.webp")).willReturn(false);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/42/abc.webp", "images/articles/42/def.webp")))
                .isInstanceOf(ArticleImagesNotReadyException.class)
                .satisfies(ex -> {
                    ArticleImagesNotReadyException notReady = (ArticleImagesNotReadyException) ex;
                    assertThat(notReady.getMissingKeys()).containsExactly("images/articles/42/def.webp");
                    assertThat(notReady.getPresentKeys()).containsExactly("images/articles/42/abc.webp");
                });
    }

    @Test
    @DisplayName("finalize: imageKey가 다른 article prefix를 갖고 있으면 INVALID_INPUT_VALUE — S3 HEAD 호출되지 않음")
    void finalize_wrongPrefixThrows() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);

        // articleId=42인데 키는 articleId=99의 것
        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/99/abc.webp")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("finalize: imageKey가 .webp가 아니면 INVALID_INPUT_VALUE")
    void finalize_nonWebpExtensionThrows() {
        long articleId = 42L;
        given(articleRepo.existsById(articleId)).willReturn(true);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                articleId, List.of("images/articles/42/abc.jpg")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(s3Util, never()).isObjectPresent(anyString());
    }

    @Test
    @DisplayName("finalize: article이 없으면 ARTICLE_NOT_FOUND. 형식 검증/S3 호출은 일어나지 않는다")
    void finalize_articleMissingThrows() {
        given(articleRepo.existsById(999L)).willReturn(false);

        assertThatThrownBy(() -> imageUploadService.finalizeImages(
                999L, List.of("images/articles/999/abc.webp")))
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
