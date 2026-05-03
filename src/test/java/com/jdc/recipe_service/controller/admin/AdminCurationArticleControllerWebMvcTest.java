package com.jdc.recipe_service.controller.admin;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateRequest;
import com.jdc.recipe_service.exception.ArticleImagesNotReadyException;
import com.jdc.recipe_service.handler.GlobalExceptionHandler;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.article.CurationArticleImageUploadService;
import com.jdc.recipe_service.service.article.CurationArticleService;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * AdminCurationArticleController @WebMvcTest.
 *
 * <p>@EnableMethodSecurity를 TestConfiguration에 명시해 클래스 레벨 @PreAuthorize가 실제로 enforced되도록
 * 한다 (WebMvcTest는 SecurityConfig를 로드하지 않으므로 method security가 자동 활성화되지 않는다).
 * URL 패턴 매처 기반 보안 (/api/admin/** ROLE_ADMIN)은 SecurityConfig 정합성 회귀로 별도 통합 테스트가 다룬다.
 */
@WebMvcTest(controllers = AdminCurationArticleController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({GlobalExceptionHandler.class, AdminCurationArticleControllerWebMvcTest.MethodSecurityTestConfig.class})
class AdminCurationArticleControllerWebMvcTest {

    @TestConfiguration
    @EnableMethodSecurity
    static class MethodSecurityTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired ObjectMapper objectMapper;

    @MockBean CurationArticleService articleService;
    @MockBean CurationArticleImageUploadService imageUploadService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    @BeforeEach
    void setUpAdminAuth() {
        User admin = User.builder()
                .nickname("admin")
                .provider("test")
                .oauthId("admin-oid")
                .role(Role.ADMIN)
                .build();
        ReflectionTestUtils.setField(admin, "id", 1L);
        CustomUserDetails principal = new CustomUserDetails(admin);
        UsernamePasswordAuthenticationToken auth =
                new UsernamePasswordAuthenticationToken(principal, null, principal.getAuthorities());
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @AfterEach
    void clearAuth() {
        SecurityContextHolder.clearContext();
    }

    @Test
    @DisplayName("POST /api/admin/curation-articles: 정상 요청 → 201 + articleId 응답")
    void create_ok() throws Exception {
        CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                .slug("summer-diet")
                .title("여름 다이어트")
                .contentMdx("# body")
                .build();
        given(articleService.create(any(CurationArticleCreateRequest.class))).willReturn(42L);

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.articleId").value(42));

        verify(articleService).create(any(CurationArticleCreateRequest.class));
    }

    @Test
    @DisplayName("POST /api/admin/curation-articles: slug 패턴 위반 → 400 (service 호출 안 됨)")
    void create_invalidSlugPattern_returns400() throws Exception {
        // slug에 대문자 포함 — 패턴 위반
        CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                .slug("Summer-Diet")
                .title("t")
                .contentMdx("c")
                .build();

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isBadRequest());

        verify(articleService, never()).create(any());
    }

    @Test
    @DisplayName("POST /api/admin/curation-articles: 필수 필드 누락(contentMdx 빈 값) → 400")
    void create_blankContentMdx_returns400() throws Exception {
        CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                .slug("ok-slug")
                .title("t")
                .contentMdx("")
                .build();

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isBadRequest());

        verify(articleService, never()).create(any());
    }

    @Test
    @DisplayName("POST /{articleId}/images/presigned-urls: articleId path가 service로 전달되고 응답 3-필드가 그대로 내려간다")
    void issuePresignedUrl_ok() throws Exception {
        long articleId = 123L;
        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg")
                .fileSize(123_456L)
                .build();
        given(imageUploadService.issuePresignedUrl(eq(articleId), any(ArticleImagePresignedUrlRequest.class)))
                .willReturn(ArticleImagePresignedUrlResponse.builder()
                        .uploadKey("original/images/articles/123/uuid.jpg")
                        .imageKey("images/articles/123/uuid.webp")
                        .presignedUrl("https://s3.test/uuid")
                        .build());

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/presigned-urls", articleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.uploadKey").value("original/images/articles/123/uuid.jpg"))
                .andExpect(jsonPath("$.imageKey").value("images/articles/123/uuid.webp"))
                .andExpect(jsonPath("$.presignedUrl").value("https://s3.test/uuid"));

        verify(imageUploadService).issuePresignedUrl(eq(articleId), any(ArticleImagePresignedUrlRequest.class));
    }

    @Test
    @DisplayName("POST /{articleId}/images/presigned-urls: fileSize 누락 → 400 (service 미호출)")
    void issuePresignedUrl_missingFileSize_returns400() throws Exception {
        // fileSize 없이 contentType만
        String body = "{\"contentType\":\"image/webp\"}";

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/presigned-urls", 1L)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isBadRequest());

        verify(imageUploadService, never()).issuePresignedUrl(any(), any());
    }

    @Test
    @DisplayName("POST /{articleId}/publish: path variable로 service에 raw Long 전달")
    void publish_callsService() throws Exception {
        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/publish", 42L))
                .andExpect(status().isOk());

        verify(articleService).publish(eq(42L));
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: 모든 imageKey가 ready면 200 + ready=true 응답 + service 호출 인자 검증")
    void finalize_ok() throws Exception {
        long articleId = 42L;
        ArticleImageFinalizeRequest req = ArticleImageFinalizeRequest.builder()
                .imageKeys(java.util.List.of("images/articles/42/abc.webp"))
                .build();
        given(imageUploadService.finalizeImages(eq(articleId), any()))
                .willReturn(ArticleImageFinalizeResponse.builder()
                        .ready(true)
                        .imageKeys(java.util.List.of("images/articles/42/abc.webp"))
                        .build());

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", articleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ready").value(true))
                .andExpect(jsonPath("$.imageKeys[0]").value("images/articles/42/abc.webp"));

        verify(imageUploadService).finalizeImages(eq(articleId), any());
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: 일부 imageKey가 누락되면 409 + code=1207 + missingKeys/presentKeys 포함")
    void finalize_notReady_returns409WithMissingKeys() throws Exception {
        long articleId = 42L;
        ArticleImageFinalizeRequest req = ArticleImageFinalizeRequest.builder()
                .imageKeys(java.util.List.of("images/articles/42/abc.webp", "images/articles/42/def.webp"))
                .build();
        given(imageUploadService.finalizeImages(eq(articleId), any()))
                .willThrow(new ArticleImagesNotReadyException(
                        java.util.List.of("images/articles/42/def.webp"),
                        java.util.List.of("images/articles/42/abc.webp")));

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", articleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.code").value("1207"))
                .andExpect(jsonPath("$.missingKeys[0]").value("images/articles/42/def.webp"))
                .andExpect(jsonPath("$.presentKeys[0]").value("images/articles/42/abc.webp"));
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: imageKeys 누락 → 400 (service 미호출)")
    void finalize_emptyImageKeys_returns400() throws Exception {
        // imageKeys 배열 자체가 비어 있음
        String body = "{\"imageKeys\":[]}";

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", 1L)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isBadRequest());

        verify(imageUploadService, never()).finalizeImages(any(), any());
    }

    @Test
    @DisplayName("ROLE_USER가 호출하면 클래스 레벨 @PreAuthorize가 막아 403 + ADMIN_ACCESS_DENIED 응답을 내려준다")
    void create_nonAdmin_returns403() throws Exception {
        // override @BeforeEach: USER role로 다시 세팅
        SecurityContextHolder.clearContext();
        User user = User.builder()
                .nickname("user")
                .provider("test")
                .oauthId("user-oid")
                .role(Role.USER)
                .build();
        ReflectionTestUtils.setField(user, "id", 99L);
        CustomUserDetails principal = new CustomUserDetails(user);
        UsernamePasswordAuthenticationToken auth =
                new UsernamePasswordAuthenticationToken(principal, null, principal.getAuthorities());
        SecurityContextHolder.getContext().setAuthentication(auth);

        CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                .slug("ok-slug")
                .title("t")
                .contentMdx("c")
                .build();

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.code").value("605"));

        verify(articleService, never()).create(any());
    }
}
