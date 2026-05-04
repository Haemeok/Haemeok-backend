package com.jdc.recipe_service.controller.admin;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
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
import org.hashids.Hashids;
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
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
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
@Import({GlobalExceptionHandler.class, HashIdConfig.class,
        AdminCurationArticleControllerWebMvcTest.MethodSecurityTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_ADMIN_CURATION_ARTICLE_CONTROLLER",
        "app.hashids.min-length=8"
})
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
    @Autowired Hashids hashids;

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
    @DisplayName("POST /api/admin/curation-articles: 정상 요청 → 201 + articleId가 HashID 문자열")
    void create_ok() throws Exception {
        CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                .slug("summer-diet")
                .title("여름 다이어트")
                .contentMdx("# body")
                .build();
        given(articleService.create(any(CurationArticleCreateRequest.class))).willReturn(42L);

        String expectedHashId = hashids.encode(42L);

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.articleId").value(expectedHashId))
                .andExpect(jsonPath("$.articleId").isString());

        verify(articleService).create(any(CurationArticleCreateRequest.class));
    }

    @Test
    @DisplayName("POST /api/admin/curation-articles: 요청 본문의 recipeIds(string HashID 배열)가 service에 raw Long으로 디코드되어 전달")
    void create_recipeIdsAreDecodedFromHashId() throws Exception {
        long rawRecipeId1 = 101L;
        long rawRecipeId2 = 102L;
        String hashedRecipe1 = hashids.encode(rawRecipeId1);
        String hashedRecipe2 = hashids.encode(rawRecipeId2);

        // body는 HashID 문자열 배열
        String body = String.format("""
                {
                  "slug": "summer-diet",
                  "title": "test",
                  "contentMdx": "# body",
                  "recipeIds": ["%s", "%s"]
                }
                """, hashedRecipe1, hashedRecipe2);

        given(articleService.create(any(CurationArticleCreateRequest.class))).willReturn(42L);

        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isCreated());

        // service에 전달된 request는 raw Long 디코드된 상태여야 한다
        org.mockito.ArgumentCaptor<CurationArticleCreateRequest> cap =
                org.mockito.ArgumentCaptor.forClass(CurationArticleCreateRequest.class);
        verify(articleService).create(cap.capture());
        org.assertj.core.api.Assertions.assertThat(cap.getValue().getRecipeIds())
                .containsExactly(rawRecipeId1, rawRecipeId2);
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
    @DisplayName("POST /{articleId}/images/presigned-urls: articleId path는 HashID로 받고 service엔 raw Long 전달")
    void issuePresignedUrl_ok() throws Exception {
        long rawArticleId = 123L;
        String hashedArticleId = hashids.encode(rawArticleId);
        ArticleImagePresignedUrlRequest req = ArticleImagePresignedUrlRequest.builder()
                .contentType("image/jpeg")
                .fileSize(123_456L)
                .build();
        // 응답의 imageKey segment도 HashID 기반
        String mockUploadKey = "original/images/articles/" + hashedArticleId + "/uuid.jpg";
        String mockImageKey = "images/articles/" + hashedArticleId + "/uuid.webp";
        given(imageUploadService.issuePresignedUrl(eq(rawArticleId), any(ArticleImagePresignedUrlRequest.class)))
                .willReturn(ArticleImagePresignedUrlResponse.builder()
                        .uploadKey(mockUploadKey)
                        .imageKey(mockImageKey)
                        .presignedUrl("https://s3.test/uuid")
                        .build());

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/presigned-urls", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.uploadKey").value(mockUploadKey))
                .andExpect(jsonPath("$.imageKey").value(mockImageKey))
                .andExpect(jsonPath("$.presignedUrl").value("https://s3.test/uuid"));

        // path의 HashID가 디코드되어 service에는 raw Long(123)으로 전달됐는지 검증
        verify(imageUploadService).issuePresignedUrl(eq(rawArticleId), any(ArticleImagePresignedUrlRequest.class));
    }

    @Test
    @DisplayName("POST /{articleId}/images/presigned-urls: fileSize 누락 → 400 (service 미호출)")
    void issuePresignedUrl_missingFileSize_returns400() throws Exception {
        String hashedArticleId = hashids.encode(1L);
        String body = "{\"contentType\":\"image/webp\"}";

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/presigned-urls", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isBadRequest());

        verify(imageUploadService, never()).issuePresignedUrl(any(), any());
    }

    @Test
    @DisplayName("POST /{articleId}/publish: HashID path가 service에 raw Long으로 디코드되어 전달")
    void publish_callsService() throws Exception {
        long rawArticleId = 42L;
        String hashedArticleId = hashids.encode(rawArticleId);

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/publish", hashedArticleId))
                .andExpect(status().isOk());

        verify(articleService).publish(eq(rawArticleId));
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: 모든 imageKey ready면 200 + service에 raw Long 전달 (HashID 디코드)")
    void finalize_ok() throws Exception {
        long rawArticleId = 42L;
        String hashedArticleId = hashids.encode(rawArticleId);
        String key = "images/articles/" + hashedArticleId + "/abc.webp";

        ArticleImageFinalizeRequest req = ArticleImageFinalizeRequest.builder()
                .imageKeys(java.util.List.of(key))
                .build();
        given(imageUploadService.finalizeImages(eq(rawArticleId), any()))
                .willReturn(ArticleImageFinalizeResponse.builder()
                        .ready(true)
                        .imageKeys(java.util.List.of(key))
                        .build());

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ready").value(true))
                .andExpect(jsonPath("$.imageKeys[0]").value(key));

        verify(imageUploadService).finalizeImages(eq(rawArticleId), any());
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: 일부 imageKey가 누락되면 409 + code=1207 + missingKeys/presentKeys 포함")
    void finalize_notReady_returns409WithMissingKeys() throws Exception {
        long rawArticleId = 42L;
        String hashedArticleId = hashids.encode(rawArticleId);
        String present = "images/articles/" + hashedArticleId + "/abc.webp";
        String missing = "images/articles/" + hashedArticleId + "/def.webp";

        ArticleImageFinalizeRequest req = ArticleImageFinalizeRequest.builder()
                .imageKeys(java.util.List.of(present, missing))
                .build();
        given(imageUploadService.finalizeImages(eq(rawArticleId), any()))
                .willThrow(new ArticleImagesNotReadyException(
                        java.util.List.of(missing),
                        java.util.List.of(present)));

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.code").value("1207"))
                .andExpect(jsonPath("$.missingKeys[0]").value(missing))
                .andExpect(jsonPath("$.presentKeys[0]").value(present));
    }

    @Test
    @DisplayName("POST /{articleId}/images/finalize: imageKeys 누락 → 400 (service 미호출)")
    void finalize_emptyImageKeys_returns400() throws Exception {
        String hashedArticleId = hashids.encode(1L);
        String body = "{\"imageKeys\":[]}";

        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/images/finalize", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isBadRequest());

        verify(imageUploadService, never()).finalizeImages(any(), any());
    }

    @Test
    @DisplayName("GET /{articleId}: 응답 DTO의 id는 HashID 문자열, recipeIds도 string[]로 직렬화된다")
    void getDetail_serializesIdsAsHashIdStrings() throws Exception {
        long rawArticleId = 7L;
        String hashedArticleId = hashids.encode(rawArticleId);
        String hashedRecipe1 = hashids.encode(101L);
        String hashedRecipe2 = hashids.encode(102L);

        com.jdc.recipe_service.domain.dto.article.CurationArticleResponse resp =
                com.jdc.recipe_service.domain.dto.article.CurationArticleResponse.builder()
                        .id(rawArticleId)
                        .slug("summer-diet")
                        .title("title")
                        .contentMdx("c")
                        .status(com.jdc.recipe_service.domain.type.article.ArticleStatus.PUBLISHED)
                        .humanReviewed(true)
                        .recipeIds(java.util.List.of(101L, 102L))
                        .build();
        given(articleService.get(eq(rawArticleId))).willReturn(resp);

        mockMvc.perform(get("/api/admin/curation-articles/{articleId}", hashedArticleId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(hashedArticleId))
                .andExpect(jsonPath("$.id").isString())
                .andExpect(jsonPath("$.recipeIds[0]").value(hashedRecipe1))
                .andExpect(jsonPath("$.recipeIds[1]").value(hashedRecipe2))
                .andExpect(jsonPath("$.recipeIds[0]").isString());
    }

    @Test
    @DisplayName("GET /: 목록 응답 content[].id가 HashID 문자열로 직렬화된다")
    void list_serializesIdsAsHashIdStrings() throws Exception {
        long rawArticleId = 42L;
        String hashedArticleId = hashids.encode(rawArticleId);
        com.jdc.recipe_service.domain.dto.article.CurationArticleSummaryResponse item =
                com.jdc.recipe_service.domain.dto.article.CurationArticleSummaryResponse.builder()
                        .id(rawArticleId)
                        .slug("summer-diet")
                        .title("title")
                        .status(com.jdc.recipe_service.domain.type.article.ArticleStatus.DRAFT)
                        .humanReviewed(false)
                        .build();
        given(articleService.search(any(), any(), any(), any()))
                .willReturn(new org.springframework.data.domain.PageImpl<>(java.util.List.of(item)));

        mockMvc.perform(get("/api/admin/curation-articles"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(hashedArticleId))
                .andExpect(jsonPath("$.content[0].id").isString());
    }

    @Test
    @DisplayName("PUT /{articleId}: 응답 articleId가 HashID 문자열")
    void update_returnsHashIdArticleId() throws Exception {
        long rawArticleId = 42L;
        String hashedArticleId = hashids.encode(rawArticleId);
        given(articleService.update(eq(rawArticleId), any())).willReturn(rawArticleId);

        // body는 최소 필수만 — recipeIds는 비워도 됨 (validation 통과)
        String body = """
                { "title": "수정", "contentMdx": "c" }
                """;
        mockMvc.perform(put("/api/admin/curation-articles/{articleId}", hashedArticleId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.articleId").value(hashedArticleId))
                .andExpect(jsonPath("$.articleId").isString());
    }

    @Test
    @DisplayName("strict decoder: raw 숫자 path(`/42/publish`)는 400 거부 (service 미호출)")
    void publish_rejectsRawNumericPath() throws Exception {
        // 숫자만 박은 path는 정책상 거부 — strict decoder가 INVALID_INPUT_VALUE
        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/publish", "42"))
                .andExpect(status().isBadRequest());

        verify(articleService, never()).publish(any());
    }

    @Test
    @DisplayName("strict decoder: 잘못된 HashID path도 400 거부 (decode 실패)")
    void publish_rejectsInvalidHashIdPath() throws Exception {
        mockMvc.perform(post("/api/admin/curation-articles/{articleId}/publish", "definitelyNotAHashId123!@#"))
                .andExpect(status().isBadRequest());

        verify(articleService, never()).publish(any());
    }

    @Test
    @DisplayName("strict deserializer: body recipeIds에 raw 숫자 배열이 오면 400 거부")
    void create_rejectsRawNumericRecipeIds() throws Exception {
        // 숫자 그대로 박은 recipeIds — strict deserializer가 거부
        String body = """
                {
                  "slug": "ok-slug",
                  "title": "t",
                  "contentMdx": "c",
                  "recipeIds": [101, 102]
                }
                """;
        mockMvc.perform(post("/api/admin/curation-articles")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isBadRequest());

        verify(articleService, never()).create(any());
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
