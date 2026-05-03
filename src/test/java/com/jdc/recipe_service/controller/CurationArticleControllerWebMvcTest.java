package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.handler.GlobalExceptionHandler;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import com.jdc.recipe_service.service.article.PublicCurationArticleService;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.hashids.Hashids;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDateTime;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * {@link CurationArticleController} @WebMvcTest. (이 프로젝트는 일반 유저용 컨트롤러에 prefix를 붙이지 않는다.)
 *
 * <p>인증 불필요. service가 PUBLISHED 필터를 강제하므로 controller는 단순히 service를 위임한다 —
 * 컨트롤러 테스트는 (1) 라우팅 정확성 (2) status 매핑 (3) 응답 shape의 unpublished 필드 미노출에 집중한다.
 */
@WebMvcTest(controllers = CurationArticleController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({GlobalExceptionHandler.class, HashIdConfig.class,
        CurationArticleControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_PUBLIC_CURATION_ARTICLE_CONTROLLER",
        "app.hashids.min-length=8"
})
class CurationArticleControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;

    @MockBean PublicCurationArticleService publicArticleService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    @Test
    @DisplayName("GET /api/curation-articles: 200 + Page 응답. id는 HashID 문자열, 운영 전용 필드 미노출")
    void list_ok() throws Exception {
        PublicCurationArticleSummaryResponse item = PublicCurationArticleSummaryResponse.builder()
                .id(7L)
                .slug("summer-diet")
                .title("여름 다이어트")
                .description("desc")
                .coverImageKey("images/articles/7/uuid.webp")
                .category("diet")
                .publishedAt(LocalDateTime.of(2026, 5, 1, 10, 0))
                .build();
        given(publicArticleService.listPublished(eq("diet"), any()))
                .willReturn(new PageImpl<>(List.of(item), PageRequest.of(0, 20), 1));

        // 기대값은 같은 Hashids 인스턴스로 동적 생성 — salt/minLength 변경되어도 회귀 안 깨짐
        String expectedArticleId = hashids.encode(7L);

        mockMvc.perform(get("/api/curation-articles")
                        .param("category", "diet"))
                .andExpect(status().isOk())
                // raw Long 7이 아니라 HashID 문자열로 내려가야 한다
                .andExpect(jsonPath("$.content[0].id").value(expectedArticleId))
                .andExpect(jsonPath("$.content[0].id").isString())
                .andExpect(jsonPath("$.content[0].slug").value("summer-diet"))
                .andExpect(jsonPath("$.content[0].title").value("여름 다이어트"))
                .andExpect(jsonPath("$.content[0].coverImageKey").value("images/articles/7/uuid.webp"))
                // 운영 전용 필드는 응답 schema에 존재하지 않아야 한다
                .andExpect(jsonPath("$.content[0].status").doesNotExist())
                .andExpect(jsonPath("$.content[0].generatedBy").doesNotExist())
                .andExpect(jsonPath("$.content[0].humanReviewed").doesNotExist())
                .andExpect(jsonPath("$.content[0].contentMdx").doesNotExist())
                // 메타데이터는 $.page 하위에 nested. WebConfig의 @EnableSpringDataWebSupport(VIA_DTO)로
                // 명시 고정된 wire shape이라 환경/버전과 무관하게 안정적이다 — 프론트 계약으로 회귀 잡는다.
                .andExpect(jsonPath("$.page.totalElements").value(1))
                .andExpect(jsonPath("$.page.size").value(20))
                .andExpect(jsonPath("$.page.number").value(0));
    }

    @Test
    @DisplayName("GET /api/curation-articles/{slug}: 200 + id/recipeIds가 HashID 문자열, 본문 포함, 운영 필드 미노출")
    void getBySlug_ok() throws Exception {
        PublicCurationArticleResponse resp = PublicCurationArticleResponse.builder()
                .id(7L)
                .slug("summer-diet")
                .title("여름 다이어트")
                .contentMdx("# body")
                .coverImageKey("images/articles/7/uuid.webp")
                .recipeIds(List.of(11L, 12L))
                .publishedAt(LocalDateTime.of(2026, 5, 1, 10, 0))
                .build();
        given(publicArticleService.getBySlug("summer-diet")).willReturn(resp);

        String expectedArticleId = hashids.encode(7L);
        String expectedRecipe0   = hashids.encode(11L);
        String expectedRecipe1   = hashids.encode(12L);

        mockMvc.perform(get("/api/curation-articles/{slug}", "summer-diet"))
                .andExpect(status().isOk())
                // id는 HashID 문자열
                .andExpect(jsonPath("$.id").value(expectedArticleId))
                .andExpect(jsonPath("$.id").isString())
                .andExpect(jsonPath("$.slug").value("summer-diet"))
                .andExpect(jsonPath("$.contentMdx").value("# body"))
                // recipeIds 각 element도 HashID 문자열
                .andExpect(jsonPath("$.recipeIds[0]").value(expectedRecipe0))
                .andExpect(jsonPath("$.recipeIds[1]").value(expectedRecipe1))
                .andExpect(jsonPath("$.recipeIds[0]").isString())
                .andExpect(jsonPath("$.status").doesNotExist())
                .andExpect(jsonPath("$.generatedBy").doesNotExist())
                .andExpect(jsonPath("$.humanReviewed").doesNotExist());
    }

    @Test
    @DisplayName("GET /api/curation-articles/{slug}: DRAFT 상태 slug는 service에서 ARTICLE_NOT_FOUND 던지고 404 + code=1201로 응답")
    void getBySlug_draft_returns404() throws Exception {
        given(publicArticleService.getBySlug("draft-slug"))
                .willThrow(new CustomException(ErrorCode.ARTICLE_NOT_FOUND));

        mockMvc.perform(get("/api/curation-articles/{slug}", "draft-slug"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("1201"));
    }

    @Test
    @DisplayName("GET /api/curation-articles/{slug}: ARCHIVED 상태도 동일하게 404 + ARTICLE_NOT_FOUND")
    void getBySlug_archived_returns404() throws Exception {
        // service 레이어에서 ARCHIVED는 PUBLISHED 필터에 걸러져 동일하게 ARTICLE_NOT_FOUND.
        given(publicArticleService.getBySlug("archived-slug"))
                .willThrow(new CustomException(ErrorCode.ARTICLE_NOT_FOUND));

        mockMvc.perform(get("/api/curation-articles/{slug}", "archived-slug"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("1201"));
    }
}
