package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeRelatedService;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.hashids.Hashids;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import static org.assertj.core.api.Assertions.assertThat;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeRelatedController @WebMvcTest — HashID resolver chain 잠금.
 *
 * 두 endpoint 모두 anonymous 허용이라 SecurityContext 설정 없이 테스트 가능 (controller가 viewerId=null 전달).
 */
@WebMvcTest(controllers = DevRecipeRelatedController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeRelatedControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RELATED_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeRelatedControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;

    @MockBean DevRecipeRelatedService devRecipeRelatedService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 24680L;

    @AfterEach
    void clearAuth() {
        SecurityContextHolder.clearContext();
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{hashId}/recommendations: HashID 디코딩 + size param 전달")
    void recommendations_decodesHashIdAndPassesSize() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        given(devRecipeRelatedService.getRecommendations(eq(null), eq(RAW_RECIPE_ID), eq(7))).willReturn(List.of());

        mockMvc.perform(get("/api/dev/recipes/{id}/recommendations", hashed).param("size", "7"))
                .andExpect(status().isOk());

        verify(devRecipeRelatedService).getRecommendations(null, RAW_RECIPE_ID, 7);
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{hashId}/recommendations: size 미지정 시 default=10")
    void recommendations_defaultSize() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        given(devRecipeRelatedService.getRecommendations(eq(null), eq(RAW_RECIPE_ID), eq(10))).willReturn(List.of());

        mockMvc.perform(get("/api/dev/recipes/{id}/recommendations", hashed))
                .andExpect(status().isOk());

        verify(devRecipeRelatedService).getRecommendations(null, RAW_RECIPE_ID, 10);
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{hashId}/remixes: HashID 디코딩 + default Pageable (size=10, sort=popularityScore DESC) ArgumentCaptor 검증")
    void remixes_decodesHashIdWithDefaultPageable() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        given(devRecipeRelatedService.findRemixes(eq(null), eq(RAW_RECIPE_ID), any(Pageable.class)))
                .willReturn(new PageImpl<>(List.of()));

        mockMvc.perform(get("/api/dev/recipes/{recipeId}/remixes", hashed))
                .andExpect(status().isOk());

        ArgumentCaptor<Pageable> pageableCap = ArgumentCaptor.forClass(Pageable.class);
        verify(devRecipeRelatedService).findRemixes(eq(null), eq(RAW_RECIPE_ID), pageableCap.capture());
        Pageable captured = pageableCap.getValue();
        assertThat(captured.getPageSize()).isEqualTo(10);
        assertThat(captured.getPageNumber()).isZero();
        // default sort = popularityScore DESC
        Sort.Order order = captured.getSort().getOrderFor("popularityScore");
        assertThat(order).isNotNull();
        assertThat(order.getDirection()).isEqualTo(Sort.Direction.DESC);
    }
}
