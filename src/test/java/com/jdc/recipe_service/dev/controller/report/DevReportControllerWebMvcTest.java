package com.jdc.recipe_service.dev.controller.report;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.report.DevReportService;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import com.jdc.recipe_service.security.CustomUserDetails;
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
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevReportController @WebMvcTest — recipeId path variable HashID 디코딩 잠금.
 */
@WebMvcTest(controllers = DevReportController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevReportControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_REPORT_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevReportControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;
    @Autowired ObjectMapper objectMapper;

    @MockBean DevReportService devReportService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_USER_ID = 7L;
    private static final long RAW_RECIPE_ID = 12345L;

    @BeforeEach
    void setUpAuth() {
        User user = User.builder().nickname("u").provider("test").oauthId("oid").build();
        ReflectionTestUtils.setField(user, "id", RAW_USER_ID);
        CustomUserDetails principal = new CustomUserDetails(user);
        UsernamePasswordAuthenticationToken auth =
                new UsernamePasswordAuthenticationToken(principal, null, principal.getAuthorities());
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @AfterEach
    void clearAuth() {
        SecurityContextHolder.clearContext();
    }

    @Test
    @DisplayName("POST /api/dev/recipes/{hashId}/reports: recipeId HashID 디코딩 후 service에 raw Long 전달")
    void report_decodesRecipeIdHashId() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        Map<String, Object> body = Map.of(
                "ingredientName", "양파",
                "reason", "WRONG_NAME"
        );

        mockMvc.perform(post("/api/dev/recipes/{recipeId}/reports", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(body)))
                .andExpect(status().isOk());

        verify(devReportService).createReportByName(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any());
    }
}
