package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.record.DevCookingRecordWriteService;
import com.jdc.recipe_service.domain.entity.CookingRecord;
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
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevCookingRecordWriteController @WebMvcTest — HashID resolver chain.
 *
 * - POST 의 recipeId는 query param HashID
 * - DELETE 의 recordId는 path variable HashID
 * - POST 응답의 recordId는 hashid 인코딩됨
 */
@WebMvcTest(controllers = DevCookingRecordWriteController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevCookingRecordWriteControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RECORD_WRITE_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevCookingRecordWriteControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;

    @MockBean DevCookingRecordWriteService devCookingRecordWriteService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_USER_ID = 7L;
    private static final long RAW_RECIPE_ID = 12345L;
    private static final long RAW_RECORD_ID = 67890L;

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
    @DisplayName("POST /api/dev/me/records?recipeId={hash}: query param HashID 디코딩 + 응답 recordId hashid 인코딩")
    void create_decodesRecipeIdAndEncodesRecordId() throws Exception {
        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        CookingRecord record = mock(CookingRecord.class);
        given(record.getId()).willReturn(RAW_RECORD_ID);
        given(devCookingRecordWriteService.createCookingRecord(eq(RAW_USER_ID), eq(RAW_RECIPE_ID))).willReturn(record);

        mockMvc.perform(post("/api/dev/me/records").param("recipeId", hashedRecipe))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.recordId").value(hashids.encode(RAW_RECORD_ID)))
                .andExpect(jsonPath("$.message").value("요리 기록이 추가되었습니다."));

        verify(devCookingRecordWriteService).createCookingRecord(RAW_USER_ID, RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("DELETE /api/dev/me/records/{hash}: path variable HashID 디코딩 후 service에 raw Long")
    void delete_decodesRecordIdHashId() throws Exception {
        String hashedRecord = hashids.encode(RAW_RECORD_ID);

        mockMvc.perform(delete("/api/dev/me/records/{recordId}", hashedRecord))
                .andExpect(status().isOk());

        verify(devCookingRecordWriteService).deleteCookingRecord(RAW_USER_ID, RAW_RECORD_ID);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: POST /api/dev/me/records (recipeId param 누락) → 400 INVALID_INPUT_VALUE, service 미호출")
    void create_missingRecipeId_returns400AndSkipsService() throws Exception {
        mockMvc.perform(post("/api/dev/me/records"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value(com.jdc.recipe_service.exception.ErrorCode.INVALID_INPUT_VALUE.getCode()));

        verify(devCookingRecordWriteService, never()).createCookingRecord(any(), any());
    }
}
