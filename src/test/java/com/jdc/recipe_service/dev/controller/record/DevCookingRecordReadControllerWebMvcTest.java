package com.jdc.recipe_service.dev.controller.record;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.domain.dto.record.DevCookingRecordFeedResponse;
import com.jdc.recipe_service.dev.service.record.DevCookingRecordReadService;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
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
import org.springframework.data.domain.Pageable;
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
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevCookingRecordReadController @WebMvcTest — HashID resolver chain (recordId path variable).
 *
 * 회귀 방지: rating/comment phase에서 잡았던 @DecodeId 명시 이름 + path variable 디코딩 회귀 영역.
 */
@WebMvcTest(controllers = DevCookingRecordReadController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevCookingRecordReadControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RECORD_READ_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevCookingRecordReadControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;

    @MockBean DevCookingRecordReadService devCookingRecordReadService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_USER_ID = 7L;
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
    @DisplayName("GET /api/dev/me/records/{hashId}: recordId path variable HashID 디코딩 후 service에 raw Long 전달")
    void detail_decodesRecordIdHashId() throws Exception {
        String hashed = hashids.encode(RAW_RECORD_ID);
        CookingRecordDto stub = mock(CookingRecordDto.class);
        given(devCookingRecordReadService.getRecordDetail(eq(RAW_USER_ID), eq(RAW_RECORD_ID))).willReturn(stub);

        mockMvc.perform(get("/api/dev/me/records/{recordId}", hashed))
                .andExpect(status().isOk());

        verify(devCookingRecordReadService).getRecordDetail(RAW_USER_ID, RAW_RECORD_ID);
    }

    @Test
    @DisplayName("GET /api/dev/me/records/timeline: 인증 principal 추출 후 service 위임 (path variable 없음)")
    void timeline_authenticated_callsService() throws Exception {
        DevCookingRecordFeedResponse stub = DevCookingRecordFeedResponse.builder().groups(java.util.List.of()).hasNext(false).build();
        given(devCookingRecordReadService.getRecordFeed(eq(RAW_USER_ID), any(Pageable.class))).willReturn(stub);

        mockMvc.perform(get("/api/dev/me/records/timeline"))
                .andExpect(status().isOk());

        verify(devCookingRecordReadService).getRecordFeed(eq(RAW_USER_ID), any(Pageable.class));
    }
}
