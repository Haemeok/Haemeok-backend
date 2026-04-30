package com.jdc.recipe_service.dev.controller.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.status.DevRecipeStatusService;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeSaveStatusResponse;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeStatusRequest;
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

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeStatusController @WebMvcTest — HashID resolver chain 잠금.
 *
 * 단건은 path variable HashID 디코딩, 배치는 request body의 List<Long>이 HashIdDeserializer로 디코딩되는지 검증.
 * 응답 인코딩(Long → HashID key)도 함께 확인.
 */
@WebMvcTest(controllers = DevRecipeStatusController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeStatusControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_STATUS_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeStatusControllerWebMvcTest {

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

    @MockBean DevRecipeStatusService devRecipeStatusService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 12345L;
    private static final long RAW_USER_ID = 7L;

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
    @DisplayName("GET /api/dev/recipes/{hashId}/status: HashID 디코딩 후 service에 raw Long 전달")
    void detail_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        RecipeDetailStatusDto stub = RecipeDetailStatusDto.builder().build();
        given(devRecipeStatusService.getDetailStatus(eq(RAW_USER_ID), eq(RAW_RECIPE_ID))).willReturn(stub);

        mockMvc.perform(get("/api/dev/recipes/{id}/status", hashed))
                .andExpect(status().isOk());

        verify(devRecipeStatusService).getDetailStatus(RAW_USER_ID, RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("POST /api/dev/recipes/status: body의 List<HashID>를 HashIdDeserializer로 raw Long으로 디코딩")
    void batch_decodesBodyHashIdsToRawLongs() throws Exception {
        long raw1 = 111L;
        long raw2 = 222L;
        String hash1 = hashids.encode(raw1);
        String hash2 = hashids.encode(raw2);
        RecipeStatusRequest req = new RecipeStatusRequest(List.of(raw1, raw2));
        // request body는 hashid 문자열이지만 HashIdDeserializer가 Long으로 변환하여 service에 전달

        RecipeSimpleStatusDto stub = RecipeSimpleStatusDto.builder().likedByCurrentUser(true).build();
        given(devRecipeStatusService.getBatchSimpleStatuses(eq(RAW_USER_ID), eq(List.of(raw1, raw2))))
                .willReturn(Map.of(raw1, stub, raw2, stub));

        // body는 hashid string array 형태로 전송 (HashIdDeserializer가 디코딩)
        String bodyJson = "{\"recipeIds\":[\"" + hash1 + "\",\"" + hash2 + "\"]}";

        mockMvc.perform(post("/api/dev/recipes/status")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(bodyJson))
                .andExpect(status().isOk())
                // 응답 key는 다시 HashID로 인코딩되어야 함 (raw Long key 노출 금지)
                .andExpect(jsonPath("$." + hash1 + ".likedByCurrentUser").value(true))
                .andExpect(jsonPath("$." + hash2 + ".likedByCurrentUser").value(true));

        // service에는 디코딩된 raw Long 리스트가 전달됨
        verify(devRecipeStatusService).getBatchSimpleStatuses(eq(RAW_USER_ID), eq(List.of(raw1, raw2)));
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{hashId}/saved-books: HashID 디코딩 + 인증 principal로부터 userId 추출")
    void savedBooks_decodesHashIdAndUsesAuthPrincipal() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        RecipeSaveStatusResponse stub = RecipeSaveStatusResponse.builder()
                .saved(false).savedBookCount(0).books(List.of()).build();
        given(devRecipeStatusService.getSaveStatus(eq(RAW_USER_ID), eq(RAW_RECIPE_ID))).willReturn(stub);

        mockMvc.perform(get("/api/dev/recipes/{id}/saved-books", hashed))
                .andExpect(status().isOk());

        verify(devRecipeStatusService).getSaveStatus(RAW_USER_ID, RAW_RECIPE_ID);
    }
}
