package com.jdc.recipe_service.dev.controller.recipe;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeImageWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeImageWriteController @WebMvcTest — HashID resolver chain (3 endpoint 모두 recipeId path variable).
 */
@WebMvcTest(controllers = DevRecipeImageWriteController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeImageWriteControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RECIPE_IMAGE_WRITE_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeImageWriteControllerWebMvcTest {

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

    @MockBean DevRecipeImageWriteService devRecipeImageWriteService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 24680L;
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
    @DisplayName("POST /api/dev/recipes/{hashId}/presigned-urls: HashID 디코딩 후 service에 raw Long 전달")
    void presigned_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        UpdatePresignedUrlRequest req = UpdatePresignedUrlRequest.builder().files(List.of()).build();
        UpdatePresignedUrlResponse stub = UpdatePresignedUrlResponse.builder().uploads(List.of()).build();
        given(devRecipeImageWriteService.generatePresignedUrlsForUpdate(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any()))
                .willReturn(stub);

        mockMvc.perform(post("/api/dev/recipes/{recipeId}/presigned-urls", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk());

        verify(devRecipeImageWriteService).generatePresignedUrlsForUpdate(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any());
    }

    @Test
    @DisplayName("POST /api/dev/recipes/{hashId}/finalize: HashID 디코딩 + raw Long 전달")
    void finalize_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        FinalizeResponse stub = new FinalizeResponse(RAW_RECIPE_ID, List.of(), List.of());
        given(devRecipeImageWriteService.finalizeRecipeImages(eq(RAW_USER_ID), eq(RAW_RECIPE_ID))).willReturn(stub);

        mockMvc.perform(post("/api/dev/recipes/{recipeId}/finalize", hashed))
                .andExpect(status().isOk());

        verify(devRecipeImageWriteService).finalizeRecipeImages(RAW_USER_ID, RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("PUT /api/dev/recipes/{hashId}/images: HashID 디코딩 + raw Long 전달")
    void updateImageKeys_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest(
                "images/recipes/" + RAW_RECIPE_ID + "/main.webp", List.of());

        mockMvc.perform(put("/api/dev/recipes/{recipeId}/images", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk());

        verify(devRecipeImageWriteService).updateImageKeys(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any(RecipeImageKeyUpdateRequest.class));
    }
}
