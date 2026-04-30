package com.jdc.recipe_service.dev.controller.recipe;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.BDDMockito.willThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeWriteController @WebMvcTest — HashID resolver chain 잠금 (PUT, DELETE).
 *
 * POST는 path variable이 없으므로 HashID 회귀 위험이 없어 unit test로 충분 — 여기서는 PUT/DELETE의
 * recipeId path variable이 raw Long으로 디코딩되어 service까지 전달되는지 검증.
 */
@WebMvcTest(controllers = DevRecipeWriteController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeWriteControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RECIPE_WRITE_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeWriteControllerWebMvcTest {

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

    @MockBean DevRecipeWriteService devRecipeWriteService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 13579L;
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
    @DisplayName("PUT /api/dev/recipes/{hashId}: HashID 디코딩 후 service에 raw Long + userId 전달")
    void update_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().build();
        PresignedUrlResponse stub = PresignedUrlResponse.builder().recipeId(RAW_RECIPE_ID).build();
        given(devRecipeWriteService.updateRecipe(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any(RecipeUpdateWithImageRequest.class)))
                .willReturn(stub);

        mockMvc.perform(put("/api/dev/recipes/{recipeId}", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk());

        verify(devRecipeWriteService).updateRecipe(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), any(RecipeUpdateWithImageRequest.class));
    }

    @Test
    @DisplayName("DELETE /api/dev/recipes/{hashId}: HashID 디코딩 후 service에 raw Long + userId 전달")
    void delete_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);

        mockMvc.perform(delete("/api/dev/recipes/{recipeId}", hashed))
                .andExpect(status().isOk());

        verify(devRecipeWriteService).deleteRecipe(RAW_USER_ID, RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: POST /api/dev/recipes — blank title → 400 INVALID_INPUT_VALUE, service 미호출")
    void create_blankTitle_returns400() throws Exception {
        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setTitle("");      // @NotBlank 위반
        dto.setDishType("찜요리");
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().recipe(dto).build();

        // service-level Validator가 nested validation 실행 → INVALID_INPUT_VALUE throw
        // (운영 RecipeWithImageUploadRequest.recipe 필드에 @Valid 없어 controller @RequestBody만으론 cascade 안 됨)
        willThrow(new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 제목은 필수입니다."))
                .given(devRecipeWriteService).createRecipe(eq(RAW_USER_ID), any(RecipeWithImageUploadRequest.class));

        mockMvc.perform(post("/api/dev/recipes")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value(ErrorCode.INVALID_INPUT_VALUE.getCode()));
    }

}
