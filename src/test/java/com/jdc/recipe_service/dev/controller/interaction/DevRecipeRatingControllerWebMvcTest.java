package com.jdc.recipe_service.dev.controller.interaction;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.interaction.DevRatingService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
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
import org.mockito.ArgumentCaptor;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeRatingController @WebMvcTest — HashID resolver chain 잠금.
 *
 * 회귀 방지: 컨트롤러 시그니처에 {@code @PathVariable}을 같이 붙이면 Spring 기본 resolver가 먼저 잡아
 * 숫자 경로만 우연히 통과하고 실제 HashID는 Long 변환 실패로 깨진다. 단위 테스트는 직접 Long을 넘기므로
 * 이 유형을 못 잡는다 — 인코딩된 HashID로 실제 라우팅해서 디코딩된 Long이 service까지 흐르는지 확인.
 */
@WebMvcTest(controllers = DevRecipeRatingController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeRatingControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RATING_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeRatingControllerWebMvcTest {

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

    @MockBean DevRatingService devRatingService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 12345L;
    private static final long RAW_USER_ID = 7L;

    @BeforeEach
    void setUpAuth() {
        // addFilters=false라 SecurityContextRepository가 propagate하지 않음 → 직접 SecurityContextHolder에 주입
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
    @DisplayName("POST /api/dev/ratings/recipe/{hashId}: encoded HashID가 디코딩되어 service에 raw Long으로 전달됨")
    void post_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(4.5).build();

        mockMvc.perform(post("/api/dev/ratings/recipe/{id}", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("평점 등록 완료"));

        ArgumentCaptor<Long> recipeIdCap = ArgumentCaptor.forClass(Long.class);
        ArgumentCaptor<Long> userIdCap = ArgumentCaptor.forClass(Long.class);
        verify(devRatingService).rateRecipe(userIdCap.capture(), recipeIdCap.capture(), any(RecipeRatingRequestDto.class));
        assertThat(recipeIdCap.getValue()).isEqualTo(RAW_RECIPE_ID);
        assertThat(userIdCap.getValue()).isEqualTo(RAW_USER_ID);
    }

    @Test
    @DisplayName("GET /api/dev/ratings/recipe/{hashId}/me: HashID 디코딩 후 service에 raw Long 전달, JSON {rating} 응답")
    void getMy_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);
        given(devRatingService.getMyRating(eq(RAW_USER_ID), eq(RAW_RECIPE_ID))).willReturn(4.5);

        mockMvc.perform(get("/api/dev/ratings/recipe/{id}/me", hashed))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.rating").value(4.5));

        verify(devRatingService).getMyRating(RAW_USER_ID, RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("DELETE /api/dev/ratings/recipe/{hashId}: HashID 디코딩 후 service에 raw Long 전달")
    void delete_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_RECIPE_ID);

        mockMvc.perform(delete("/api/dev/ratings/recipe/{id}", hashed))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("평점 삭제 완료"));

        verify(devRatingService).deleteRating(RAW_USER_ID, RAW_RECIPE_ID);
    }
}
