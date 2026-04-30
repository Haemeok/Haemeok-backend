package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.interaction.DevCommentLikeService;
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

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevCommentLikeController @WebMvcTest — HashID resolver chain 잠금.
 *
 * commentId path variable HashID 디코딩이 service까지 raw Long으로 전달되는지 + 응답 shape 검증.
 */
@WebMvcTest(controllers = DevCommentLikeController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevCommentLikeControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_COMMENT_LIKE_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevCommentLikeControllerWebMvcTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired MockMvc mockMvc;
    @Autowired Hashids hashids;

    @MockBean DevCommentLikeService devCommentLikeService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_COMMENT_ID = 56789L;
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
    @DisplayName("POST /api/dev/comments/{hashId}/like: HashID 디코딩 후 service에 raw Long 전달")
    void toggleLike_decodesHashIdToRawLong() throws Exception {
        String hashed = hashids.encode(RAW_COMMENT_ID);
        given(devCommentLikeService.toggleLike(eq(RAW_USER_ID), eq(RAW_COMMENT_ID))).willReturn(true);
        given(devCommentLikeService.countLikes(eq(RAW_COMMENT_ID))).willReturn(3);

        mockMvc.perform(post("/api/dev/comments/{commentId}/like", hashed))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.liked").value(true))
                .andExpect(jsonPath("$.likeCount").value(3))
                .andExpect(jsonPath("$.message").value("댓글 좋아요 등록 완료"));

        verify(devCommentLikeService).toggleLike(RAW_USER_ID, RAW_COMMENT_ID);
        verify(devCommentLikeService).countLikes(RAW_COMMENT_ID);
    }
}
