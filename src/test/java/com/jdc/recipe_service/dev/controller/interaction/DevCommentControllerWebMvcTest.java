package com.jdc.recipe_service.dev.controller.interaction;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.interaction.DevCommentService;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevCommentController @WebMvcTest — multi-path-variable HashID resolver chain 잠금.
 *
 * 회귀 방지: {@code @DecodeId} 명시 이름 없이 파라미터명 추론에 의존하면 컴파일 옵션/리팩토링에 따라
 * 다른 path variable이 잘못 바인딩될 수 있다. {@code /comments/{parentId}/replies}처럼 multi-path
 * 시나리오는 단위 테스트로 못 잡는다 — 실제 라우팅으로 각 HashID가 올바른 파라미터로 디코딩되는지 검증.
 */
@WebMvcTest(controllers = DevCommentController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevCommentControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_COMMENT_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevCommentControllerWebMvcTest {

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

    @MockBean DevCommentService devCommentService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_RECIPE_ID = 12345L;
    private static final long RAW_PARENT_ID = 67890L;
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
    @DisplayName("POST /api/dev/recipes/{recipeId}/comments/{parentId}/replies: 두 HashID가 각각 recipeId/parentId로 디코딩")
    void reply_decodesMultipleHashIdsToCorrectParameters() throws Exception {
        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        String hashedParent = hashids.encode(RAW_PARENT_ID);
        CommentRequestDto req = CommentRequestDto.builder().content("re").build();
        ReplyDto stub = ReplyDto.builder().id(1L).build();
        given(devCommentService.createReply(eq(RAW_USER_ID), eq(RAW_RECIPE_ID), eq(RAW_PARENT_ID), any(CommentRequestDto.class), any(User.class)))
                .willReturn(stub);

        mockMvc.perform(post("/api/dev/recipes/{recipeId}/comments/{parentId}/replies", hashedRecipe, hashedParent)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isCreated());

        // 핵심 invariant: recipeId와 parentId가 뒤바뀌지 않고 각각 정확한 raw Long으로 service에 전달됨
        ArgumentCaptor<Long> userCap = ArgumentCaptor.forClass(Long.class);
        ArgumentCaptor<Long> recipeCap = ArgumentCaptor.forClass(Long.class);
        ArgumentCaptor<Long> parentCap = ArgumentCaptor.forClass(Long.class);
        verify(devCommentService).createReply(userCap.capture(), recipeCap.capture(), parentCap.capture(),
                any(CommentRequestDto.class), any(User.class));
        assertThat(userCap.getValue()).isEqualTo(RAW_USER_ID);
        assertThat(recipeCap.getValue()).isEqualTo(RAW_RECIPE_ID);
        assertThat(parentCap.getValue()).isEqualTo(RAW_PARENT_ID);
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{recipeId}/comments: anonymous + HashID 디코딩 → service에 viewerId=null 전달")
    void list_anonymousDecodesHashId() throws Exception {
        SecurityContextHolder.clearContext(); // 이 테스트만 anonymous

        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        Page<CommentDto> page = new PageImpl<>(List.of());
        given(devCommentService.getAllCommentsWithLikes(eq(null), eq(RAW_RECIPE_ID), any(Pageable.class)))
                .willReturn(page);

        mockMvc.perform(get("/api/dev/recipes/{recipeId}/comments", hashedRecipe))
                .andExpect(status().isOk());

        verify(devCommentService).getAllCommentsWithLikes(eq(null), eq(RAW_RECIPE_ID), any(Pageable.class));
    }

    @Test
    @DisplayName("GET /api/dev/recipes/{recipeId}/comments/{commentId}/replies: 두 HashID → recipeId/commentId로 정확히 디코딩")
    void getReplies_decodesMultipleHashIdsToCorrectParameters() throws Exception {
        SecurityContextHolder.clearContext(); // anonymous 경로도 잠금

        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        long rawCommentId = 99999L;
        String hashedComment = hashids.encode(rawCommentId);
        given(devCommentService.getCommentWithReplies(eq(null), eq(RAW_RECIPE_ID), eq(rawCommentId), any(Pageable.class)))
                .willReturn(null); // body 검증 아님 — 라우팅만 검증

        mockMvc.perform(get("/api/dev/recipes/{recipeId}/comments/{commentId}/replies", hashedRecipe, hashedComment))
                .andExpect(status().isOk());

        verify(devCommentService).getCommentWithReplies(eq(null), eq(RAW_RECIPE_ID), eq(rawCommentId), any(Pageable.class));
    }

    @Test
    @DisplayName("DELETE /api/dev/recipes/{recipeId}/comments/{commentId}: URL의 recipeId는 무시, commentId만 디코딩되어 service 전달")
    void delete_ignoresRecipeIdUsesCommentId() throws Exception {
        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        long rawCommentId = 88888L;
        String hashedComment = hashids.encode(rawCommentId);

        mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders
                        .delete("/api/dev/recipes/{recipeId}/comments/{commentId}", hashedRecipe, hashedComment))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").value("댓글이 삭제되었습니다."));

        verify(devCommentService).deleteComment(RAW_USER_ID, rawCommentId);
    }
}
