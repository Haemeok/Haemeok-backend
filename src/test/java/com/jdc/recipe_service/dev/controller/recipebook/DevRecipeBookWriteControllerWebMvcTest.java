package com.jdc.recipe_service.dev.controller.recipebook;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.recipebook.DevRecipeBookWriteService;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RemoveRecipesFromBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RenameRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.ReorderRecipeBooksRequest;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import org.mockito.ArgumentCaptor;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * DevRecipeBookWriteController @WebMvcTest — HashID resolver chain (bookId path variable).
 *
 * 4 endpoint가 bookId path variable 사용 (PATCH rename, DELETE delete, POST addRecipes, DELETE removeRecipes).
 * 대표 케이스로 PATCH rename, POST addRecipes, DELETE removeRecipes 검증 — recipe IDs body deserialize도 함께.
 */
@WebMvcTest(controllers = DevRecipeBookWriteController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevRecipeBookWriteControllerWebMvcTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_RECIPE_BOOK_WRITE_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevRecipeBookWriteControllerWebMvcTest {

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

    @MockBean DevRecipeBookWriteService devRecipeBookWriteService;
    @MockBean JwtTokenProvider jwtTokenProvider;
    @MockBean UserDetailsService userDetailsService;
    @MockBean CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    private static final long RAW_BOOK_ID = 31415L;
    private static final long RAW_USER_ID = 7L;
    private static final long RAW_RECIPE_ID = 9999L;

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
    @DisplayName("PATCH /api/dev/me/recipe-books/{hashId}: bookId HashID 디코딩 후 service에 raw Long 전달")
    void rename_decodesBookIdHashId() throws Exception {
        String hashed = hashids.encode(RAW_BOOK_ID);
        RenameRecipeBookRequest req = RenameRecipeBookRequest.builder().name("새이름").build();

        mockMvc.perform(patch("/api/dev/me/recipe-books/{bookId}", hashed)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(req)))
                .andExpect(status().isOk());

        verify(devRecipeBookWriteService).renameBook(eq(RAW_USER_ID), eq(RAW_BOOK_ID), any(RenameRecipeBookRequest.class));
    }

    @Test
    @DisplayName("DELETE /api/dev/me/recipe-books/{hashId}: bookId HashID 디코딩 + 메시지 응답")
    void delete_decodesBookIdHashId() throws Exception {
        String hashed = hashids.encode(RAW_BOOK_ID);

        mockMvc.perform(delete("/api/dev/me/recipe-books/{bookId}", hashed))
                .andExpect(status().isOk());

        verify(devRecipeBookWriteService).deleteBook(RAW_USER_ID, RAW_BOOK_ID);
    }

    @Test
    @DisplayName("POST /api/dev/me/recipe-books/{hashId}/recipes: bookId 디코딩 + body recipeIds HashIdDeserializer로 raw Long 디코딩 (ArgumentCaptor 검증)")
    void addRecipes_decodesBookIdAndRecipeIdsInBody() throws Exception {
        String hashedBook = hashids.encode(RAW_BOOK_ID);
        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        // body는 HashID 문자열 배열 — HashIdDeserializer가 raw Long으로 변환되어야 함
        String bodyJson = "{\"recipeIds\":[\"" + hashedRecipe + "\"]}";
        given(devRecipeBookWriteService.addRecipesToBook(eq(RAW_USER_ID), eq(RAW_BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(1).skippedCount(0).build());

        mockMvc.perform(post("/api/dev/me/recipe-books/{bookId}/recipes", hashedBook)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(bodyJson))
                .andExpect(status().isOk());

        // ArgumentCaptor로 service에 전달된 raw Long recipeIds 직접 assert
        ArgumentCaptor<AddRecipesToBookRequest> reqCap = ArgumentCaptor.forClass(AddRecipesToBookRequest.class);
        verify(devRecipeBookWriteService).addRecipesToBook(eq(RAW_USER_ID), eq(RAW_BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).containsExactly(RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("DELETE /api/dev/me/recipe-books/{hashId}/recipes: bookId 디코딩 + body recipeIds 디코딩 (ArgumentCaptor 검증)")
    void removeRecipes_decodesBookIdAndRecipeIdsInBody() throws Exception {
        String hashedBook = hashids.encode(RAW_BOOK_ID);
        String hashedRecipe = hashids.encode(RAW_RECIPE_ID);
        String bodyJson = "{\"recipeIds\":[\"" + hashedRecipe + "\"]}";

        mockMvc.perform(delete("/api/dev/me/recipe-books/{bookId}/recipes", hashedBook)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(bodyJson))
                .andExpect(status().isOk());

        ArgumentCaptor<RemoveRecipesFromBookRequest> reqCap = ArgumentCaptor.forClass(RemoveRecipesFromBookRequest.class);
        verify(devRecipeBookWriteService).removeRecipesFromBook(eq(RAW_USER_ID), eq(RAW_BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).containsExactly(RAW_RECIPE_ID);
    }

    @Test
    @DisplayName("PUT /api/dev/me/recipe-books/order: 인증 principal 추출 + body bookIds HashIdDeserializer 디코딩 (ArgumentCaptor 검증)")
    void reorder_extractsAuthAndDelegates() throws Exception {
        long rawBook1 = 100L;
        long rawBook2 = 200L;
        String hash1 = hashids.encode(rawBook1);
        String hash2 = hashids.encode(rawBook2);
        String bodyJson = "{\"bookIds\":[\"" + hash1 + "\",\"" + hash2 + "\"]}";
        given(devRecipeBookWriteService.reorderBooks(eq(RAW_USER_ID), any(ReorderRecipeBooksRequest.class)))
                .willReturn(List.<RecipeBookResponse>of());

        mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders
                        .put("/api/dev/me/recipe-books/order")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(bodyJson))
                .andExpect(status().isOk());

        ArgumentCaptor<ReorderRecipeBooksRequest> reqCap = ArgumentCaptor.forClass(ReorderRecipeBooksRequest.class);
        verify(devRecipeBookWriteService).reorderBooks(eq(RAW_USER_ID), reqCap.capture());
        assertThat(reqCap.getValue().getBookIds()).containsExactly(rawBook1, rawBook2);
    }
}
