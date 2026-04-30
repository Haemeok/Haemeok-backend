package com.jdc.recipe_service.dev.controller.search;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.dev.service.ingredient.DevIngredientSearchService;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.hashids.Hashids;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = DevSearchController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, DevSearchControllerTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_DEV_SEARCH_CONTROLLER",
        "app.hashids.min-length=8"
})
class DevSearchControllerTest {

    @TestConfiguration
    static class MeterRegistryTestConfig {
        @Bean
        MeterRegistry meterRegistry() {
            return new SimpleMeterRegistry();
        }
    }

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private Hashids hashids;

    @MockBean
    private DevIngredientSearchService ingredientSearchService;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private UserDetailsService userDetailsService;

    @MockBean
    private CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    @Test
    @DisplayName("GET /api/dev/search/ingredients: returns ingredient summaries with default unit in unchanged response shape")
    void searchIngredients_returnsDefaultUnitShape() throws Exception {
        long rawId = 10L;
        String hashedId = hashids.encode(rawId);
        IngredientSummaryDto dto = new IngredientSummaryDto(
                rawId,
                "potato",
                "vegetable",
                "potato.webp",
                "piece",
                null
        );
        given(ingredientSearchService.search(eq("potato"), any(), isNull(), any(Pageable.class)))
                .willReturn(new PageImpl<>(List.of(dto)));

        mockMvc.perform(get("/api/dev/search/ingredients")
                        .param("q", "potato")
                        .param("category", "VEGETABLE")
                        .param("page", "1")
                        .param("size", "80"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(hashedId))
                .andExpect(jsonPath("$.content[0].name").value("potato"))
                .andExpect(jsonPath("$.content[0].unit").value("piece"));

        ArgumentCaptor<Pageable> pageableCaptor = ArgumentCaptor.forClass(Pageable.class);
        verify(ingredientSearchService).search(eq("potato"), any(), isNull(), pageableCaptor.capture());
        assertThat(pageableCaptor.getValue().getPageNumber()).isEqualTo(1);
        assertThat(pageableCaptor.getValue().getPageSize()).isEqualTo(50);
    }
}
