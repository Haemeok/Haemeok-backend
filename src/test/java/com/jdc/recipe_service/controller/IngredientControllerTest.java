package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import com.jdc.recipe_service.service.IngredientService;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.hamcrest.Matchers;
import org.hashids.Hashids;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = IngredientController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import({HashIdConfig.class, IngredientControllerTest.MeterRegistryTestConfig.class})
@TestPropertySource(properties = {
        "app.hashids.salt=TEST_SALT_FOR_INGREDIENT_CONTROLLER",
        "app.hashids.min-length=8"
})
class IngredientControllerTest {

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
    private IngredientService ingredientService;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private UserDetailsService userDetailsService;

    @MockBean
    private CustomAuthenticationEntryPoint customAuthenticationEntryPoint;

    @Test
    @DisplayName("GET /api/ingredients/{id}: hashid 디코딩 후 service에 Long id 전달, JSON은 hashid/필드 순서대로 반환")
    void getIngredientDetail_returnsHashedIdAndStorageMethodField() throws Exception {
        // given
        long rawId = 42L;
        long recipeRawId = 100L;
        long authorRawId = 500L;
        String hashedId = hashids.encode(rawId);

        RecipeSimpleDto recipe = RecipeSimpleDto.builder()
                .id(recipeRawId)
                .title("대파닭구이")
                .authorId(authorRawId)
                .build();
        IngredientDetailDto dto = IngredientDetailDto.builder()
                .id(rawId)
                .name("대파")
                .category("채소")
                .imageUrl("https://example.com/대파.webp")
                .storageLocation("냉장")
                .storageTemperature("0~4℃")
                .storageDuration("1~2주")
                .storageNotes("습기 주의")
                .goodPairs("돼지고기 / 마늘")
                .badPairs(null)
                .recommendedCookingMethods("구이 / 볶음")
                .recipes(List.of(recipe))
                .build();
        given(ingredientService.findDetailById(eq(rawId))).willReturn(dto);

        // when & then
        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(hashedId))
                .andExpect(jsonPath("$.name").value("대파"))
                .andExpect(jsonPath("$.category").value("채소"))
                .andExpect(jsonPath("$.storageLocation").value("냉장"))
                .andExpect(jsonPath("$.recommendedCookingMethods").value("구이 / 볶음"))
                .andExpect(jsonPath("$.recipes.length()").value(1))
                .andExpect(jsonPath("$.recipes[0].id").value(hashids.encode(recipeRawId)))
                .andExpect(jsonPath("$.recipes[0].authorId").value(hashids.encode(authorRawId)));
    }

    @Test
    @DisplayName("GET /api/ingredients/{id}: 보관 정보가 모두 null이어도 응답에 필드가 존재한다(NON_NULL 미적용 계약)")
    void getIngredientDetail_nullStorageFields_fieldsStillPresent() throws Exception {
        // given
        long rawId = 7L;
        String hashedId = hashids.encode(rawId);

        IngredientDetailDto dto = IngredientDetailDto.builder()
                .id(rawId)
                .name("셀러리")
                .category("채소")
                .imageUrl("https://example.com/셀러리.webp")
                .recipes(List.of())
                .build();
        given(ingredientService.findDetailById(eq(rawId))).willReturn(dto);

        // when & then
        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.storageLocation").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.recommendedCookingMethods").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.recipes").isArray())
                .andExpect(jsonPath("$.recipes.length()").value(0));
    }

    @Test
    @DisplayName("GET /api/ingredients/{id}: 서비스가 INGREDIENT_NOT_FOUND CustomException 던지면 HTTP 404 + 에러 코드 매핑")
    void getIngredientDetail_notFound_returns404() throws Exception {
        // given
        long rawId = 999L;
        String hashedId = hashids.encode(rawId);
        given(ingredientService.findDetailById(eq(rawId)))
                .willThrow(new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

        // when & then
        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value(ErrorCode.INGREDIENT_NOT_FOUND.getCode()));
    }
}
