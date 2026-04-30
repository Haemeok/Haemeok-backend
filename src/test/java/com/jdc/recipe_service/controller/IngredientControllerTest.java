package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientNutritionPer100gDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientPairItemDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientUnitDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientUnitsBatchResponse;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientUnitsResponse;
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
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
    @DisplayName("GET /api/ingredients/{id}: decodes hashid and returns enriched detail response")
    void getIngredientDetail_returnsEnrichedDetail() throws Exception {
        long rawId = 42L;
        long recipeRawId = 100L;
        long authorRawId = 500L;
        long pairRawId = 200L;
        String hashedId = hashids.encode(rawId);

        RecipeSimpleDto recipe = RecipeSimpleDto.builder()
                .id(recipeRawId)
                .title("potato roast")
                .authorId(authorRawId)
                .build();
        IngredientDetailDto dto = IngredientDetailDto.builder()
                .id(rawId)
                .name("potato")
                .category("vegetable")
                .imageUrl("https://example.com/potato.webp")
                .coupangLink("https://link.coupang.com/a/potato")
                .nutritionPer100g(IngredientNutritionPer100gDto.builder()
                        .kcal(new BigDecimal("77.000"))
                        .carbohydrateG(new BigDecimal("17.500"))
                        .proteinG(new BigDecimal("2.000"))
                        .fatG(new BigDecimal("0.100"))
                        .sugarG(new BigDecimal("0.800"))
                        .sodiumMg(new BigDecimal("6.000"))
                        .build())
                .storageLocation("fridge")
                .storageTemperature("0~4C")
                .storageDuration("1~2 weeks")
                .storageNotes("keep dry")
                .goodPairs("Garlic / Unknown")
                .goodPairItems(List.of(
                        IngredientPairItemDto.builder()
                                .id(pairRawId)
                                .name("Garlic")
                                .imageUrl("https://example.com/garlic.webp")
                                .build(),
                        IngredientPairItemDto.builder()
                                .name("Unknown")
                                .build()))
                .badPairs(null)
                .badPairItems(List.of())
                .benefits("Rich in vitamin C")
                .seasonMonths(List.of(6, 7, 8))
                .recommendedCookingMethods("roast / fry")
                .recipes(List.of(recipe))
                .build();
        given(ingredientService.findDetailById(eq(rawId))).willReturn(dto);

        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(hashedId))
                .andExpect(jsonPath("$.name").value("potato"))
                .andExpect(jsonPath("$.category").value("vegetable"))
                .andExpect(jsonPath("$.coupangLink").value("https://link.coupang.com/a/potato"))
                .andExpect(jsonPath("$.nutritionPer100g.kcal").value(77.000))
                .andExpect(jsonPath("$.nutritionPer100g.carbohydrateG").value(17.500))
                .andExpect(jsonPath("$.storageLocation").value("fridge"))
                .andExpect(jsonPath("$.goodPairItems[0].id").value(hashids.encode(pairRawId)))
                .andExpect(jsonPath("$.goodPairItems[0].name").value("Garlic"))
                .andExpect(jsonPath("$.goodPairItems[0].imageUrl").value("https://example.com/garlic.webp"))
                .andExpect(jsonPath("$.goodPairItems[1].id").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.goodPairItems[1].name").value("Unknown"))
                .andExpect(jsonPath("$.benefits").value("Rich in vitamin C"))
                .andExpect(jsonPath("$.seasonMonths[0]").value(6))
                .andExpect(jsonPath("$.seasonMonths[1]").value(7))
                .andExpect(jsonPath("$.seasonMonths[2]").value(8))
                .andExpect(jsonPath("$.recommendedCookingMethods").value("roast / fry"))
                .andExpect(jsonPath("$.recipes.length()").value(1))
                .andExpect(jsonPath("$.recipes[0].id").value(hashids.encode(recipeRawId)))
                .andExpect(jsonPath("$.recipes[0].authorId").value(hashids.encode(authorRawId)));
    }

    @Test
    @DisplayName("GET /api/ingredients/{id}: nullable fields remain present")
    void getIngredientDetail_nullStorageFields_fieldsStillPresent() throws Exception {
        long rawId = 7L;
        String hashedId = hashids.encode(rawId);

        IngredientDetailDto dto = IngredientDetailDto.builder()
                .id(rawId)
                .name("celery")
                .category("vegetable")
                .imageUrl("https://example.com/celery.webp")
                .recipes(List.of())
                .build();
        given(ingredientService.findDetailById(eq(rawId))).willReturn(dto);

        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.storageLocation").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.recommendedCookingMethods").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.nutritionPer100g").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.benefits").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.seasonMonths").value(Matchers.nullValue()))
                .andExpect(jsonPath("$.recipes").isArray())
                .andExpect(jsonPath("$.recipes.length()").value(0));
    }

    @Test
    @DisplayName("GET /api/ingredients/{id}: maps INGREDIENT_NOT_FOUND to 404")
    void getIngredientDetail_notFound_returns404() throws Exception {
        long rawId = 999L;
        String hashedId = hashids.encode(rawId);
        given(ingredientService.findDetailById(eq(rawId)))
                .willThrow(new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

        mockMvc.perform(get("/api/ingredients/{id}", hashedId))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value(ErrorCode.INGREDIENT_NOT_FOUND.getCode()));
    }

    @Test
    @DisplayName("GET /api/ingredients/{id}/units: decodes ingredient id and returns display units without unit ids")
    void getIngredientUnits_returnsUnits() throws Exception {
        long ingredientId = 42L;
        String hashedIngredientId = hashids.encode(ingredientId);

        IngredientUnitsResponse response = IngredientUnitsResponse.builder()
                .ingredientId(ingredientId)
                .units(List.of(
                        IngredientUnitDto.builder()
                                .unit("개")
                                .gramsPerUnit(new BigDecimal("150.000"))
                                .isDefault(true)
                                .build(),
                        IngredientUnitDto.builder()
                                .unit("g")
                                .gramsPerUnit(new BigDecimal("1.000"))
                                .isDefault(false)
                                .build()
                ))
                .build();
        given(ingredientService.findUnitsByIngredientId(eq(ingredientId))).willReturn(response);

        mockMvc.perform(get("/api/ingredients/{id}/units", hashedIngredientId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ingredientId").value(hashedIngredientId))
                .andExpect(jsonPath("$.units.length()").value(2))
                .andExpect(jsonPath("$.units[0].id").doesNotExist())
                .andExpect(jsonPath("$.units[0].unit").value("개"))
                .andExpect(jsonPath("$.units[0].unitLabelKo").doesNotExist())
                .andExpect(jsonPath("$.units[0].gramsPerUnit").value(150.000))
                .andExpect(jsonPath("$.units[0].isDefault").value(true))
                .andExpect(jsonPath("$.units[1].id").doesNotExist())
                .andExpect(jsonPath("$.units[1].unit").value("g"))
                .andExpect(jsonPath("$.units[1].unitLabelKo").doesNotExist())
                .andExpect(jsonPath("$.units[1].gramsPerUnit").value(1.000))
                .andExpect(jsonPath("$.units[1].isDefault").value(false));
    }

    @Test
    @DisplayName("POST /api/ingredients/units/batch: decodes body ids and returns grouped units")
    void getIngredientUnitsBatch_returnsGroupedUnits() throws Exception {
        long potatoId = 42L;
        long garlicId = 43L;
        String potatoHash = hashids.encode(potatoId);
        String garlicHash = hashids.encode(garlicId);

        IngredientUnitsBatchResponse response = IngredientUnitsBatchResponse.builder()
                .items(List.of(
                        IngredientUnitsResponse.builder()
                                .ingredientId(potatoId)
                                .units(List.of(IngredientUnitDto.builder()
                                        .unit("개")
                                        .gramsPerUnit(new BigDecimal("150.000"))
                                        .isDefault(true)
                                        .build()))
                                .build(),
                        IngredientUnitsResponse.builder()
                                .ingredientId(garlicId)
                                .units(List.of(IngredientUnitDto.builder()
                                        .unit("쪽")
                                        .gramsPerUnit(new BigDecimal("5.000"))
                                        .isDefault(true)
                                        .build()))
                                .build()
                ))
                .build();
        given(ingredientService.findUnitsByIngredientIds(eq(List.of(potatoId, garlicId)))).willReturn(response);

        mockMvc.perform(post("/api/ingredients/units/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "ingredientIds": ["%s", "%s"]
                                }
                                """.formatted(potatoHash, garlicHash)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(2))
                .andExpect(jsonPath("$.items[0].ingredientId").value(potatoHash))
                .andExpect(jsonPath("$.items[0].units[0].id").doesNotExist())
                .andExpect(jsonPath("$.items[0].units[0].unit").value("개"))
                .andExpect(jsonPath("$.items[0].units[0].unitLabelKo").doesNotExist())
                .andExpect(jsonPath("$.items[1].ingredientId").value(garlicHash))
                .andExpect(jsonPath("$.items[1].units[0].id").doesNotExist())
                .andExpect(jsonPath("$.items[1].units[0].unit").value("쪽"))
                .andExpect(jsonPath("$.items[1].units[0].unitLabelKo").doesNotExist());
    }

    @Test
    @DisplayName("POST /api/ingredients/units/batch: empty ingredientIds returns 400")
    void getIngredientUnitsBatch_emptyIds_returns400() throws Exception {
        mockMvc.perform(post("/api/ingredients/units/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "ingredientIds": []
                                }
                                """))
                .andExpect(status().isBadRequest());

        verifyNoInteractions(ingredientService);
    }

    @Test
    @DisplayName("GET /api/ingredients/names: ids query is required")
    void getIngredientNames_missingIds_returns400() throws Exception {
        mockMvc.perform(get("/api/ingredients/names"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value(ErrorCode.INVALID_INGREDIENT_REQUEST.getCode()));

        verifyNoInteractions(ingredientService);
    }

    @Test
    @DisplayName("GET /api/ingredients/names: passes hash ids to service and serializes ids")
    void getIngredientNames_returnsContent() throws Exception {
        long rawId = 42L;
        String hashedId = hashids.encode(rawId);
        given(ingredientService.findNamesByHashIds(eq(List.of(hashedId))))
                .willReturn(List.of(new com.jdc.recipe_service.domain.dto.ingredient.IngredientIdNameDto(rawId, "onion")));

        mockMvc.perform(get("/api/ingredients/names").param("ids", hashedId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(hashedId))
                .andExpect(jsonPath("$.content[0].name").value("onion"));

        verify(ingredientService).findNamesByHashIds(eq(List.of(hashedId)));
    }
}
