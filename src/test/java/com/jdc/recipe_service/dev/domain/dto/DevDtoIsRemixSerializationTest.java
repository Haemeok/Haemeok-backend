package com.jdc.recipe_service.dev.domain.dto;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevMyRecipeSummaryDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleDto;
import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookItemResponse;
import com.jdc.recipe_service.dev.domain.dto.record.DevCookingRecordSummaryDto;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Dev DTO의 isRemix가 JSON 키 "isRemix"로만 직렬화되고 "remix"로 새지 않는지 잠근다 (Lombok+Jackson 이중 등록 회귀 방지).
 *
 * <p>id/recipeId/recordId는 HashIdSerializer가 정적 hashids 의존성을 가져 단위 테스트에서 NPE — 직렬화 키 검증이 목적이라 비워둔다.
 */
class DevDtoIsRemixSerializationTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    @DisplayName("DevMyRecipeSummaryDto: remix=true → JSON에 \"isRemix\":true (키 이름 \"remix\" 아님)")
    void devMyRecipeSummary_isRemixSerializesAsIsRemix() throws Exception {
        DevMyRecipeSummaryDto dto = DevMyRecipeSummaryDto.builder()
                .title("t").remix(true).build();

        JsonNode json = objectMapper.readTree(objectMapper.writeValueAsString(dto));

        assertThat(json.has("isRemix")).isTrue();
        assertThat(json.get("isRemix").asBoolean()).isTrue();
        assertThat(json.has("remix")).isFalse();
    }

    @Test
    @DisplayName("DevRecipeSimpleDto: remix=true → JSON에 \"isRemix\":true")
    void devRecipeSimple_isRemixSerializesAsIsRemix() throws Exception {
        DevRecipeSimpleDto dto = DevRecipeSimpleDto.builder()
                .title("t").remix(true).build();

        JsonNode json = objectMapper.readTree(objectMapper.writeValueAsString(dto));

        assertThat(json.has("isRemix")).isTrue();
        assertThat(json.get("isRemix").asBoolean()).isTrue();
        assertThat(json.has("remix")).isFalse();
    }

    @Test
    @DisplayName("DevRecipeBookItemResponse: remix=true → JSON에 \"isRemix\":true")
    void devRecipeBookItem_isRemixSerializesAsIsRemix() throws Exception {
        DevRecipeBookItemResponse dto = DevRecipeBookItemResponse.builder()
                .title("t").remix(true).build();

        JsonNode json = objectMapper.readTree(objectMapper.writeValueAsString(dto));

        assertThat(json.has("isRemix")).isTrue();
        assertThat(json.get("isRemix").asBoolean()).isTrue();
        assertThat(json.has("remix")).isFalse();
    }

    @Test
    @DisplayName("DevCookingRecordSummaryDto: remix=true → JSON에 \"isRemix\":true + visibility 키 존재")
    void devCookingRecordSummary_isRemixSerializesAsIsRemix() throws Exception {
        DevCookingRecordSummaryDto dto = DevCookingRecordSummaryDto.builder()
                .recipeTitle("t")
                .visibility("PUBLIC").remix(true)
                .build();

        JsonNode json = objectMapper.readTree(objectMapper.writeValueAsString(dto));

        assertThat(json.has("isRemix")).isTrue();
        assertThat(json.get("isRemix").asBoolean()).isTrue();
        assertThat(json.has("remix")).isFalse();
        assertThat(json.get("visibility").asText()).isEqualTo("PUBLIC");
        assertThat(json.has("listingStatus")).isFalse();
    }

    @Test
    @DisplayName("remix=false도 JSON에 명시 직렬화 (생략 안 됨)")
    void devDtos_isRemixFalseStillEmitted() throws Exception {
        DevMyRecipeSummaryDto a = DevMyRecipeSummaryDto.builder().remix(false).build();
        DevRecipeSimpleDto b = DevRecipeSimpleDto.builder().remix(false).build();
        DevRecipeBookItemResponse c = DevRecipeBookItemResponse.builder().remix(false).build();
        DevCookingRecordSummaryDto d = DevCookingRecordSummaryDto.builder().remix(false).build();

        for (Object dto : new Object[]{a, b, c, d}) {
            JsonNode json = objectMapper.readTree(objectMapper.writeValueAsString(dto));
            assertThat(json.has("isRemix"))
                    .as("%s", dto.getClass().getSimpleName()).isTrue();
            assertThat(json.get("isRemix").asBoolean()).isFalse();
        }
    }
}
