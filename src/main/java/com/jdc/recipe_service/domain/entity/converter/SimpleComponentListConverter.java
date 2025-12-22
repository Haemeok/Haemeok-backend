package com.jdc.recipe_service.domain.entity.converter;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.SimpleComponentDto;
import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import java.util.List;

@Converter
public class SimpleComponentListConverter implements AttributeConverter<List<SimpleComponentDto>, String> {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public String convertToDatabaseColumn(List<SimpleComponentDto> attribute) {
        try {
            return objectMapper.writeValueAsString(attribute);
        } catch (Exception e) {
            throw new RuntimeException("Error converting list to JSON", e);
        }
    }

    @Override
    public List<SimpleComponentDto> convertToEntityAttribute(String dbData) {
        try {
            if (dbData == null || dbData.isEmpty()) return null;
            return objectMapper.readValue(dbData, new TypeReference<>() {});
        } catch (Exception e) {
            throw new RuntimeException("Error reading JSON to list", e);
        }
    }
}
