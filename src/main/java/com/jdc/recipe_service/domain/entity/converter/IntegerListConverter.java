package com.jdc.recipe_service.domain.entity.converter;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * {@code List<Integer>} ↔ JSON 문자열 변환 JPA AttributeConverter.
 *
 * <p>{@link StringListConverter} 패턴과 동일 — VARCHAR 컬럼에 JSON array 직렬화 형태로 저장.
 * 예: {@code [1,3,5,11]}.
 */
@Slf4j
@Converter
public class IntegerListConverter implements AttributeConverter<List<Integer>, String> {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public String convertToDatabaseColumn(List<Integer> attribute) {
        if (attribute == null || attribute.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(attribute);
        } catch (Exception e) {
            log.error("Error converting List<Integer> to JSON", e);
            throw new RuntimeException("JSON conversion error", e);
        }
    }

    @Override
    public List<Integer> convertToEntityAttribute(String dbData) {
        if (dbData == null || dbData.isBlank()) {
            return null;
        }
        try {
            return objectMapper.readValue(dbData, new TypeReference<List<Integer>>() {});
        } catch (Exception e) {
            log.error("Error converting JSON to List<Integer>", e);
            throw new RuntimeException("JSON reading error", e);
        }
    }
}
