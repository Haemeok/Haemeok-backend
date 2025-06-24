package com.jdc.recipe_service.util;

import jakarta.annotation.PostConstruct;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UnitService {
    private List<String> allowedUnits;
    private Map<String,String> defaultUnitByIngredient;

    @PostConstruct
    public void loadUnits() {
        try (var reader = new BufferedReader(
                new InputStreamReader(
                        new ClassPathResource("units.csv").getInputStream(),
                        StandardCharsets.UTF_8))) {

            List<String> lines = reader.lines().skip(1).toList();

            defaultUnitByIngredient = lines.stream()
                    .map(line -> line.split(","))
                    .filter(parts -> parts.length > 1)
                    .collect(Collectors.toMap(
                            parts -> parts[0].trim(),
                            parts -> parts[1].trim(),
                            (u1, u2) -> u1
                    ));

            allowedUnits = defaultUnitByIngredient.values().stream()
                    .distinct()
                    .toList();

        } catch (Exception e) {
            throw new RuntimeException("units.csv 로드 실패", e);
        }
    }

    public String unitsAsString() {
        return String.join(", ", allowedUnits);
    }

    public String mappingAsString() {
        return defaultUnitByIngredient.entrySet().stream()
                .map(e -> e.getKey() + ":" + e.getValue())
                .collect(Collectors.joining(", "));
    }

    public Optional<String> getDefaultUnit(String ingredientName) {
        return Optional.ofNullable(defaultUnitByIngredient.get(ingredientName));
    }
}
