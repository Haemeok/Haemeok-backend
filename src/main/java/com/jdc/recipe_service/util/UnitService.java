package com.jdc.recipe_service.util;

import jakarta.annotation.PostConstruct;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UnitService {
    private List<String> allowedUnits;
    private Map<String,String> defaultUnitByIngredient;

    private String marketInventoryString;

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

            StringBuilder sb = new StringBuilder();

            for (String line : lines) {
                String[] parts = line.split(",");
                if (parts.length >= 2) {
                    String name = parts[0].trim();
                    String unit = parts[1].trim();

                    String price = (parts.length > 2) ? parts[2].trim() : "0";
                    String calorie = (parts.length > 4) ? parts[4].trim() : "0";
                    String protein = (parts.length > 5) ? parts[5].trim() : "0";

                    sb.append(String.format("- %s (1%s당: %s원 | %skcal | 단백질 %sg)\n",
                            name, unit, price, calorie, protein));
                }
            }
            this.marketInventoryString = sb.toString();

        } catch (Exception e) {
            throw new IllegalStateException("units.csv 로드 실패", e);
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
        return Optional.ofNullable(defaultUnitByIngredient.get(ingredientName.trim()));
    }

    public Map<String,String> mappingByIngredient() {
        return Collections.unmodifiableMap(defaultUnitByIngredient);
    }

    public String getMarketInventoryString() {
        return this.marketInventoryString;
    }
}