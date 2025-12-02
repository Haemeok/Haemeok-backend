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
                    .map(line -> line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1))
                    .filter(parts -> parts.length > 1)
                    .collect(Collectors.toMap(
                            parts -> parts[0].trim().replace("\"", ""), // 이름
                            parts -> parts[1].trim().replace("\"", ""), // 단위
                            (u1, u2) -> u1
                    ));

            allowedUnits = defaultUnitByIngredient.values().stream()
                    .distinct()
                    .toList();

            StringBuilder sb = new StringBuilder();

            for (String line : lines) {
                String[] parts = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);

                if (parts.length >= 10) {
                    String name = parts[0].trim().replace("\"", "");
                    String unit = parts[1].trim().replace("\"", "");

                    String price = parts[2].trim().replace("\"", "").replace(",", "");


                    String cal = parts[4].isBlank() ? "0" : parts[4].trim();
                    String carb = parts[5].isBlank() ? "0" : parts[5].trim();
                    String prot = parts[6].isBlank() ? "0" : parts[6].trim();
                    String fat = parts[7].isBlank() ? "0" : parts[7].trim();
                    String sugar = parts[8].isBlank() ? "0" : parts[8].trim();
                    String sodium = parts[9].isBlank() ? "0" : parts[9].trim();

                    sb.append(String.format("- %s (1%s당: %s원 | %skcal | 탄수%sg, 단백%sg, 지방%sg, 당%sg, 나트륨%smg)\n",
                            name, unit, price, cal, carb, prot, fat, sugar, sodium));
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