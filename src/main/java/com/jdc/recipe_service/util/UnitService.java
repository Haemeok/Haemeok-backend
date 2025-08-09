package com.jdc.recipe_service.util;

import jakarta.annotation.PostConstruct;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class UnitService {

    private Map<String, String> defaultUnitByIngredient;
    private List<String> allowedUnits;
    private String allowedUnitsCsv;

    private static String normalize(String s) {
        if (s == null) return null;
        return s.trim().toLowerCase(Locale.ROOT);
    }

    @PostConstruct
    public void loadUnits() {
        try (var reader = new BufferedReader(
                new InputStreamReader(
                        new ClassPathResource("units.csv").getInputStream(),
                        StandardCharsets.UTF_8))) {

            List<String> lines = reader.lines().skip(1).toList();
            Map<String, String> map = new LinkedHashMap<>();

            for (String raw : lines) {
                if (raw == null || raw.isBlank()) continue;
                String line = raw.strip();
                if (line.startsWith("#")) continue;

                String[] parts = line.split(",", 2);
                if (parts.length < 2) continue;

                String name = normalize(parts[0]);
                String unit = parts[1].trim();
                if (!name.isEmpty() && !unit.isEmpty()) {
                    map.putIfAbsent(name, unit);
                }
            }

            if (map.isEmpty()) throw new IllegalStateException("units.csv가 비어있습니다.");

            defaultUnitByIngredient = Map.copyOf(map);
            allowedUnits = defaultUnitByIngredient.values().stream().distinct().sorted().toList();
            allowedUnitsCsv = String.join(",", allowedUnits);

        } catch (Exception e) {
            throw new IllegalStateException("units.csv 로드 실패", e);
        }
    }

    public String mappingAsStringFor(Collection<String> names) {
        if (names == null || names.isEmpty()) return "";
        Set<String> seen = new HashSet<>();
        return names.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(n -> seen.add(n.toLowerCase()))
                .map(n -> getDefaultUnit(n).map(u -> n + ":" + u))
                .flatMap(Optional::stream)
                .collect(Collectors.joining(","));
    }

    public String unitsAsString() {
        return allowedUnitsCsv;
    }

    public Optional<String> getDefaultUnit(String ingredientName) {
        if (ingredientName == null) return Optional.empty();
        return Optional.ofNullable(defaultUnitByIngredient.get(normalize(ingredientName)));
    }
}
