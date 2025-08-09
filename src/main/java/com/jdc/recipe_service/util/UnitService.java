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
    private List<String> allowedUnits;
    private Map<String,String> defaultUnitByIngredient;

    @PostConstruct
    public void loadUnits() {
        try (var reader = new BufferedReader(
                new InputStreamReader(
                        new ClassPathResource("units.csv").getInputStream(),
                        StandardCharsets.UTF_8))) {

            String header = reader.readLine();
            if (header == null) throw new IllegalStateException("units.csv가 비어있습니다.");

            Map<String,String> map = new LinkedHashMap<>();
            String raw;
            while ((raw = reader.readLine()) != null) {
                if (raw.isBlank()) continue;
                String line = raw.strip();
                if (line.startsWith("#")) continue;

                List<String> cols = parseCsvLine(line);
                if (cols.size() < 2) continue;

                String name = cols.get(0) != null ? cols.get(0).trim() : "";
                String unit = cols.get(1) != null ? cols.get(1).trim() : "";
                if (!name.isEmpty() && !unit.isEmpty()) {
                    map.putIfAbsent(name, unit);
                }
            }

            defaultUnitByIngredient = Map.copyOf(map);

            allowedUnits = defaultUnitByIngredient.values().stream()
                    .filter(Objects::nonNull)
                    .map(String::trim)
                    .distinct()
                    .toList();

        } catch (Exception e) {
            throw new IllegalStateException("units.csv 로드 실패", e);
        }
    }

    public String unitsAsString() {
        return String.join(", ", allowedUnits);
    }

    public String mappingAsStringFor(Collection<String> names) {
        if (names == null || names.isEmpty()) return "";
        return names.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .distinct()
                .map(n -> getDefaultUnit(n).map(u -> n + ":" + u))
                .flatMap(Optional::stream)
                .collect(Collectors.joining(", "));
    }

    public Optional<String> getDefaultUnit(String ingredientName) {
        return Optional.ofNullable(
                ingredientName == null ? null : defaultUnitByIngredient.get(ingredientName.trim())
        );
    }

    private static List<String> parseCsvLine(String line) {
        List<String> out = new ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean inQ = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                if (inQ && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    cur.append('"'); i++;
                } else {
                    inQ = !inQ;
                }
            } else if (c == ',' && !inQ) {
                out.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(c);
            }
        }
        out.add(cur.toString());
        return out;
    }
}
