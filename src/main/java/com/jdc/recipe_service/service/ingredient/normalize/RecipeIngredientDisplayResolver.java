package com.jdc.recipe_service.service.ingredient.normalize;

import org.springframework.stereotype.Component;

/**
 * recipe_ingredients 한 row의 표시용(name/quantity/unit) 우선순위 결정.
 *
 * <p><b>표시 우선순위 (raw 보존 정책)</b>
 * <ul>
 *   <li>name: rawName ?? customName ?? ingredientName</li>
 *   <li>quantity: rawQuantityText ?? legacyQuantity</li>
 *   <li>unit: rawUnitText ?? customUnit ?? legacyUnit</li>
 * </ul>
 *
 * <p>raw 보존 시대에는 사용자/AI가 입력한 원문이 가장 정확한 의도이므로 가장 먼저 본다.
 * 백필 진행 중에 raw_*가 비어 있는 legacy row는 자동으로 customName/customUnit/legacy 컬럼으로
 * fallback된다.
 */
@Component
public class RecipeIngredientDisplayResolver {

    public DisplayLine resolve(LineDisplayInput input) {
        return new DisplayLine(
                firstNonBlank(input.rawName(), input.customName(), input.ingredientName()),
                firstNonBlank(input.rawQuantityText(), input.legacyQuantity()),
                firstNonBlank(input.rawUnitText(), input.customUnit(), input.legacyUnit())
        );
    }

    private static String firstNonBlank(String... values) {
        if (values == null) return null;
        for (String v : values) {
            if (v != null && !v.isBlank()) return v;
        }
        return null;
    }

    public record LineDisplayInput(
            String rawName,
            String customName,
            String ingredientName,
            String rawQuantityText,
            String legacyQuantity,
            String rawUnitText,
            String customUnit,
            String legacyUnit
    ) {}

    public record DisplayLine(String name, String quantity, String unit) {}
}
