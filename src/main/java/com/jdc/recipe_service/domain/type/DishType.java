package com.jdc.recipe_service.domain.type;

import java.util.Arrays;

public enum DishType {
    frying("볶음"),
    soup_stew("국/찌개/탕"),
    grill("구이"),
    salad("무침/샐러드"),
    fried_pan("튀김/부침"),
    steamed_braised("찜/조림"),
    oven("오븐요리"),
    raw("생식/회"),
    pickle("절임/피클류"),
    rice_noodle("밥/면/파스타"),
    dessert("디저트/간식류");

    private final String displayName;

    DishType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public static DishType fromDisplayName(String displayName) {
        return Arrays.stream(DishType.values())
                .filter(type -> type.displayName.equals(displayName))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown dish type: " + displayName));
    }
}
