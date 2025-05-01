package com.jdc.recipe_service.domain.type;

import java.util.Arrays;

public enum DishType {
    FRYING("볶음"),
    SOUP_STEW("국/찌개/탕"),
    GRILL("구이"),
    SALAD("무침/샐러드"),
    FRIED_PAN("튀김/부침"),
    STEAMED_BRAISED("찜/조림"),
    OVEN("오븐요리"),
    RAW("생식/회"),
    PICKLE("절임/피클류"),
    RICE_NOODLE("밥/면/파스타"),
    DESSERT("디저트/간식류");

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
