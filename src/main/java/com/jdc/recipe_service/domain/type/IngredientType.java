package com.jdc.recipe_service.domain.type;

public enum IngredientType {
    SEASONING("seasoning", "조미료/양념"),
    LEGUME_NUT("legume_nut", "콩/견과류"),
    DAIRY("dairy", "가공/유제품"),
    MEAT("meat", "고기"),
    GRAIN("grain", "곡물"),
    FRUIT("fruit", "과일"),
    OTHER("other", "기타"),
    NOODLE("noodle", "면"),
    BREAD("bread", "빵/떡"),
    BEVERAGE("beverage", "음료/주류"),
    VEGETABLE("vegetable", "채소"),
    SEAFOOD("seafood", "해산물");

    private final String code;
    private final String kor;

    IngredientType(String code, String kor) {
        this.code = code;
        this.kor = kor;
    }

    public String getCode() {
        return code;
    }

    public String getKor() {
        return kor;
    }

    public static IngredientType fromCode(String code) {
        for (IngredientType c : values()) {
            if (c.code.equalsIgnoreCase(code)) {
                return c;
            }
        }
        throw new IllegalArgumentException("Invalid category code: " + code);
    }
}
