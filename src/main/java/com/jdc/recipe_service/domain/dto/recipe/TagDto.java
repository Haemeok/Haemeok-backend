package com.jdc.recipe_service.domain.dto.recipe;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
public class TagDto {
    private String code;    // 예: "LUNCHBOX"
    private String name;    // 예: "🍱 도시락"

    public TagDto(String code, String name) {
        this.code = code;
        this.name = name;
    }
}