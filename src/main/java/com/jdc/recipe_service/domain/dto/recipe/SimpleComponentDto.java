package com.jdc.recipe_service.domain.dto.recipe;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public class SimpleComponentDto {
    private String role;
    private String name;
    private String description;
    private List<String> process;
}
