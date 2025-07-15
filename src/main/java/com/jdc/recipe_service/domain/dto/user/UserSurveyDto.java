package com.jdc.recipe_service.domain.dto.user;

import lombok.*;

import java.util.Set;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSurveyDto {
    private Integer spiceLevel;
    private String allergy;
    private Set<String> tags;
}
