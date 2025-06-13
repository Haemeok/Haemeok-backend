package com.jdc.recipe_service.domain.dto.user;

import com.jdc.recipe_service.domain.type.SaltinessPreference;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.*;

import java.util.Set;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSurveyDto {
    private Integer spiceLevel;
    private SaltinessPreference saltiness;
    private String allergy;
    private String dietType;
    private Set<String> tags;
}
