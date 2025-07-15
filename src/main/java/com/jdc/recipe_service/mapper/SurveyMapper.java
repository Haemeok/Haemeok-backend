package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.UserSurvey;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.TagType;

import java.util.stream.Collectors;

public class SurveyMapper {

    public static UserSurveyDto toDto(UserSurvey entity) {
        return UserSurveyDto.builder()
                .spiceLevel(entity.getSpiceLevel())
                .allergy(entity.getAllergy())
                .tags(
                        entity.getTags().stream()
                                .map(TagType::getDisplayName)
                                .collect(Collectors.toSet())
                )
                .build();
    }

    public static UserSurvey toEntity(User user, UserSurveyDto dto) {
        return UserSurvey.builder()
                .user(user)
                .spiceLevel(dto.getSpiceLevel())
                .allergy(dto.getAllergy())
                .tags(
                        dto.getTags().stream()
                                .map(TagType::fromDisplayName)
                                .collect(Collectors.toSet())
                )
                .build();
    }
}
