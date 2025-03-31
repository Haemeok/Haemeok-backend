package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.User;
import org.mapstruct.Mapper;
import org.mapstruct.MappingTarget;
import org.mapstruct.NullValuePropertyMappingStrategy;

@Mapper(componentModel = "spring")
public interface UserMapper {

    User toEntity(UserRequestDTO dto);

    UserResponseDTO toDto(User user);

    // 일부 필드만 업데이트 (id나 role 등은 제외)
    @org.mapstruct.BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void updateEntityFromDto(UserRequestDTO dto, @MappingTarget User user);
}
