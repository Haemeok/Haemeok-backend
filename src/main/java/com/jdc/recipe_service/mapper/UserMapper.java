package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.user.*;
import com.jdc.recipe_service.domain.entity.User;

public class UserMapper {

    // 간단 정보 (레시피/프로필 등에서 사용)
    public static UserDto toSimpleDto(User user) {
        if (user == null) return null;
        return UserDto.builder()
                .id(user.getId())
                .nickname(user.getNickname())
                .profileImage(user.getProfileImage())
                .introduction(user.getIntroduction())
                .build();
    }

    // 댓글용 작성자 정보
    public static CommentUserDto toCommentUserDto(User user) {
        if (user == null) return null;
        return CommentUserDto.builder()
                .id(user.getId())
                .nickname(user.getNickname())
                .profileImage(user.getProfileImage())
                .build();
    }

    // 프로필 수정용: nickname/profileImage/introduction만
    public static User toEntity(UserRequestDTO dto) {
        if (dto == null) return null;
        return User.builder()
                .nickname(dto.getNickname())
                .profileImage(dto.getProfileImage())
                .introduction(dto.getIntroduction())
                .build();
    }


    // 엔티티 → 응답용 전체 DTO
    public static UserResponseDTO toDto(User user) {
        if (user == null) return null;
        return UserResponseDTO.builder()
                .id(user.getId())
                .nickname(user.getNickname())
                .profileImage(user.getProfileImage())
                .introduction(user.getIntroduction())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .provider(user.getProvider())
                .build();
    }

    // 요청 DTO로 유저 일부 필드만 업데이트
    public static void updateEntityFromDto(UserRequestDTO dto, User user) {
        user.updateProfile(
                dto.getNickname() != null ? dto.getNickname() : user.getNickname(),
                dto.getProfileImage() != null ? dto.getProfileImage() : user.getProfileImage(),
                dto.getIntroduction() != null ? dto.getIntroduction() : user.getIntroduction()
        );
    }

}
