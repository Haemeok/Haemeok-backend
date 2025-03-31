package com.jdc.recipe_service.domain.dto;

import com.jdc.recipe_service.domain.entity.User;
import lombok.Data;
import java.time.LocalDateTime;

@Data
public class UserResponseDTO {
    private Long id;
    private String nickname;
    private String profileImage;
    private String introduction;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    //추후 삭제
    private String provider;

    public UserResponseDTO() {
        // Lombok이 생성해주지 않을 경우 수동으로 기본 생성자 추가
    }

    public UserResponseDTO(User user) {
        this.id = user.getId();
        this.nickname = user.getNickname();
        this.profileImage = user.getProfileImage();
        this.introduction = user.getIntroduction();
        this.createdAt = user.getCreatedAt();
        this.updatedAt = user.getUpdatedAt();
        this.provider = user.getProvider();
    }
}
