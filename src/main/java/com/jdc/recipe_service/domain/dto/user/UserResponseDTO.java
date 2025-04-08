package com.jdc.recipe_service.domain.dto.user;

import com.jdc.recipe_service.domain.entity.User;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserResponseDTO {
    private Long id;
    private String nickname;
    private String profileImage;
    private String introduction;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    //추후 삭제
    private String provider;

    public UserResponseDTO(User user) {
        this.id = user.getId();
        this.nickname = user.getNickname();
        this.profileImage = user.getProfileImage();
        this.introduction = user.getIntroduction();
        this.createdAt = user.getCreatedAt();
        this.updatedAt = user.getUpdatedAt();

    }
}
