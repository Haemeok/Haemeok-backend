package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.Role;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "users", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"provider", "oauth_id"}),
        @UniqueConstraint(columnNames = {"nickname"})
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class User extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, length = 50)
    private String provider;

    @Column(name = "oauth_id", nullable = false, length = 100)
    private String oauthId;

    @Column(length = 50)
    private String nickname;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    @Builder.Default
    private Role role = Role.USER;

    @Column(name = "profile_image", length = 255)
    private String profileImage;

    @Column(name = "profile_image_key", length = 255)
    private String profileImageKey;

    @Column(length = 255)
    private String introduction;


    public void updateProfile(String nickname, String profileImage, String introduction) {
        if (nickname     != null) this.nickname     = nickname;
        if (profileImage != null) this.profileImage = profileImage;
        if (introduction != null) this.introduction = introduction;
    }
    public void updateProfileImageKey(String profileImageKey) {
        if (profileImageKey != null && !profileImageKey.isBlank()) {
            this.profileImageKey = profileImageKey;
        }
    }
}