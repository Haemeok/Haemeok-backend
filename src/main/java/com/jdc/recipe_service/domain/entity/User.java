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

    @Column(length = 100)
    private String email;

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

    @Column(name = "survey_completed", nullable = false)
    @Builder.Default
    private boolean surveyCompleted = false;

    @Column(name = "has_first_record", nullable = false)
    @Builder.Default
    private boolean hasFirstRecord = false;

    @Column(name = "youtube_token", nullable = false)
    @Builder.Default
    private int youtubeToken = 0;

    @Column(name = "ai_token", nullable = false)
    @Builder.Default
    private int aiToken = 0;

    @Column(name = "referral_code", length = 20, unique = true)
    private String referralCode;

    @Column(name = "monthly_invite_count", nullable = false)
    @Builder.Default
    private int monthlyInviteCount = 0;

    public void updateProfile(String nickname, String profileImage, String introduction) {
        if (nickname     != null) this.nickname     = nickname;
        if (profileImage != null) this.profileImage = profileImage;
        if (introduction != null) this.introduction = introduction;
    }
    public void updateEmail(String email) {
        if (email != null && !email.isBlank()) {
            this.email = email;
        }
    }
    public void updateProfileImageKey(String profileImageKey) {
        if (profileImageKey != null && !profileImageKey.isBlank()) {
            this.profileImageKey = profileImageKey;
        }
    }
    public void markSurveyCompleted() {
        this.surveyCompleted = true;
    }

    public void markFirstRecord() {this.hasFirstRecord = true;}

    public void addYoutubeToken(int amount) {
        this.youtubeToken += amount;
    }

    public void addAiToken(int amount) {
        this.aiToken += amount;
    }

    public boolean tryUseYoutubeToken() {
        if (this.youtubeToken > 0) {
            this.youtubeToken--;
            return true;
        }
        return false;
    }

    public boolean tryUseAiToken() {
        if (this.aiToken > 0) {
            this.aiToken--;
            return true;
        }
        return false;
    }

    public void increaseInviteCount() {
        this.monthlyInviteCount++;
    }

    public void resetMonthlyInviteCount() {
        this.monthlyInviteCount = 0;
    }

    public void generateReferralCode() {
        if (this.referralCode == null) {
            this.referralCode = java.util.UUID.randomUUID().toString().substring(0, 8).toUpperCase();
        }
    }
}