package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(
        name = "refresh_tokens",
        indexes = {
                @Index(name = "idx_rt_user_created", columnList = "user_id, created_at"),
                // 아래 두 인덱스는 Flyway(V20260418_001, V20260418_002)에서 실제로 생성된다.
                // 여기 @Index 선언은 엔티티-스키마 일치를 드러내기 위한 문서용이며 DDL은 Flyway가 소유한다.
                @Index(name = "uk_rt_token", columnList = "token", unique = true),
                @Index(name = "idx_rt_prev_token", columnList = "previous_token")
        }
)
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RefreshToken extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 255)
    private String token;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Column(name = "expired_at", nullable = false)
    private LocalDateTime expiredAt;

    // 직전 회전에서 이 row가 들고 있던 token 값. Android WebView CookieManager가
    // 새 refresh 쿠키를 디스크에 flush하기 전에 앱이 죽는 창을 메우기 위한 "유예 토큰"이다.
    // grace 윈도우 안에 들어온 옛 토큰을 현재 토큰과 동등 처리하는 lookup 키로 쓴다.
    @Column(name = "previous_token", length = 255)
    private String previousToken;

    // previousToken이 유효한 마지막 시각. 이 시점이 지나면 옛 토큰은 invalid로 취급한다.
    @Column(name = "previous_token_grace_until")
    private LocalDateTime previousTokenGraceUntil;
    //createdat은 BaseTimeEntity에 상속됨
}
