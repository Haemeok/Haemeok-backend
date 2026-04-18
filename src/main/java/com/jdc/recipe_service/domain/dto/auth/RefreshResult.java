package com.jdc.recipe_service.domain.dto.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * refresh 회전 결과. 컨트롤러가 쿠키 maxAge, 로그 태그, 메트릭을 구성할 수 있게
 * accessToken/refreshToken과 함께 refresh 만료 시각, 어떤 경로(rotated | grace_replay)
 * 로 생성되었는지를 함께 전달한다.
 */
@Getter
@Builder
@AllArgsConstructor
public class RefreshResult {

    public enum Path {
        // 정상 회전: 받은 토큰이 현재 유효 토큰이라 새 refresh를 발급함.
        ROTATED,
        // grace 재전송: Android CookieManager flush 누락 등으로 옛 토큰을 받았으나
        // grace 윈도우 안이라 기존 현재 토큰(+새 access)을 그대로 내려줌.
        GRACE_REPLAY
    }

    private final Long userId;
    private final String accessToken;
    private final String refreshToken;
    private final LocalDateTime refreshExpiredAt;
    private final Path path;
}
