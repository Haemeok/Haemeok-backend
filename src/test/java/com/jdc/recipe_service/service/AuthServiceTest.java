package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.auth.RefreshResult;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.oauth.AppleClientSecretGenerator;
import com.jdc.recipe_service.security.oauth.CustomOAuth2UserService;
import io.jsonwebtoken.JwtException;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;

import java.time.LocalDateTime;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.never;

@ExtendWith(MockitoExtension.class)
@DisplayName("AuthService#refresh 동작")
class AuthServiceTest {

    @Mock private ClientRegistrationRepository clientRegistrationRepository;
    @Mock private CustomOAuth2UserService customOAuth2UserService;
    @Mock private JwtTokenProvider jwtTokenProvider;
    @Mock private RefreshTokenRepository refreshTokenRepository;
    @Mock private OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> accessTokenResponseClient;
    @Mock private AppleClientSecretGenerator appleClientSecretGenerator;

    private MeterRegistry meterRegistry;
    private AuthService authService;

    @BeforeEach
    void setUp() {
        meterRegistry = new SimpleMeterRegistry();
        authService = new AuthService(
                clientRegistrationRepository,
                customOAuth2UserService,
                jwtTokenProvider,
                refreshTokenRepository,
                accessTokenResponseClient,
                appleClientSecretGenerator,
                meterRegistry
        );
    }

    private double counter(String result) {
        return meterRegistry.counter("auth_refresh_total", "result", result).count();
    }

    @Nested
    @DisplayName("JWT 선검증 실패")
    class JwtValidation {

        @Test
        @DisplayName("JWT 형식/서명이 유효하지 않으면 INVALID_REFRESH_TOKEN으로 던지고 jwt_invalid 카운터가 증가한다")
        void rejects_invalid_jwt() {
            given(jwtTokenProvider.validateToken("bad"))
                    .willThrow(new JwtException("유효하지 않은 토큰입니다."));

            assertThatThrownBy(() -> authService.refresh("bad"))
                    .isInstanceOf(CustomException.class)
                    .extracting(ex -> ((CustomException) ex).getErrorCode())
                    .isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN);

            assertThat(counter("jwt_invalid")).isEqualTo(1.0);
            then(refreshTokenRepository).should(never()).findByTokenForUpdate(any());
        }

        @Test
        @DisplayName("JWT가 만료되었으면 REFRESH_TOKEN_EXPIRED로 던지고 jwt_expired 카운터가 증가한다")
        void rejects_expired_jwt() {
            given(jwtTokenProvider.validateToken("stale"))
                    .willThrow(new JwtException("토큰이 만료되었습니다."));

            assertThatThrownBy(() -> authService.refresh("stale"))
                    .isInstanceOf(CustomException.class)
                    .extracting(ex -> ((CustomException) ex).getErrorCode())
                    .isEqualTo(ErrorCode.REFRESH_TOKEN_EXPIRED);

            assertThat(counter("jwt_expired")).isEqualTo(1.0);
        }
    }

    @Nested
    @DisplayName("정상 회전 경로")
    class RotatePath {

        @Test
        @DisplayName("현재 유효 토큰으로 조회되면 새 refresh로 회전하고 이전 토큰을 previousToken으로 접어넣는다")
        void rotates_and_stores_previous() {
            // given
            String oldToken = "T1";
            User user = User.builder().id(42L).build();
            RefreshToken row = RefreshToken.builder()
                    .id(1L)
                    .user(user)
                    .token(oldToken)
                    .expiredAt(LocalDateTime.now().plusDays(7))
                    .build();

            given(jwtTokenProvider.validateToken(oldToken)).willReturn(true);
            given(refreshTokenRepository.findByTokenForUpdate(oldToken)).willReturn(Optional.of(row));
            given(jwtTokenProvider.createAccessToken(user)).willReturn("ACCESS_NEW");
            given(jwtTokenProvider.createRefreshToken()).willReturn("T2");
            LocalDateTime newExpiry = LocalDateTime.now().plusDays(7);
            given(jwtTokenProvider.getRefreshTokenExpiryAsLocalDateTime()).willReturn(newExpiry);

            // when
            RefreshResult result = authService.refresh(oldToken);

            // then
            assertThat(result.getPath()).isEqualTo(RefreshResult.Path.ROTATED);
            assertThat(result.getUserId()).isEqualTo(42L);
            assertThat(result.getAccessToken()).isEqualTo("ACCESS_NEW");
            assertThat(result.getRefreshToken()).isEqualTo("T2");
            assertThat(result.getRefreshExpiredAt()).isEqualTo(newExpiry);

            assertThat(row.getToken()).isEqualTo("T2");
            assertThat(row.getPreviousToken()).isEqualTo("T1");
            assertThat(row.getPreviousTokenGraceUntil()).isNotNull();
            assertThat(row.getExpiredAt()).isEqualTo(newExpiry);

            assertThat(counter("rotated")).isEqualTo(1.0);
            then(refreshTokenRepository).should(never())
                    .findByPreviousTokenInGraceForUpdate(any(), any());
        }

        @Test
        @DisplayName("현재 토큰으로 조회된 row가 DB에서 만료 상태면 REFRESH_TOKEN_EXPIRED로 던진다")
        void expired_db_row_throws() {
            String oldToken = "T1";
            RefreshToken row = RefreshToken.builder()
                    .id(1L)
                    .user(User.builder().id(7L).build())
                    .token(oldToken)
                    .expiredAt(LocalDateTime.now().minusMinutes(1))
                    .build();

            given(jwtTokenProvider.validateToken(oldToken)).willReturn(true);
            given(refreshTokenRepository.findByTokenForUpdate(oldToken)).willReturn(Optional.of(row));

            assertThatThrownBy(() -> authService.refresh(oldToken))
                    .isInstanceOf(CustomException.class)
                    .extracting(ex -> ((CustomException) ex).getErrorCode())
                    .isEqualTo(ErrorCode.REFRESH_TOKEN_EXPIRED);

            assertThat(counter("expired")).isEqualTo(1.0);
            assertThat(row.getPreviousToken()).isNull();
        }
    }

    @Nested
    @DisplayName("grace 재전송 경로")
    class GracePath {

        @Test
        @DisplayName("현재 토큰 조회는 miss지만 grace 조회에서 잡히면 회전 없이 기존 refresh를 재송신한다")
        void grace_hit_returns_existing_token_without_rotation() {
            // given: Android 재전송 — 이미 회전된 row(T1→T2)에 T1이 다시 들어오는 상황
            String replayedOld = "T1";
            User user = User.builder().id(99L).build();
            LocalDateTime expiry = LocalDateTime.now().plusDays(7);
            LocalDateTime graceUntil = LocalDateTime.now().plusSeconds(20);
            RefreshToken row = RefreshToken.builder()
                    .id(1L)
                    .user(user)
                    .token("T2")
                    .previousToken("T1")
                    .previousTokenGraceUntil(graceUntil)
                    .expiredAt(expiry)
                    .build();

            given(jwtTokenProvider.validateToken(replayedOld)).willReturn(true);
            given(refreshTokenRepository.findByTokenForUpdate(replayedOld))
                    .willReturn(Optional.empty());
            given(refreshTokenRepository.findByPreviousTokenInGraceForUpdate(eq(replayedOld), any()))
                    .willReturn(Optional.of(row));
            given(jwtTokenProvider.createAccessToken(user)).willReturn("ACCESS_NEW");

            // when
            RefreshResult result = authService.refresh(replayedOld);

            // then: 기존 current 토큰(T2)이 그대로 내려가고 새 access만 발급된다
            assertThat(result.getPath()).isEqualTo(RefreshResult.Path.GRACE_REPLAY);
            assertThat(result.getAccessToken()).isEqualTo("ACCESS_NEW");
            assertThat(result.getRefreshToken()).isEqualTo("T2");
            assertThat(result.getRefreshExpiredAt()).isEqualTo(expiry);

            // row의 token/previous_token/만료는 건드리지 않는다 (idempotent)
            assertThat(row.getToken()).isEqualTo("T2");
            assertThat(row.getPreviousToken()).isEqualTo("T1");
            assertThat(row.getPreviousTokenGraceUntil()).isEqualTo(graceUntil);

            assertThat(counter("grace_replay")).isEqualTo(1.0);
            then(jwtTokenProvider).should(never()).createRefreshToken();
        }
    }

    @Nested
    @DisplayName("둘 다 miss")
    class Miss {

        @Test
        @DisplayName("현재/grace 모두 miss이고 이력에도 없으면 invalid 카운터로 INVALID_REFRESH_TOKEN을 던진다")
        void both_miss_without_history_throws_invalid() {
            String oldToken = "unknown";

            given(jwtTokenProvider.validateToken(oldToken)).willReturn(true);
            given(refreshTokenRepository.findByTokenForUpdate(oldToken))
                    .willReturn(Optional.empty());
            given(refreshTokenRepository.findByPreviousTokenInGraceForUpdate(eq(oldToken), any()))
                    .willReturn(Optional.empty());
            given(refreshTokenRepository.existsByPreviousToken(oldToken)).willReturn(false);

            assertThatThrownBy(() -> authService.refresh(oldToken))
                    .isInstanceOf(CustomException.class)
                    .extracting(ex -> ((CustomException) ex).getErrorCode())
                    .isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN);

            assertThat(counter("invalid")).isEqualTo(1.0);
            assertThat(counter("replay_suspected")).isEqualTo(0.0);
        }

        @Test
        @DisplayName("grace 만료 후 옛 토큰이 재전송되면 replay_suspected 카운터가 찍히고 INVALID_REFRESH_TOKEN을 던진다")
        void both_miss_with_history_increments_replay_suspected() {
            // 시나리오: 클라이언트가 T1→T2 회전 후 30초 + 약간 더 지나서 T1을 재전송
            //  - findByTokenForUpdate(T1) → empty (이미 회전됨)
            //  - findByPreviousTokenInGraceForUpdate(T1, now) → empty (grace 만료)
            //  - existsByPreviousToken(T1) → true (어딘가 row가 previous_token=T1을 갖고 있음)
            String oldToken = "T1";

            given(jwtTokenProvider.validateToken(oldToken)).willReturn(true);
            given(refreshTokenRepository.findByTokenForUpdate(oldToken))
                    .willReturn(Optional.empty());
            given(refreshTokenRepository.findByPreviousTokenInGraceForUpdate(eq(oldToken), any()))
                    .willReturn(Optional.empty());
            given(refreshTokenRepository.existsByPreviousToken(oldToken)).willReturn(true);

            assertThatThrownBy(() -> authService.refresh(oldToken))
                    .isInstanceOf(CustomException.class)
                    .extracting(ex -> ((CustomException) ex).getErrorCode())
                    .isEqualTo(ErrorCode.INVALID_REFRESH_TOKEN);

            // 응답 계약은 invalid와 같지만 메트릭 태그는 분리돼 관찰 가능해야 한다
            assertThat(counter("replay_suspected")).isEqualTo(1.0);
            assertThat(counter("invalid")).isEqualTo(0.0);
        }
    }
}
