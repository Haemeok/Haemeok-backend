package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.type.Role;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * RefreshTokenRepository의 grace 회전 lookup 쿼리 shape을 고정한다.
 * - findByTokenForUpdate: UNIQUE 인덱스(uk_rt_token) 위에서 정확히 0/1 row
 * - findByPreviousTokenInGraceForUpdate: previous_token 매칭 + grace_until > now AND previous_token NOT NULL
 * - existsByPreviousToken: grace 만료 후 "옛 토큰 재전송" 관찰용 플래그
 * - findAllByTokenOrPreviousToken: logout이 grace 상태의 row까지 같이 revoke하게 하는 OR lookup
 *
 * <p>여기서는 "쿼리가 의도한 row를 고르는가"만 본다. 실제 lock semantics(PESSIMISTIC_WRITE)는
 * MySQL InnoDB 고유 동작에 의존하므로 RefreshTokenConcurrencyIT에서 Testcontainers로 따로 검증한다.
 * 본 테스트는 프로젝트 testing rule(`@DataJpaTest` + H2 기본)을 따른다.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class RefreshTokenRepositoryTest {

    @Autowired
    private EntityManager em;

    @Autowired
    private RefreshTokenRepository refreshTokenRepository;

    private User user;

    @BeforeEach
    void setUp() {
        user = User.builder()
                .provider("google")
                .oauthId("u-1")
                .nickname("테스터")
                .role(Role.USER)
                .build();
        em.persist(user);
        em.flush();
    }

    @Test
    @DisplayName("findByTokenForUpdate: token 컬럼이 정확히 일치하는 row를 반환한다")
    void findByTokenForUpdate_exactMatch() {
        RefreshToken row = persist("T1", null, null, futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository.findByTokenForUpdate("T1");

        assertThat(found).isPresent();
        assertThat(found.get().getId()).isEqualTo(row.getId());
    }

    @Test
    @DisplayName("findByTokenForUpdate: 존재하지 않는 token은 빈 결과")
    void findByTokenForUpdate_missing() {
        persist("T1", null, null, futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository.findByTokenForUpdate("T-UNKNOWN");

        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("findByPreviousTokenInGraceForUpdate: previousToken 일치 + grace_until 미래면 row를 반환한다")
    void grace_hit() {
        RefreshToken row = persist("T2", "T1", futureSeconds(30), futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository
                .findByPreviousTokenInGraceForUpdate("T1", LocalDateTime.now());

        assertThat(found).isPresent();
        assertThat(found.get().getId()).isEqualTo(row.getId());
    }

    @Test
    @DisplayName("findByPreviousTokenInGraceForUpdate: grace_until이 과거면 빈 결과 (grace 만료)")
    void grace_expired() {
        persist("T2", "T1", pastSeconds(5), futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository
                .findByPreviousTokenInGraceForUpdate("T1", LocalDateTime.now());

        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("findByPreviousTokenInGraceForUpdate: previousToken이 NULL인 row는 잡히지 않는다")
    void previousToken_null_skipped() {
        // previous_token IS NULL이면 어떤 :token 값으로도 매치되지 않아야 한다.
        persist("T1", null, null, futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository
                .findByPreviousTokenInGraceForUpdate("T1", LocalDateTime.now());

        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("findByPreviousTokenInGraceForUpdate: previous_token_grace_until이 NULL이면 빈 결과")
    void grace_until_null_skipped() {
        persist("T2", "T1", null, futureDays(7));

        Optional<RefreshToken> found = refreshTokenRepository
                .findByPreviousTokenInGraceForUpdate("T1", LocalDateTime.now());

        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("existsByPreviousToken: previous_token으로 등록된 적 있는 값은 true, 없는 값은 false")
    void existsByPreviousToken_reflectsHistory() {
        persist("T2", "T1", pastSeconds(5), futureDays(7));

        assertThat(refreshTokenRepository.existsByPreviousToken("T1")).isTrue();
        assertThat(refreshTokenRepository.existsByPreviousToken("never-issued")).isFalse();
    }

    @Test
    @DisplayName("findAllByTokenOrPreviousToken: 현재 토큰 일치 row를 잡는다")
    void logoutLookup_matchesByCurrentToken() {
        RefreshToken row = persist("T2", "T1", futureSeconds(30), futureDays(7));

        List<RefreshToken> rows = refreshTokenRepository.findAllByTokenOrPreviousToken("T2");

        assertThat(rows).extracting(RefreshToken::getId).containsExactly(row.getId());
    }

    @Test
    @DisplayName("findAllByTokenOrPreviousToken: previous_token만 일치해도 잡는다 (grace 상태 logout)")
    void logoutLookup_matchesByPreviousTokenEvenAfterGraceExpired() {
        // grace 만료 여부와 무관하게 revoke 대상이 되어야 한다 — logout은 "어떤 경우에도 지워라"가 자연스럽다.
        RefreshToken row = persist("T2", "T1", pastSeconds(5), futureDays(7));

        List<RefreshToken> rows = refreshTokenRepository.findAllByTokenOrPreviousToken("T1");

        assertThat(rows).extracting(RefreshToken::getId).containsExactly(row.getId());
    }

    @Test
    @DisplayName("findAllByTokenOrPreviousToken: 어느 쪽에도 안 걸리면 빈 리스트")
    void logoutLookup_returnsEmptyWhenNoMatch() {
        persist("T2", "T1", futureSeconds(30), futureDays(7));

        List<RefreshToken> rows = refreshTokenRepository.findAllByTokenOrPreviousToken("unrelated");

        assertThat(rows).isEmpty();
    }

    private RefreshToken persist(String token, String previousToken,
                                 LocalDateTime graceUntil, LocalDateTime expiredAt) {
        RefreshToken row = RefreshToken.builder()
                .user(user)
                .token(token)
                .previousToken(previousToken)
                .previousTokenGraceUntil(graceUntil)
                .expiredAt(expiredAt)
                .build();
        em.persist(row);
        em.flush();
        em.clear();
        return row;
    }

    private static LocalDateTime futureDays(long days) {
        return LocalDateTime.now().plusDays(days);
    }

    private static LocalDateTime futureSeconds(long seconds) {
        return LocalDateTime.now().plusSeconds(seconds);
    }

    private static LocalDateTime pastSeconds(long seconds) {
        return LocalDateTime.now().minusSeconds(seconds);
    }
}
