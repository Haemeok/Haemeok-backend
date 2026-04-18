package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.auth.RefreshResult;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.MySQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * refresh 동시성 보장 integration test.
 *
 * 목적: 같은 refresh token으로 두 요청이 동시에 들어왔을 때
 *  - 정확히 하나만 "회전(ROTATED)"을 성공시키고,
 *  - 나머지 하나는 "grace 재전송(GRACE_REPLAY)"으로 살아 돌아와야 한다.
 *
 * 왜 MySQL Testcontainer를 쓰는가: 이 시나리오는 PESSIMISTIC_WRITE가 UNIQUE 인덱스(uk_rt_token)
 * 위에서 row-level X-lock으로 내려가는 InnoDB 동작에 의존한다. H2는 locking semantics가 달라
 * 같은 invariant를 재현하지 못한다. 그래서 본 테스트는 Testcontainers를 쓴다.
 *
 * 주의: 본 테스트는 test 메서드에 @Transactional을 걸지 않는다. 두 스레드가 서로 다른
 * 트랜잭션에서 lock을 경쟁해야 동시성 의미가 살아나기 때문이다.
 */
@SpringBootTest(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop"
})
@ActiveProfiles("test")
@Tag("concurrency")
@Testcontainers
@TestPropertySource(properties = {
        "spring.autoconfigure.exclude=" +
                "org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration," +
                "org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration"
})
class RefreshTokenConcurrencyIT {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private AuthService authService;

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RefreshTokenRepository refreshTokenRepository;

    private User user;
    private String initialToken;

    @BeforeEach
    void setUp() {
        user = userRepository.save(User.builder()
                .provider("google")
                .oauthId("concurrency-" + System.nanoTime())
                .nickname("동시성유저")
                .role(Role.USER)
                .build());

        initialToken = jwtTokenProvider.createRefreshToken();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(user)
                .token(initialToken)
                .expiredAt(jwtTokenProvider.getRefreshTokenExpiryAsLocalDateTime())
                .build());
    }

    @AfterEach
    void tearDown() {
        refreshTokenRepository.deleteByUserId(user.getId());
        userRepository.delete(user);
    }

    @Test
    @DisplayName("동일 토큰으로 병렬 refresh가 들어오면 ROTATED 1회 + GRACE_REPLAY 1회로 갈라진다")
    void parallel_refresh_yields_one_rotated_and_one_grace_replay() throws Exception {
        int parties = 2;
        ExecutorService pool = Executors.newFixedThreadPool(parties);
        CountDownLatch startGate = new CountDownLatch(1);

        try {
            Callable<RefreshResult> task = () -> {
                startGate.await();
                return authService.refresh(initialToken);
            };

            Future<RefreshResult> f1 = pool.submit(task);
            Future<RefreshResult> f2 = pool.submit(task);

            // 두 스레드가 submit되어 start gate 직전에 대기하도록 잠깐 스케줄 양보
            Thread.sleep(50);
            startGate.countDown();

            RefreshResult r1 = f1.get(15, TimeUnit.SECONDS);
            RefreshResult r2 = f2.get(15, TimeUnit.SECONDS);

            List<RefreshResult.Path> paths = List.of(r1.getPath(), r2.getPath());
            assertThat(paths)
                    .as("한 스레드는 정상 회전, 다른 스레드는 grace 재전송이어야 한다")
                    .containsExactlyInAnyOrder(
                            RefreshResult.Path.ROTATED,
                            RefreshResult.Path.GRACE_REPLAY
                    );

            // grace 경로가 반환한 refresh는 rotated 경로가 새로 만든 refresh와 같아야 한다
            RefreshResult rotated = r1.getPath() == RefreshResult.Path.ROTATED ? r1 : r2;
            RefreshResult grace   = r1.getPath() == RefreshResult.Path.GRACE_REPLAY ? r1 : r2;
            assertThat(grace.getRefreshToken())
                    .as("grace_replay는 rotated가 새로 발급한 current 토큰을 재송신해야 한다")
                    .isEqualTo(rotated.getRefreshToken());

            // DB 상태 검증: 한 row만 남아 있고 token은 새 값, previous_token은 initialToken.
            List<RefreshToken> rows = refreshTokenRepository.findByUserOrderByCreatedAtAsc(user);
            assertThat(rows).hasSize(1);
            RefreshToken stored = rows.get(0);
            assertThat(stored.getToken()).isEqualTo(rotated.getRefreshToken());
            assertThat(stored.getPreviousToken()).isEqualTo(initialToken);
            assertThat(stored.getPreviousTokenGraceUntil()).isAfter(LocalDateTime.now().minusMinutes(1));
        } finally {
            pool.shutdownNow();
        }
    }
}
