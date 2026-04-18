package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long> {

    Optional<RefreshToken> findByToken(String token);
    Optional<RefreshToken> findByUser(User user);

    List<RefreshToken> findByUserOrderByCreatedAtAsc(User user);

    // 회전 이력 관찰용. 현재/grace lookup이 모두 miss된 뒤에 "grace 윈도우가 지난 옛 토큰의 재전송"인지
    // "한 번도 발급된 적 없는 토큰"인지 구분한다. 메트릭 태그(result=replay_suspected vs invalid)만
    // 분리하는 용도라 X-lock은 걸지 않는다.
    boolean existsByPreviousToken(String previousToken);

    // logout 경로 전용. 클라이언트가 회전 직전의 옛 refresh를 들고 logout을 눌러도 서버쪽 row가
    // 남아 "로그아웃했는데 세션이 살아있는" 상태가 되지 않도록, token/previous_token 양쪽을 같이 본다.
    // grace_until 만료는 보지 않는다: 로그아웃은 어차피 revoke가 목적이므로 previous_token으로만 매칭되는
    // 상황도 같이 지워주는 편이 안전하다. List 반환 이유: previous_token에는 UNIQUE 제약이 없어
    // 이론상 2개 row가 match될 수 있고, 그 경우 Optional 단건 조회는 NonUniqueResultException로 logout을
    // 깬다. logout 의도상 해당되는 row는 모두 revoke하는 게 자연스럽다.
    @Query("""
            select rt from RefreshToken rt
            where rt.token = :token
               or rt.previousToken = :token
            """)
    List<RefreshToken> findAllByTokenOrPreviousToken(@Param("token") String token);

    // refresh 회전 전용 lookup. 반드시 @Transactional 안에서만 호출한다.
    // UNIQUE 인덱스(uk_rt_token) 위에서 동작하므로 row-level X-lock만 잡고,
    // next-key lock이 인접 키로 번지지 않는다.
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select rt from RefreshToken rt where rt.token = :token")
    Optional<RefreshToken> findByTokenForUpdate(@Param("token") String token);

    // grace 윈도우 안의 옛 토큰을 "현재 row"로 끌어오는 lookup. previous_token은 평문 INDEX(idx_rt_prev_token)라
    // 같은 previous_token을 들고 있는 row가 드물게 둘 이상일 수도 있으므로, 호출자는 Optional 0/1 row 가정을 쓰되
    // 서비스 레이어에서 grace_until 유효성을 한 번 더 확인한다(읽기와 쓰기 사이 만료 대비).
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
            select rt from RefreshToken rt
            where rt.previousToken = :token
              and rt.previousTokenGraceUntil is not null
              and rt.previousTokenGraceUntil > :now
            """)
    Optional<RefreshToken> findByPreviousTokenInGraceForUpdate(
            @Param("token") String token,
            @Param("now") LocalDateTime now
    );

    @Modifying
    @Query("delete from RefreshToken t where t.user.id = :userId")
    void deleteByUserId(@Param("userId") Long userId);

    @Modifying
    @Query("delete from RefreshToken t where t.expiredAt < :now")
    void deleteAllByExpiredAtBefore(@Param("now") LocalDateTime now);
}
