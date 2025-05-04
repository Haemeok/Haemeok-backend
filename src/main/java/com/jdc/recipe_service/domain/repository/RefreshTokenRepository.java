package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long> {

    Optional<RefreshToken> findByToken(String token);

    List<RefreshToken> findByUserOrderByCreatedAtAsc(User user);

    // 전체 로그아웃
    @Modifying
    @Query("delete from RefreshToken t where t.user.id = :userId")
    void deleteByUserId(@Param("userId") Long userId);

    //만료 토큰 청소
    @Modifying
    @Query("delete from RefreshToken t where t.expiredAt < :now")
    void deleteAllByExpiredAtBefore(@Param("now") LocalDateTime now);
}
