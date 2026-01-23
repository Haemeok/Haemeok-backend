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
    Optional<RefreshToken> findByUser(User user);

    List<RefreshToken> findByUserOrderByCreatedAtAsc(User user);

    @Modifying
    @Query("delete from RefreshToken t where t.user.id = :userId")
    void deleteByUserId(@Param("userId") Long userId);

    @Modifying
    @Query("delete from RefreshToken t where t.expiredAt < :now")
    void deleteAllByExpiredAtBefore(@Param("now") LocalDateTime now);
}
