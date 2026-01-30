package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

public interface UserRepository extends JpaRepository<User, Long> {

    @Modifying
    @Transactional
    @Query("DELETE FROM User u WHERE u.id = :id")
    void deleteByIdForce(@Param("id") Long id);

    Optional<User> findByProviderAndOauthId(String provider, String oauthId);

    Optional<User> findByNickname(String nickname);

    boolean existsByNickname(String nickname);
}