package com.jdc.recipe_service.domain.repository.user;

import com.jdc.recipe_service.domain.entity.user.UserCredit;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface UserCreditRepository extends JpaRepository<UserCredit, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT uc FROM UserCredit uc " +
            "WHERE uc.user.id = :userId " +
            "AND uc.amount > 0 " +
            "AND uc.expiresAt > CURRENT_TIMESTAMP " +
            "ORDER BY " +
            "CASE WHEN uc.creditType = 'SUBSCRIPTION' THEN 1 " +
            "     WHEN uc.creditType = 'BONUS' THEN 2 " +
            "     WHEN uc.creditType = 'BASIC' THEN 3 " +
            "     WHEN uc.creditType = 'PAID' THEN 4 END ASC, " +
            "uc.expiresAt ASC")
    List<UserCredit> findUseableCredits(@Param("userId") Long userId, Pageable pageable);

    Optional<UserCredit> findByTransactionId(String transactionId);

    @Query("SELECT COALESCE(SUM(uc.amount), 0) FROM UserCredit uc WHERE uc.user.id = :userId AND uc.amount > 0 AND uc.expiresAt > CURRENT_TIMESTAMP")
    Integer calculateTotalBalance(@Param("userId") Long userId);

    boolean existsByTransactionId(String transactionId);
}