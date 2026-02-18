package com.jdc.recipe_service.domain.repository.credit;

import com.jdc.recipe_service.domain.entity.credit.CreditHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CreditHistoryRepository extends JpaRepository<CreditHistory, Long> {
    Page<CreditHistory> findAllByUserIdOrderByCreatedAtDesc(Long userId, Pageable pageable);
    boolean existsByTransactionId(String transactionId);
}