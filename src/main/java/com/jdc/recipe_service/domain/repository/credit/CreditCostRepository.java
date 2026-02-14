package com.jdc.recipe_service.domain.repository.credit;

import com.jdc.recipe_service.domain.entity.credit.CreditCostEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CreditCostRepository extends JpaRepository<CreditCostEntity, Long> {
    Optional<CreditCostEntity> findByCode(String code);
}