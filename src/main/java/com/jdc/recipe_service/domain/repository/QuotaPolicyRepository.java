package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.QuotaPolicy;
import com.jdc.recipe_service.domain.type.QuotaType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface QuotaPolicyRepository extends JpaRepository<QuotaPolicy, Long> {
    Optional<QuotaPolicy> findByQuotaType(QuotaType type);
}