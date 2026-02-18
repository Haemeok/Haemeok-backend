package com.jdc.recipe_service.domain.repository.credit;

import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface CreditProductRepository extends JpaRepository<CreditProduct, Long> {
    Optional<CreditProduct> findByLemonSqueezyVariantId(Long variantId);

    Optional<CreditProduct> findByName(String name);

    List<CreditProduct> findAllByTypeAndIsActiveTrueOrderByCreditAmountAsc(CreditType type);

    Optional<CreditProduct> findByLemonSqueezyVariantUuid(String uuid);
}