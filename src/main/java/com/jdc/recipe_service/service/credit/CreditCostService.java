package com.jdc.recipe_service.service.credit;

import com.jdc.recipe_service.domain.entity.credit.CreditCostEntity;
import com.jdc.recipe_service.domain.repository.credit.CreditCostRepository;
import com.jdc.recipe_service.domain.type.credit.CreditCost;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class CreditCostService {

    private final CreditCostRepository creditCostRepository;

    @Transactional(readOnly = true)
    public int getCost(CreditCost type) {
        return creditCostRepository.findByCode(type.name())
                .map(CreditCostEntity::getCost)
                .orElseThrow(() -> new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "가격 정보를 찾을 수 없습니다: " + type.name()));
    }
}