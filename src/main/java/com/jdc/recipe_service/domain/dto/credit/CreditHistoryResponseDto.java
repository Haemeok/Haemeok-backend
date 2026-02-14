package com.jdc.recipe_service.domain.dto.credit;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.jdc.recipe_service.domain.entity.credit.CreditHistory;
import com.jdc.recipe_service.domain.type.credit.CreditTransactionType;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class CreditHistoryResponseDto {
    private Long id;
    private int amount;
    private int balanceAfter;
    private String description;
    private CreditTransactionType type;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime createdAt;

    public static CreditHistoryResponseDto from(CreditHistory entity) {
        return CreditHistoryResponseDto.builder()
                .id(entity.getId())
                .amount(entity.getAmount())
                .balanceAfter(entity.getBalanceAfter())
                .description(entity.getDescription())
                .type(entity.getTransactionType())
                .createdAt(entity.getCreatedAt())
                .build();
    }
}