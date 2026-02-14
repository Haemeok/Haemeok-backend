package com.jdc.recipe_service.domain.entity.credit;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.credit.CreditTransactionType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "credit_histories", indexes = {
        @Index(name = "idx_history_user_created", columnList = "user_id, created_at")
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class CreditHistory extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    // 변동 내역 (+10, -1 등)
    @Column(nullable = false)
    private int amount;

    // 거래 직후 잔액 (Total Balance) - 나중에 역추적할 때 필수
    @Column(name = "balance_after", nullable = false)
    private int balanceAfter;

    // 거래 종류 (충전, 사용, 환불 등)
    @Enumerated(EnumType.STRING)
    @Column(name = "transaction_type", nullable = false, length = 20)
    private CreditTransactionType transactionType;

    // 상세 설명 (예: "친구 초대 보상 (user_99)", "AI 레시피 생성")
    @Column(nullable = false)
    private String description;

    // 결제 ID (레몬스퀴즈 Order ID 등) - 없으면 null
    @Column(name = "transaction_id")
    private String transactionId;
}