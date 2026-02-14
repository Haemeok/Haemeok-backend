package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "user_credits", indexes = {
        @Index(name = "idx_user_credit_priority", columnList = "user_id, credit_type, expires_at")
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class UserCredit extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Enumerated(EnumType.STRING)
    @Column(name = "credit_type", nullable = false, length = 20)
    private CreditType creditType;

    @Column(nullable = false)
    private int amount;

    @Column(name = "original_amount", nullable = false)
    private int originalAmount;

    @Column(name = "transaction_id", length = 100)
    private String transactionId;

    @Column(name = "expires_at", nullable = false)
    private LocalDateTime expiresAt;

    public void use(int usage) {
        if (this.amount < usage) {
            throw new IllegalArgumentException("잔액이 부족합니다.");
        }
        this.amount -= usage;
    }
}