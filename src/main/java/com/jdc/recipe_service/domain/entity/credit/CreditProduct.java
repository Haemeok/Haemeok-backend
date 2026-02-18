package com.jdc.recipe_service.domain.entity.credit;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "credit_products", indexes = {
        @Index(name = "idx_ls_variant_id", columnList = "ls_variant_id")
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class CreditProduct extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // 관리자용 상품명 (예: "10 Credit Pack", "Pro Plan Monthly")
    @Column(nullable = false)
    private String name;

    // 레몬스퀴즈의 Variant ID (이걸로 매핑)
    @Column(name = "ls_variant_id", nullable = false, unique = true)
    private Long lemonSqueezyVariantId;

    @Column(name = "ls_variant_uuid", nullable = false)
    private String lemonSqueezyVariantUuid;

    // 결제 시 지급할 크레딧 양 (예: 10, 50)
    @Column(nullable = false)
    private int creditAmount;

    // 보너스 크레딧 (옵션, 예: 10+1 행사 시 1)
    @Column(nullable = false)
    @Builder.Default
    private int bonusAmount = 0;

    // 이 상품의 타입 (구독형인지, 단건 충전인지)
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private CreditType type;

    // 유효기간 (일 단위, 0이면 무제한/5년)
    // 예: 정기권은 30일, 충전은 1825일(5년)
    @Column(name = "valid_days", nullable = false)
    private int validDays;

    // 판매 중 여부 (이걸로 상품 내렸다 올렸다 가능)
    @Column(name = "is_active", nullable = false)
    @Builder.Default
    private boolean isActive = true;

    @Column(name = "is_unlimited", nullable = false)
    @Builder.Default
    private boolean isUnlimited = false;
}