package com.jdc.recipe_service.domain.entity.user;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.product.SubscriptionStatus;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "user_subscriptions")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class UserSubscription extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false, unique = true)
    private User user;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    @Builder.Default
    private SubscriptionStatus status = SubscriptionStatus.NONE;

    @Column(name = "ls_variant_id")
    private String lemonSqueezyVariantId;

    @Column(name = "ls_subscription_id", unique = true)
    private Long lemonSqueezySubscriptionId;

    @Column(name = "customer_portal_url")
    private String customerPortalUrl;

    @Column(name = "next_billing_date")
    private LocalDateTime nextBillingDate;

    public void activate(Long subId, String variantId, String portalUrl, LocalDateTime nextBilling) {
        this.status = SubscriptionStatus.ACTIVE;
        this.lemonSqueezySubscriptionId = subId;
        this.lemonSqueezyVariantId = variantId;
        this.customerPortalUrl = portalUrl;
        this.nextBillingDate = nextBilling;
    }

    public void cancel() {
        this.status = SubscriptionStatus.CANCELED;
    }
}