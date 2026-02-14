package com.jdc.recipe_service.domain.entity.credit;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "credit_costs")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class CreditCostEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true)
    private String code;

    @Column(nullable = false)
    private int cost;

    private String description;
}