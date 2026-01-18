package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.type.QuotaType;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuotaPolicy {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Enumerated(EnumType.STRING)
    @Column(unique = true, nullable = false)
    private QuotaType quotaType;

    @Column(nullable = false)
    private int limitCount;

    private String description;
}