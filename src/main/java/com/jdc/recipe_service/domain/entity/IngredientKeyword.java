package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor
@Table(name = "ingredient_keywords", indexes = @Index(name = "idx_keyword", columnList = "keyword"))
public class IngredientKeyword {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id")
    private Ingredient ingredient;

    @Column(nullable = false)
    private String keyword;

    public IngredientKeyword(Ingredient ingredient, String keyword) {
        this.ingredient = ingredient;
        this.keyword = keyword;
    }
}