package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "recipe_book_items",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_recipe_book_items_book_recipe",
                columnNames = {"book_id", "recipe_id"}),
        indexes = @Index(name = "idx_recipe_book_items_book_created",
                columnList = "book_id, created_at DESC"))
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeBookItem extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "book_id", nullable = false)
    private RecipeBook book;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;
}
