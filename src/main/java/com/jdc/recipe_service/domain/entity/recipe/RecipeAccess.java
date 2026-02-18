package com.jdc.recipe_service.domain.entity.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.recipe.RecipeAccessRole;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(
        name = "recipe_access",
        uniqueConstraints = {
                @UniqueConstraint(name = "uq_recipe_access_user_recipe", columnNames = {"user_id", "recipe_id"})
        },
        indexes = {
                @Index(name = "idx_recipe_access_user_created", columnList = "user_id, created_at"),
                @Index(name = "idx_recipe_access_recipe", columnList = "recipe_id")
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeAccess extends BaseTimeEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    private RecipeAccessRole role;
}