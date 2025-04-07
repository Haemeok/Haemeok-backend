package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.type.TagType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "recipe_tags", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"recipe_id", "tag_id"})
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeTag {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @Enumerated(EnumType.STRING)
    @Column(name = "tag_name", length = 50, nullable = false)
    private TagType tag;

}
