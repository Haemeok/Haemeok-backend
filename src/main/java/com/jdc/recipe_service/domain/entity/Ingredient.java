package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "ingredients", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"name"})
})
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class Ingredient {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, length = 100)
    private String name;

    @Column(length = 50)
    private String category;

    @Column(name = "image_url", nullable = true, length = 255)
    private String imageUrl;

    @Column(name = "price")
    private Integer price;

    @Column
    private String unit;

    @Column(name = "calorie", nullable = true)
    private Double calorie;

    @Column(name = "english_name", length = 100)
    private String englishName;
}
