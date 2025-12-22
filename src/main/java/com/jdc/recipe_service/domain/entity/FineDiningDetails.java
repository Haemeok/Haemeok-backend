package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.dto.recipe.SimpleComponentDto;
import com.jdc.recipe_service.domain.entity.converter.SimpleComponentListConverter;
import com.jdc.recipe_service.domain.entity.converter.StringListConverter;
import jakarta.persistence.*;
import lombok.*;

import java.util.List;

@Entity
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FineDiningDetails {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(columnDefinition = "TEXT")
    @Convert(converter = SimpleComponentListConverter.class)
    private List<SimpleComponentDto> components;

    private String platingVessel;

    @Column(columnDefinition = "TEXT")
    private String platingGuide;

    @Convert(converter = StringListConverter.class)
    private List<String> visualKeys;

    private String viewpoint;
    private String lighting;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id")
    private Recipe recipe;
}