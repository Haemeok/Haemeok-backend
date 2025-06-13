package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.type.SaltinessPreference;
import com.jdc.recipe_service.domain.type.TagType;
import jakarta.persistence.*;
import lombok.*;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Entity
@Table(name = "user_survey")
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSurvey {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "user_id", nullable = false, unique = true)
    private User user;

    private Integer spiceLevel;

    @Enumerated(EnumType.STRING)
    private SaltinessPreference saltiness;

    private String allergy;

    private String dietType;

    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(
            name = "user_survey_tags",
            joinColumns = @JoinColumn(name = "user_id")
    )
    @Enumerated(EnumType.STRING)
    @Column(name = "tag_type")
    private Set<TagType> tags;

    public void updateFromDto(UserSurveyDto dto) {
        this.spiceLevel = dto.getSpiceLevel();
        this.saltiness   = dto.getSaltiness();
        this.allergy     = dto.getAllergy();
        this.dietType    = dto.getDietType();

        Set<TagType> newTags = dto.getTags().stream()
                .map(TagType::fromDisplayName)
                .collect(Collectors.toSet());

        if (this.tags == null) {
            this.tags = new HashSet<>();
        }
        this.tags.clear();
        this.tags.addAll(newTags);
    }
}
