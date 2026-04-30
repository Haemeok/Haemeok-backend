package com.jdc.recipe_service.domain.entity.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(
        name = "recipe_youtube_extraction_info",
        uniqueConstraints = {
                @UniqueConstraint(name = "uq_youtube_extraction_recipe_id", columnNames = {"recipe_id"})
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeYoutubeExtractionInfo extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    @OnDelete(action = OnDeleteAction.CASCADE)
    private Recipe recipe;

    @Column(name = "has_subtitle", nullable = false)
    @Builder.Default
    private boolean hasSubtitle = false;

    @Column(name = "has_description_ingredient", nullable = false)
    @Builder.Default
    private boolean hasDescriptionIngredient = false;

    @Column(name = "has_comment_ingredient", nullable = false)
    @Builder.Default
    private boolean hasCommentIngredient = false;

    @Column(name = "used_gemini_analysis", nullable = false)
    @Builder.Default
    private boolean usedGeminiAnalysis = false;

    @Enumerated(EnumType.STRING)
    @Column(name = "evidence_level", nullable = false, length = 20)
    private EvidenceLevel evidenceLevel;

    @Column(name = "token_cost", nullable = false)
    private int tokenCost;
}
