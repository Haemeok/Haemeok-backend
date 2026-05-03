package com.jdc.recipe_service.domain.entity.article;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.*;

/**
 * 큐레이션 아티클이 참고한 레시피의 soft link.
 *
 * <p>화면 렌더링용 join 소스가 아니라, 어느 레시피들이 아티클 생성에 참고됐는지 audit하는 용도다.
 * recipe_id에는 FK를 두지 않는다 — 레시피가 hard delete된 뒤에도 참조 이력을 보존해야 하기 때문이다.
 * (chat_log.recipeId와 같은 audit 참조 패턴)
 */
@Entity
@Table(name = "curation_article_recipe_refs",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_curation_article_recipe_refs_article_recipe",
                columnNames = {"article_id", "recipe_id"}),
        indexes = {
                @Index(name = "idx_curation_article_recipe_refs_article",
                        columnList = "article_id, id"),
                @Index(name = "idx_curation_article_recipe_refs_recipe",
                        columnList = "recipe_id")
        })
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class CurationArticleRecipeRef extends BaseCreateTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "article_id", nullable = false)
    private CurationArticle article;

    @Column(name = "recipe_id", nullable = false)
    private Long recipeId;
}
