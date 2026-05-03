package com.jdc.recipe_service.domain.entity.article;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "curation_articles", indexes = {
        @Index(name = "idx_curation_articles_status_published",
                columnList = "status, published_at DESC, id DESC"),
        @Index(name = "idx_curation_articles_category_published",
                columnList = "category, status, published_at DESC, id DESC")
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class CurationArticle extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true, length = 255)
    private String slug;

    @Column(nullable = false, length = 255)
    private String title;

    @Column(length = 500)
    private String description;

    @Column(name = "cover_image_key", length = 500)
    private String coverImageKey;

    @Column(name = "content_mdx", columnDefinition = "LONGTEXT", nullable = false)
    private String contentMdx;

    @Column(length = 50)
    private String category;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    @Builder.Default
    private ArticleStatus status = ArticleStatus.DRAFT;

    @Column(name = "generated_by", length = 100)
    private String generatedBy;

    @Column(name = "human_reviewed", nullable = false)
    @Builder.Default
    private boolean humanReviewed = false;

    @Column(name = "published_at")
    private LocalDateTime publishedAt;

    public void publish() {
        if (this.publishedAt == null) {
            this.publishedAt = LocalDateTime.now();
        }
        this.status = ArticleStatus.PUBLISHED;
    }

    public void archive() {
        this.status = ArticleStatus.ARCHIVED;
    }

    public void markReviewed() {
        this.humanReviewed = true;
    }

    public void updateContent(String title, String description, String contentMdx,
                              String coverImageKey, String category, String generatedBy) {
        this.title = title;
        this.description = description;
        this.contentMdx = contentMdx;
        this.coverImageKey = coverImageKey;
        this.category = category;
        this.generatedBy = generatedBy;
        // 본문이 바뀌면 이전 검수 승인은 무효다. 필요하면 다시 markReviewed()를 호출한다.
        this.humanReviewed = false;
    }
}
