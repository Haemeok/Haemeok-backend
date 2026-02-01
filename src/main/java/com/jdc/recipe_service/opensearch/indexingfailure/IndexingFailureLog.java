package com.jdc.recipe_service.opensearch.indexingfailure;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;


@Entity
@Table(name = "indexing_failure_log")
@Getter
@NoArgsConstructor
public class IndexingFailureLog extends BaseCreateTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "recipe_id", unique = true, nullable = false)
    private Long recipeId;

    public IndexingFailureLog(Long recipeId) {
        this.recipeId = recipeId;
    }
}