package com.jdc.recipe_service.opensearch.indexingfailure;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.Instant;

@Entity
@Table(name = "indexing_failure_log")
@Getter
@NoArgsConstructor
public class IndexingFailureLog {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "recipe_id", unique = true, nullable = false)
    private Long recipeId;

    @CreationTimestamp
    @Column(name = "fail_time", nullable = false, updatable = false)
    private Instant failTime;

    public IndexingFailureLog(Long recipeId) {
        this.recipeId = recipeId;
    }
}