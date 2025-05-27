package com.jdc.recipe_service.opensearch.keyword;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "search_keyword")
@Getter
@NoArgsConstructor @AllArgsConstructor
@Builder
public class SearchKeyword {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true, length = 100, nullable = false)
    private String keyword;

    @Column(nullable = false)
    private long count;

    public void increment() {
        this.count++;
    }

}