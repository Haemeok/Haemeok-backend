package com.jdc.recipe_service.opensearch.indexingfailure;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IndexingFailureLogRepository extends JpaRepository<IndexingFailureLog, Long> {

    Optional<IndexingFailureLog> findByRecipeId(Long recipeId);

    List<IndexingFailureLog> findAll();

    void deleteByRecipeId(Long recipeId);
}