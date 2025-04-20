package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Ingredient;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface IngredientRepository extends JpaRepository<Ingredient, Long> {

    Optional<Ingredient> findByNameIgnoreCase(String name);

    Page<Ingredient> findByCategoryIgnoreCase(String category, Pageable pageable);
}
