package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Ingredient;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;


@Repository
public interface IngredientRepository
        extends JpaRepository<Ingredient, Long>,
        JpaSpecificationExecutor<Ingredient> {

}
