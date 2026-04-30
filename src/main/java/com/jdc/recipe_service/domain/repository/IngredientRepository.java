package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Ingredient;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;


@Repository
public interface IngredientRepository
        extends JpaRepository<Ingredient, Long>,
        JpaSpecificationExecutor<Ingredient> {

    List<Ingredient> findAllByNameIn(List<String> names);

    @Query("SELECT i.name FROM Ingredient i WHERE i.name IN :names")
    Set<String> findAllNamesByNameIn(@Param("names") List<String> names);

    List<Ingredient> findTop20ByCoupangLinkIsNullOrderByIdAsc();

    /**
     * dev write path lookup용 — is_active=true 마스터만.
     * 비활성 ingredient가 매칭되어 dual-write에 박히는 것을 차단.
     */
    List<Ingredient> findAllByIsActiveTrue();
}
