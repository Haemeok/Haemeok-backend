package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeBook;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RecipeBookRepository extends JpaRepository<RecipeBook, Long> {

    List<RecipeBook> findByUserIdOrderByDisplayOrderAsc(Long userId);

    Optional<RecipeBook> findByUserIdAndIsDefaultTrue(Long userId);

    boolean existsByUserIdAndIsDefaultTrue(Long userId);

    @Query("SELECT COALESCE(MAX(b.displayOrder), -1) FROM RecipeBook b WHERE b.user.id = :userId")
    int findMaxDisplayOrderByUserId(@Param("userId") Long userId);

    long countByUserId(Long userId);

    boolean existsByUserIdAndName(Long userId, String name);
}
