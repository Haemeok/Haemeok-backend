package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Repository
public interface RecipeFavoriteRepository extends JpaRepository<RecipeFavorite, Long> {
    Optional<RecipeFavorite> findByUserIdAndRecipeId(Long userId, Long recipeId);

    @EntityGraph(attributePaths = {"recipe", "recipe.user"})
    Page<RecipeFavorite> findByUserId(Long userId, Pageable pageable);

    boolean existsByUserIdAndRecipeId(Long userId, Long recipeId);

    boolean existsByRecipeIdAndUserId(Long recipeId, Long userId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeFavorite rf WHERE rf.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT rf.recipe.id FROM RecipeFavorite rf WHERE rf.user.id = :userId AND rf.recipe.id IN :recipeIds")
    Set<Long> findRecipeIdsByUserIdAndRecipeIdIn(@Param("userId") Long userId, @Param("recipeIds") List<Long> recipeIds);

    @Query("SELECT rf FROM RecipeFavorite rf " +
            "JOIN FETCH rf.recipe r " +
            "JOIN FETCH r.user " +
            "WHERE rf.user.id = :userId " +
            "AND (r.isPrivate = false OR r.imageStatus = 'PENDING')")
    Page<RecipeFavorite> findMyFavoritesWithPending(@Param("userId") Long userId, Pageable pageable);
}