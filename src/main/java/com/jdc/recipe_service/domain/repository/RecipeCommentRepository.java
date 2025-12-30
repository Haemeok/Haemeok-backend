package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeComment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RecipeCommentRepository extends JpaRepository<RecipeComment, Long> {

    Optional<RecipeComment> findByIdAndRecipeId(Long id, Long recipeId);

    @Query("""
        SELECT c
        FROM RecipeComment c
        LEFT JOIN FETCH c.user
        WHERE c.recipe.id = :recipeId
          AND c.parentComment IS NULL
        ORDER BY c.createdAt DESC
        """)
    List<RecipeComment> findAllWithRepliesAndUsers(@Param("recipeId") Long recipeId, Pageable pageable);

    @Query("""
        SELECT c
        FROM RecipeComment c
        LEFT JOIN FETCH c.user
        WHERE c.recipe.id = :recipeId
          AND c.parentComment IS NULL
        ORDER BY c.createdAt DESC
        """)
    List<RecipeComment> findTop3ByRecipeIdAndParentCommentIsNull(
            @Param("recipeId") Long recipeId,
            Pageable pageable
    );


    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeComment c WHERE c.recipe.id = :recipeId AND c.parentComment IS NOT NULL")
    void deleteRepliesByRecipeId(@Param("recipeId") Long recipeId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeComment c WHERE c.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    List<RecipeComment> findByRecipeId(Long recipeId);


    long countByRecipeId(Long recipeId);

    Page<RecipeComment> findByParentCommentId(Long parentId, Pageable pageable);

    @Query("SELECT c.id FROM RecipeComment c WHERE c.recipe.id = :recipeId AND c.parentComment IS NULL")
    List<Long> findIdsByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT c.id FROM RecipeComment c WHERE c.recipe.id = :recipeId AND c.parentComment IS NULL ORDER BY c.createdAt DESC")
    List<Long> findTopNIdsByRecipeId(
            @Param("recipeId") Long recipeId,
            Pageable pageable);
}
