package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeComment;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RecipeCommentRepository extends JpaRepository<RecipeComment, Long> {

    @EntityGraph(attributePaths = {"user"})
    List<RecipeComment> findByRecipeIdAndParentCommentIdIsNull(Long recipeId);

    @Query("SELECT c FROM RecipeComment c JOIN FETCH c.user " +
            "WHERE c.recipe.id = :recipeId AND c.parentComment IS NULL " +
            "ORDER BY c.createdAt DESC")
    List<RecipeComment> findTop3ByRecipeIdAndParentCommentIsNull(@Param("recipeId") Long recipeId, Pageable pageable);

    @Query("SELECT c FROM RecipeComment c JOIN FETCH c.user " +
            "WHERE c.recipe.id = :recipeId AND c.parentComment IS NULL " +
            "ORDER BY c.createdAt DESC")
    List<RecipeComment> findAllTopLevelComments(@Param("recipeId") Long recipeId);

    void deleteByRecipeId(Long recipeId);

    List<RecipeComment> findByRecipeId(Long recipeId);

    @Query("""
    SELECT COUNT(c)
    FROM RecipeComment c
    WHERE c.recipe.id = :recipeId
    AND (c.isDeleted = false OR SIZE(c.replies) > 0)
""")
    long countVisibleComments(@Param("recipeId") Long recipeId);

    List<RecipeComment> findByParentCommentIdOrderByCreatedAtAsc(Long parentId);
}
