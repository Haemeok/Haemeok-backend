package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

@Repository
public interface RecipeBookItemRepository extends JpaRepository<RecipeBookItem, Long> {

    boolean existsByBookIdAndRecipeId(Long bookId, Long recipeId);

    /** 접근 가능한 레시피만 조회 (비공개가 아니거나 내 레시피). recipe.user도 함께 fetch. */
    @EntityGraph(attributePaths = {"recipe", "recipe.user"})
    @Query("""
      SELECT i FROM RecipeBookItem i
      JOIN i.recipe r
      WHERE i.book.id = :bookId
        AND (r.isPrivate = false OR r.user.id = :userId)
      ORDER BY i.createdAt DESC
      """)
    Slice<RecipeBookItem> findAccessibleByBookIdAndUserId(
            @Param("bookId") Long bookId,
            @Param("userId") Long userId,
            Pageable pageable);

    /** 접근 가능한 레시피 수 (폴더 상세에서 사용) */
    @Query("""
      SELECT COUNT(i) FROM RecipeBookItem i
      JOIN i.recipe r
      WHERE i.book.id = :bookId
        AND (r.isPrivate = false OR r.user.id = :userId)
      """)
    int countAccessibleByBookIdAndUserId(
            @Param("bookId") Long bookId,
            @Param("userId") Long userId);

    /** 레시피 삭제 시 영향받는 폴더별 아이템 수 조회 (count 보정용) */
    @Query("SELECT i.book.id, COUNT(i) FROM RecipeBookItem i WHERE i.recipe.id = :recipeId GROUP BY i.book.id")
    List<Object[]> countByRecipeIdGroupByBookId(@Param("recipeId") Long recipeId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeBookItem i WHERE i.book.id = :bookId AND i.recipe.id IN :recipeIds")
    int deleteByBookIdAndRecipeIdIn(
            @Param("bookId") Long bookId,
            @Param("recipeIds") List<Long> recipeIds);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeBookItem i WHERE i.book.id = :bookId")
    void deleteByBookId(@Param("bookId") Long bookId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeBookItem i WHERE i.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    /** 유저의 모든 폴더에서 해당 레시피를 삭제하고, 삭제된 폴더별 건수를 반환 */
    @Query("SELECT i.book.id, COUNT(i) FROM RecipeBookItem i WHERE i.book.user.id = :userId AND i.recipe.id = :recipeId GROUP BY i.book.id")
    List<Object[]> countByUserIdAndRecipeIdGroupByBookId(
            @Param("userId") Long userId,
            @Param("recipeId") Long recipeId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeBookItem i WHERE i.book.user.id = :userId AND i.recipe.id = :recipeId")
    int deleteAllByUserIdAndRecipeId(
            @Param("userId") Long userId,
            @Param("recipeId") Long recipeId);

    /** 유저가 저장한 레시피 ID 집합 (배치 조회용, source of truth) */
    @Query("SELECT DISTINCT i.recipe.id FROM RecipeBookItem i WHERE i.book.user.id = :userId AND i.recipe.id IN :recipeIds")
    Set<Long> findSavedRecipeIdsByUserIdAndRecipeIdIn(
            @Param("userId") Long userId,
            @Param("recipeIds") List<Long> recipeIds);

    /** 유저가 해당 레시피를 1개 이상 폴더에 저장했는지 (단건 조회용) */
    @Query("SELECT CASE WHEN COUNT(i) > 0 THEN true ELSE false END FROM RecipeBookItem i WHERE i.book.user.id = :userId AND i.recipe.id = :recipeId")
    boolean existsByUserIdAndRecipeId(
            @Param("userId") Long userId,
            @Param("recipeId") Long recipeId);

    /** 폴더에 속한 아이템 전체 조회 (폴더 삭제 시 favorite 동기화용) */
    @EntityGraph(attributePaths = {"recipe"})
    List<RecipeBookItem> findByBookId(@Param("bookId") Long bookId);

    /** 특정 폴더를 제외하고 유저가 해당 레시피를 저장한 폴더 수 */
    @Query("""
      SELECT COUNT(i) FROM RecipeBookItem i
      WHERE i.book.user.id = :userId
        AND i.recipe.id = :recipeId
        AND i.book.id != :excludeBookId
      """)
    long countByUserIdAndRecipeIdExcludingBook(
            @Param("userId") Long userId,
            @Param("recipeId") Long recipeId,
            @Param("excludeBookId") Long excludeBookId);

    /** 특정 유저의 폴더 중 해당 레시피가 담긴 폴더 목록 조회 */
    @Query("""
      SELECT i.book FROM RecipeBookItem i
      WHERE i.recipe.id = :recipeId
        AND i.book.user.id = :userId
      ORDER BY i.book.displayOrder ASC
      """)
    List<com.jdc.recipe_service.domain.entity.RecipeBook> findBooksByRecipeIdAndUserId(
            @Param("recipeId") Long recipeId,
            @Param("userId") Long userId);
}
