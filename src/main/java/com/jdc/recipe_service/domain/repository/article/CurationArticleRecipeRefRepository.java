package com.jdc.recipe_service.domain.repository.article;

import com.jdc.recipe_service.domain.entity.article.CurationArticleRecipeRef;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CurationArticleRecipeRefRepository extends JpaRepository<CurationArticleRecipeRef, Long> {

    @Query("""
            SELECT r.recipeId FROM CurationArticleRecipeRef r
            WHERE r.article.id = :articleId
            ORDER BY r.id ASC
            """)
    List<Long> findRecipeIdsByArticleId(@Param("articleId") Long articleId);

    /**
     * PUT 시 refs 전체 교체용. delete 후 saveAll 패턴으로 사용한다.
     * flushAutomatically: 같은 트랜잭션 안에서 saveAll INSERT가 DELETE 이후 실행되도록 보장.
     * clearAutomatically는 의도적으로 두지 않는다 — service.update()가 보유한 CurationArticle 엔티티가
     * detach되어 후속 saveAll의 ManyToOne 부모 참조가 stale해질 위험을 만든다.
     * 삭제된 ref 엔티티는 saveAll에서 새 ID로 INSERT되므로 stale 캐시 충돌 가능성은 없다.
     */
    @Modifying(flushAutomatically = true)
    @Query("DELETE FROM CurationArticleRecipeRef r WHERE r.article.id = :articleId")
    void deleteByArticleId(@Param("articleId") Long articleId);
}
