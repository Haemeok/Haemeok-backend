package com.jdc.recipe_service.domain.repository.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface ChatLogRepository extends JpaRepository<ChatLog, Long> {

    // Pro 컨텍스트용 — 같은 session_id의 최근 N턴, 시간 cutoff(안전망), 정상 답변만, DESC (서비스에서 ASC로 뒤집음).
    @Query("""
            SELECT c FROM ChatLog c
            WHERE c.userId = :userId
              AND c.recipeId = :recipeId
              AND c.sessionId = :sessionId
              AND c.errorMessage IS NULL
              AND c.createdAt >= :since
            ORDER BY c.createdAt DESC
            """)
    List<ChatLog> findRecentForContext(@Param("userId") Long userId,
                                       @Param("recipeId") Long recipeId,
                                       @Param("sessionId") String sessionId,
                                       @Param("since") LocalDateTime since,
                                       Pageable pageable);

    // UI 표시용 — paginated DESC, 시간 cutoff 없음, 정상 답변만.
    @Query("""
            SELECT c FROM ChatLog c
            WHERE c.userId = :userId
              AND c.recipeId = :recipeId
              AND c.errorMessage IS NULL
            ORDER BY c.createdAt DESC
            """)
    List<ChatLog> findForDisplay(@Param("userId") Long userId,
                                 @Param("recipeId") Long recipeId,
                                 Pageable pageable);
}
