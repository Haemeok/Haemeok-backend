package com.jdc.recipe_service.domain.repository.chat;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * findRecentForContext의 session_id 격리 검증.
 * Pro 컨텍스트 reload는 (user, recipe, sessionId) 3-key + 시간 cutoff + error_message IS NULL 필터.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class ChatLogRepositoryTest {

    @Autowired private EntityManager em;
    @Autowired private ChatLogRepository repository;

    private static final Long USER_A = 100L;
    private static final Long USER_B = 200L;
    private static final Long RECIPE = 60L;
    private static final String SESSION_A = "session-a-uuid";
    private static final String SESSION_B = "session-b-uuid";
    private static final String SESSION_C = "session-c-uuid-no-rows";

    private void persistChat(Long userId, Long recipeId, String sessionId, String question, String answer) {
        ChatLog log = ChatLog.builder()
                .userId(userId)
                .recipeId(recipeId)
                .sessionId(sessionId)
                .question(question)
                .answer(answer)
                .intent("IN_SCOPE")
                .proCalled(true)
                .totalLatencyMs(100)
                .build();
        em.persist(log);
    }

    @Test
    @DisplayName("findRecentForContext: 같은 sessionId의 row만 반환 (session 격리 핵심)")
    void sessionIsolation() {
        persistChat(USER_A, RECIPE, SESSION_A, "Q-A1", "A-A1");
        persistChat(USER_A, RECIPE, SESSION_A, "Q-A2", "A-A2");
        persistChat(USER_A, RECIPE, SESSION_A, "Q-A3", "A-A3");
        persistChat(USER_A, RECIPE, SESSION_B, "Q-B1", "A-B1");
        persistChat(USER_A, RECIPE, SESSION_B, "Q-B2", "A-B2");
        em.flush();

        LocalDateTime since = LocalDateTime.now().minusHours(1);

        List<ChatLog> sessionAResult = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_A, since, PageRequest.of(0, 10));
        List<ChatLog> sessionBResult = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_B, since, PageRequest.of(0, 10));
        List<ChatLog> sessionCResult = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_C, since, PageRequest.of(0, 10));

        assertThat(sessionAResult).hasSize(3);
        assertThat(sessionAResult).extracting(ChatLog::getQuestion)
                .containsExactlyInAnyOrder("Q-A1", "Q-A2", "Q-A3");

        assertThat(sessionBResult).hasSize(2);
        assertThat(sessionBResult).extracting(ChatLog::getQuestion)
                .containsExactlyInAnyOrder("Q-B1", "Q-B2");

        assertThat(sessionCResult).isEmpty();
    }

    @Test
    @DisplayName("findRecentForContext: 같은 sessionId여도 다른 user는 격리 (user_id boundary)")
    void userIsolationWithSameSession() {
        persistChat(USER_A, RECIPE, SESSION_A, "Q-A", "A-A");
        persistChat(USER_B, RECIPE, SESSION_A, "Q-B", "A-B");
        em.flush();

        LocalDateTime since = LocalDateTime.now().minusHours(1);

        List<ChatLog> userAResult = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_A, since, PageRequest.of(0, 10));

        assertThat(userAResult).hasSize(1);
        assertThat(userAResult.get(0).getQuestion()).isEqualTo("Q-A");
    }

    @Test
    @DisplayName("findRecentForContext: 시간 cutoff 안전망 — since 이전 row는 제외")
    void timeCutoffSafetyNet() {
        persistChat(USER_A, RECIPE, SESSION_A, "Q-RECENT", "A-RECENT");
        em.flush();

        LocalDateTime futureSince = LocalDateTime.now().plusHours(1);

        List<ChatLog> result = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_A, futureSince, PageRequest.of(0, 10));

        assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("findRecentForContext: error_message IS NOT NULL row 제외")
    void errorMessageRowsExcluded() {
        ChatLog ok = ChatLog.builder()
                .userId(USER_A).recipeId(RECIPE).sessionId(SESSION_A)
                .question("Q-OK").answer("A-OK").intent("IN_SCOPE")
                .proCalled(true).totalLatencyMs(100).build();
        ChatLog err = ChatLog.builder()
                .userId(USER_A).recipeId(RECIPE).sessionId(SESSION_A)
                .question("Q-ERR").answer("").intent("UNKNOWN")
                .proCalled(false).totalLatencyMs(50)
                .errorMessage("705:쿼터 초과")
                .build();
        em.persist(ok);
        em.persist(err);
        em.flush();

        LocalDateTime since = LocalDateTime.now().minusHours(1);

        List<ChatLog> result = repository.findRecentForContext(
                USER_A, RECIPE, SESSION_A, since, PageRequest.of(0, 10));

        assertThat(result).hasSize(1);
        assertThat(result.get(0).getQuestion()).isEqualTo("Q-OK");
    }
}
