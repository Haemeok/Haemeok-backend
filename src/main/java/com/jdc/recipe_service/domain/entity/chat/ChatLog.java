package com.jdc.recipe_service.domain.entity.chat;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.Generated;
import org.hibernate.generator.EventType;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(
        name = "chat_log",
        indexes = {
                @Index(name = "idx_chat_log_user_created", columnList = "user_id, created_at DESC"),
                @Index(name = "idx_chat_log_recipe_id",   columnList = "recipe_id"),
                @Index(name = "idx_chat_log_intent",      columnList = "intent"),
                @Index(name = "idx_chat_log_created",     columnList = "created_at DESC"),
                @Index(name = "idx_chat_log_context",     columnList = "user_id, recipe_id, session_id, created_at DESC"),
                @Index(name = "idx_chat_log_display",     columnList = "user_id, recipe_id, created_at DESC")
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class ChatLog {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false)
    private Long userId;

    @Column(name = "recipe_id", nullable = false)
    private Long recipeId;

    @Column(name = "session_id", length = 50)
    private String sessionId;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String question;

    @Column(nullable = false, length = 20)
    private String intent;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String answer;

    @Column(name = "pro_called", nullable = false)
    private boolean proCalled;

    @Column(name = "mini_latency_ms")
    private Integer miniLatencyMs;

    @Column(name = "pro_latency_ms")
    private Integer proLatencyMs;

    @Column(name = "total_latency_ms", nullable = false)
    private Integer totalLatencyMs;

    @Column(name = "mini_input_tokens")
    private Integer miniInputTokens;

    @Column(name = "mini_output_tokens")
    private Integer miniOutputTokens;

    @Column(name = "pro_input_tokens")
    private Integer proInputTokens;

    @Column(name = "pro_cached_tokens")
    private Integer proCachedTokens;

    @Column(name = "pro_output_tokens")
    private Integer proOutputTokens;

    @Column(name = "estimated_cost_krw", precision = 10, scale = 4)
    private BigDecimal estimatedCostKrw;

    @Column(name = "repetition_detected", nullable = false)
    @Builder.Default
    private boolean repetitionDetected = false;

    @Column(name = "answer_truncated", nullable = false)
    @Builder.Default
    private boolean answerTruncated = false;

    @Column(name = "suspicious", nullable = false)
    @Builder.Default
    private boolean suspicious = false;

    @Column(name = "suspicious_reason", length = 100)
    private String suspiciousReason;

    @Column(name = "classifier_version", nullable = false, length = 20)
    @Builder.Default
    private String classifierVersion = "v2";

    @Column(name = "chat_version", nullable = false, length = 20)
    @Builder.Default
    private String chatVersion = "v6";

    @Column(name = "error_message", columnDefinition = "TEXT")
    private String errorMessage;

    @Column(name = "user_feedback", length = 20)
    private String userFeedback;

    @Column(name = "feedback_comment", columnDefinition = "TEXT")
    private String feedbackComment;

    @Generated(event = EventType.INSERT)
    @Column(name = "created_at", insertable = false, updatable = false,
            columnDefinition = "DATETIME(6) DEFAULT CURRENT_TIMESTAMP(6)")
    private LocalDateTime createdAt;

    public void recordFeedback(String feedback, String comment) {
        this.userFeedback = feedback;
        this.feedbackComment = comment;
    }
}
