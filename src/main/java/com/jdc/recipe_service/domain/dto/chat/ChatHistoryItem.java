package com.jdc.recipe_service.domain.dto.chat;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.entity.chat.ChatLog;
import com.jdc.recipe_service.domain.type.chat.Intent;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "챗봇 대화 기록 항목 (UI 표시용)")
public class ChatHistoryItem {

    @Schema(description = "chat_log row id (hashids 인코딩 문자열)", example = "x9Lb3a7Q", type = "string")
    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    private Long id;

    @Schema(description = "생성 시각", example = "2026-04-26T03:15:30")
    private LocalDateTime createdAt;

    @Schema(description = "유저 질문", example = "이거 매워요?")
    private String question;

    @Schema(description = "챗봇 답변", example = "보통 매운맛이에요...")
    private String answer;

    @Schema(description = "분류 의도", example = "IN_SCOPE")
    private Intent intent;

    @Schema(description = "Pro 호출 여부", example = "true")
    private boolean fromLlm;

    public static ChatHistoryItem from(ChatLog log) {
        return ChatHistoryItem.builder()
                .id(log.getId())
                .createdAt(log.getCreatedAt())
                .question(log.getQuestion())
                .answer(log.getAnswer())
                .intent(Intent.fromString(log.getIntent()))
                .fromLlm(log.isProCalled())
                .build();
    }
}
