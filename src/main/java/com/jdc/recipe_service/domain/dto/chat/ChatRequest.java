package com.jdc.recipe_service.domain.dto.chat;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피 챗봇 질문 요청")
public class ChatRequest {

    // 운영 한도는 chat_config.max_question_length에서 동적 체크. 2000은 비정상 페이로드 1차 방어.
    @NotBlank(message = "질문 내용은 필수입니다.")
    @Size(max = 2000, message = "질문이 너무 깁니다.")
    @Schema(description = "유저 질문", example = "이거 매워요?")
    private String question;
}
