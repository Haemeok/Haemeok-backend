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

    @NotBlank(message = "질문 내용은 필수입니다.")
    @Size(max = 2000, message = "질문이 너무 깁니다.")
    @Schema(description = "유저 질문", example = "이거 매워요?")
    private String question;

    // null이면 stateless (history 합치지 않음). frontend 챗봇 컴포넌트 mount 시 uuid 발급 권장.
    @Size(max = 50)
    @Schema(description = "페이지/컴포넌트 단위 대화 세션 식별자 (선택). 같은 sessionId만 history 합침. 미전송 시 1회성 stateless 호출",
            example = "550e8400-e29b-41d4-a716-446655440000",
            nullable = true)
    private String sessionId;
}
