package com.jdc.recipe_service.domain.dto.chat;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
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

    // 외부 입력은 hashids 인코딩 문자열 ("x9Lb3a7Q"). 운영 backward-compat 위해 raw numeric도 허용 — HashIdDeserializer 참조.
    @NotNull(message = "recipeId는 필수입니다.")
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    @Schema(description = "질문 대상 레시피 ID (hashids 인코딩 문자열)", example = "x9Lb3a7Q", type = "string")
    private Long recipeId;

    // 비즈니스 한도는 chat_config.max_question_length(DB)에서 ChatService가 동적 체크.
    // 여기 2000은 비정상적 페이로드 1차 방어용.
    @NotBlank(message = "질문 내용은 필수입니다.")
    @Size(max = 2000, message = "질문이 너무 깁니다.")
    @Schema(description = "유저 질문", example = "이거 매워요?")
    private String question;
}
