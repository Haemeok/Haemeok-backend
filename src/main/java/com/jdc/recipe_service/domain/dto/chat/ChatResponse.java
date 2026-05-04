package com.jdc.recipe_service.domain.dto.chat;

import com.jdc.recipe_service.domain.type.chat.Intent;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피 챗봇 응답")
public class ChatResponse {

    @Schema(description = "챗봇 답변 본문", example = "이 레시피는 그렇게 맵지 않아요. 고춧가루 1티스푼만 들어가서...")
    private String answer;

    @Schema(description = "Mini 분류기 결과 의도", example = "IN_SCOPE")
    private Intent intent;

    @Schema(description = "최종 표시 답변이 Pro 원문 기반이면 true, 정형 응답 또는 후처리 fallback이면 false.", example = "true")
    private boolean fromLlm;
}
