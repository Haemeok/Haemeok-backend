package com.jdc.recipe_service.controller.chat;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.chat.ChatHistoryItem;
import com.jdc.recipe_service.domain.dto.chat.ChatRequest;
import com.jdc.recipe_service.domain.dto.chat.ChatResponse;
import com.jdc.recipe_service.domain.dto.chat.QuotaResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.chat.ChatService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Tag(name = "레시피 챗봇 API", description = "보고 있는 레시피에 대한 질문 응답")
public class ChatController {

    private final ChatService chatService;

    @PostMapping("/api/recipes/{recipeId}/chat")
    @Operation(summary = "레시피 챗봇 질문",
            description = "Mini 분류기로 의도 판별 후 IN_SCOPE는 Pro 답변, 그 외는 정형 응답 반환")
    public ResponseEntity<ChatResponse> chat(
            @DecodeId Long recipeId,
            @Valid @RequestBody ChatRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        boolean isAdmin = userDetails.getAuthorities().stream()
                .anyMatch(a -> "ROLE_ADMIN".equals(a.getAuthority()));

        ChatResponse response = chatService.chat(
                userDetails.getId(), recipeId, request.getQuestion(), request.getSessionId(), isAdmin);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/api/recipes/{recipeId}/chat/history")
    @Operation(summary = "레시피별 챗봇 대화 기록 조회",
            description = "유저의 특정 레시피에 대한 정상 답변 history를 최신순으로 반환. UI 표시용.")
    public ResponseEntity<List<ChatHistoryItem>> getHistory(
            @DecodeId Long recipeId,
            @RequestParam(defaultValue = "20") int limit,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        return ResponseEntity.ok(chatService.getHistoryForDisplay(userDetails.getId(), recipeId, limit));
    }

    @GetMapping("/api/chat/quota")
    @Operation(summary = "챗봇 일일 쿼터 조회",
            description = "현재 일일 한도, 사용량, 남은 호출 수, 리셋 시각 반환.")
    public ResponseEntity<QuotaResponse> getQuota(@AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        return ResponseEntity.ok(chatService.getQuota(userDetails.getId()));
    }
}
