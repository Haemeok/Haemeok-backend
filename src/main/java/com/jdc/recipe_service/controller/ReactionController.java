package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.user.ReactionRequestDto;
import com.jdc.recipe_service.service.ReactionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/test/recipes")
public class ReactionController {

    private final ReactionService reactionService;

    @PostMapping("/{recipeId}/reactions")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<String> addReactions(
            @DecodeId Long recipeId,
            @RequestBody ReactionRequestDto requestDto
    ) {
        reactionService.addReactions(recipeId, requestDto);
        return ResponseEntity.ok("테스트 반응(좋아요/평점) 생성 완료");
    }
}