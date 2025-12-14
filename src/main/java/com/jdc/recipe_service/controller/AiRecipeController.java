package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.facade.AiRecipeFacade;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/recipes/ai")
@RequiredArgsConstructor
@Tag(name = "AI 레시피 생성 API", description = "AI를 이용한 레시피 생성 전용 API입니다.")
public class AiRecipeController {

    private final AiRecipeFacade aiRecipeFacade;

    @PostMapping
    @Operation(summary = "AI 레시피 생성", description = "AI 로봇이 레시피를 생성하고 저장합니다. (처리 시간 소요됨)")
    public ResponseEntity<PresignedUrlResponse> createAiRecipe(
            @Parameter(description = "요리 컨셉 선택 (INGREDIENT_FOCUS, COST_EFFECTIVE, NUTRITION_BALANCE, FINE_DINING)")
            @RequestParam(value = "concept") AiRecipeConcept concept,

            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "AI 생성 요청 정보")
            @RequestBody @Valid RecipeWithImageUploadRequest request,

            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        PresignedUrlResponse response = aiRecipeFacade.generateAndSave(
                request,
                concept,
                userDetails.getUser().getId()
        );

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}