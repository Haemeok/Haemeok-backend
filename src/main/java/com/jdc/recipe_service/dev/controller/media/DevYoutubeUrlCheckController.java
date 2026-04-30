package com.jdc.recipe_service.dev.controller.media;

import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.service.media.YoutubeUrlCheckService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 유튜브 URL 존재 여부 체크.
 *
 * 운영 {@code GET /api/recipes/youtube/check} 미러. dev V3 차이점:
 *  - 운영은 덜 엄격한 {@code RecipeExtractionService.checkUrlExistence}(visibility/listing/lifecycle 검사 안 함)를 쓰지만
 *    dev V3는 더 엄격한 {@link YoutubeUrlCheckService}를 사용 — 4 enum 모두 검사 + official user + imageReady 필터.
 *  - 결과: RESTRICTED/PRIVATE/non-ACTIVE 또는 image PENDING 레시피의 존재가 URL probe로 누설되지 않음.
 *  - anonymous 허용 (단, strict service가 어차피 PUBLIC+LISTED+ACTIVE만 통과시킴).
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes/youtube")
@Tag(name = "Dev V3 유튜브 URL 체크 API",
        description = "운영보다 엄격한 가시성 필터로 RESTRICTED/PRIVATE/non-ACTIVE 누수 차단")
public class DevYoutubeUrlCheckController {

    private final YoutubeUrlCheckService youtubeUrlCheckService;

    @GetMapping("/check")
    @Operation(summary = "Dev V3 유튜브 URL 존재 여부 확인",
            description = """
                    URL이 이미 official YOUTUBE 레시피(PUBLIC+LISTED+ACTIVE+imageReady)로 등록됐는지 확인.
                    조건 미충족 시 null 반환 — 비공개/non-ACTIVE/image 미생성 레시피의 존재가 누설되지 않음.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — {recipeId: HashID} 또는 null",
                    content = @Content(schema = @io.swagger.v3.oas.annotations.media.Schema(implementation = RecipeIdResponse.class))),
            @ApiResponse(responseCode = "400", description = "잘못된 URL 형식 (INVALID_URL_FORMAT)", content = @Content)
    })
    public ResponseEntity<RecipeIdResponse> check(
            @Parameter(description = "유튜브 영상 URL", required = true) @RequestParam("url") String url) {
        Long recipeId = youtubeUrlCheckService.checkUrlExistence(url);
        if (recipeId != null) {
            return ResponseEntity.ok(new RecipeIdResponse(recipeId));
        }
        return ResponseEntity.ok(null);
    }

    public record RecipeIdResponse(
            @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
            Long recipeId
    ) {}
}
