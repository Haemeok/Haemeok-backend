package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.service.article.PublicCurationArticleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * 큐레이션 아티클 (public) 조회 API.
 *
 * <p>이 프로젝트 컨벤션상 일반 유저용 컨트롤러는 도메인명만 쓰고 (RecipeController, IngredientController …),
 * 특수 경계만 prefix를 붙인다 (Admin*, Dev*). 그래서 admin은 {@code AdminCurationArticleController}, public은
 * 이 클래스가 담당한다.
 *
 * <p>인증 없이 접근 가능하며 항상 PUBLISHED 상태만 노출한다. DRAFT/ARCHIVED는 404로 매핑된다.
 * 정렬은 publishedAt DESC, id DESC로 service 레이어에서 강제 — 클라이언트가 sort param을 변조해도
 * 운영 의도와 다른 결과가 나오지 않는다.
 */
@RestController
@RequestMapping("/api/curation-articles")
@RequiredArgsConstructor
@Tag(name = "큐레이션 아티클 API",
        description = "발행된 큐레이션 아티클 목록/상세 공개 조회 API입니다.")
public class CurationArticleController {

    private final PublicCurationArticleService publicArticleService;

    @GetMapping
    @Operation(summary = "발행된 아티클 목록 조회",
            description = """
                    status=PUBLISHED 만 반환. category optional. 정렬은 publishedAt DESC, id DESC로 고정된다 (sort param은 무시됨).
                    본문 contentMdx는 응답에서 제외.
                    페이지 크기 정책: 기본 20, 최대 50. 50을 초과해 요청해도 service에서 50으로 clamp된다.""")
    public ResponseEntity<Page<PublicCurationArticleSummaryResponse>> list(
            @Parameter(description = "카테고리 필터") @RequestParam(required = false) String category,
            @PageableDefault(size = 20) Pageable pageable) {
        return ResponseEntity.ok(publicArticleService.listPublished(category, pageable));
    }

    @GetMapping("/{slug}")
    @Operation(summary = "발행된 아티클 상세 조회 (slug 기준)",
            description = "PUBLISHED 상태 아티클만 반환. DRAFT/ARCHIVED 또는 존재하지 않는 slug는 404 ARTICLE_NOT_FOUND.")
    public ResponseEntity<PublicCurationArticleResponse> getBySlug(
            @Parameter(description = "URL slug") @PathVariable String slug) {
        return ResponseEntity.ok(publicArticleService.getBySlug(slug));
    }
}
