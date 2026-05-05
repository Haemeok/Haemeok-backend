package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.article.CurationArticleRecommendationResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSitemapResponse;
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

import java.util.List;

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

    /**
     * sitemap 엔드포인트는 {@code @GetMapping("/{slug}")}보다 위에 둬야 의도가 명확하다.
     * Spring MVC는 literal path가 path variable보다 더 specific하게 매칭되므로 위치만으로
     * 정렬되진 않지만, 사람이 읽을 때 "/sitemap"이 먼저 잡힌다는 의도가 드러나야 후임이
     * 이 매핑을 실수로 {@code /{slug}} 아래로 옮기는 일을 막을 수 있다.
     */
    @GetMapping("/sitemap")
    @Operation(summary = "발행된 아티클 sitemap 데이터 조회",
            description = """
                    sitemap.xml 생성용 — PUBLISHED 아티클의 slug + updatedAt 배열만 반환한다.
                    DRAFT/ARCHIVED는 절대 포함되지 않는다. 정렬은 updatedAt DESC, id DESC.
                    인증/path/query/body 모두 없음.""")
    public ResponseEntity<List<CurationArticleSitemapResponse>> sitemap() {
        return ResponseEntity.ok(publicArticleService.listSitemap());
    }

    @GetMapping("/{slug}")
    @Operation(summary = "발행된 아티클 상세 조회 (slug 기준)",
            description = "PUBLISHED 상태 아티클만 반환. DRAFT/ARCHIVED 또는 존재하지 않는 slug는 404 ARTICLE_NOT_FOUND.")
    public ResponseEntity<PublicCurationArticleResponse> getBySlug(
            @Parameter(description = "URL slug") @PathVariable String slug) {
        return ResponseEntity.ok(publicArticleService.getBySlug(slug));
    }

    @GetMapping("/{slug}/recommendations")
    @Operation(summary = "아티클 상세 페이지 추천 카드",
            description = """
                    상세 페이지 하단의 추천 아티클 목록. PUBLISHED만 반환하며 현재 아티클은 결과에서 제외된다.
                    deterministic random 기반이라 같은 아티클에서 같은 추천 순서가 안정적으로 유지된다 (새로고침해도 동일).
                    같은 카테고리 추천(min(3, size/2))과 explore 추천을 섞어 단일 카테고리가 추천을 독점하지 않게 한다.
                    size는 [4, 12]로 clamp되며 기본 6.""")
    public ResponseEntity<List<CurationArticleRecommendationResponse>> recommendations(
            @Parameter(description = "현재 보고 있는 아티클의 URL slug") @PathVariable String slug,
            @Parameter(description = "추천 개수. 기본 6, 최소 4, 최대 12 (범위 밖은 service에서 clamp)")
            @RequestParam(required = false) Integer size) {
        return ResponseEntity.ok(publicArticleService.listRecommendations(slug, size));
    }
}
