package com.jdc.recipe_service.controller.admin;

import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateRequest;
import com.jdc.recipe_service.domain.dto.article.CurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleUpdateRequest;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.service.article.CurationArticleImageUploadService;
import com.jdc.recipe_service.service.article.CurationArticleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * 어드민 전용 큐레이션 아티클 API.
 *
 * <p>SecurityConfig의 `/api/admin/**` ROLE_ADMIN 매처에 추가로 클래스 레벨 @PreAuthorize로 이중 방어한다.
 * 일반 유저용 public API는 별도의 {@link com.jdc.recipe_service.controller.CurationArticleController}에서
 * PUBLISHED 상태 아티클만 노출하도록 제공한다.
 */
@RestController
@RequestMapping("/api/admin/curation-articles")
@PreAuthorize("hasRole('ADMIN')")
@RequiredArgsConstructor
@Tag(name = "관리자 전용 큐레이션 아티클 API",
        description = "어드민 전용 큐레이션 아티클 CRUD/발행/이미지 업로드 API입니다.")
public class AdminCurationArticleController {

    private final CurationArticleService articleService;
    private final CurationArticleImageUploadService imageUploadService;

    @PostMapping
    @Operation(summary = "아티클 생성", description = "DRAFT 상태로 새 아티클을 생성한다. recipeIds는 audit/soft link로 저장된다.")
    public ResponseEntity<Map<String, Long>> create(
            @RequestBody @Valid CurationArticleCreateRequest request) {
        Long articleId = articleService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(Map.of("articleId", articleId));
    }

    @GetMapping
    @Operation(summary = "아티클 목록 조회",
            description = "status/category/q(title LIKE) 모두 optional. Page 기반으로 총 개수가 응답에 포함된다.")
    public ResponseEntity<Page<CurationArticleSummaryResponse>> list(
            @Parameter(description = "발행 상태") @RequestParam(required = false) ArticleStatus status,
            @Parameter(description = "카테고리") @RequestParam(required = false) String category,
            @Parameter(description = "제목 LIKE 검색어") @RequestParam(required = false) String q,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {
        return ResponseEntity.ok(articleService.search(status, category, q, pageable));
    }

    @GetMapping("/{articleId}")
    @Operation(summary = "아티클 상세 조회")
    public ResponseEntity<CurationArticleResponse> get(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId) {
        return ResponseEntity.ok(articleService.get(articleId));
    }

    @PutMapping("/{articleId}")
    @Operation(summary = "아티클 수정",
            description = "본문/메타데이터 + recipeIds 전체 교체. slug는 변경 불가. 본문이 바뀌면 humanReviewed가 false로 초기화된다.")
    public ResponseEntity<Map<String, Long>> update(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId,
            @RequestBody @Valid CurationArticleUpdateRequest request) {
        Long updatedId = articleService.update(articleId, request);
        return ResponseEntity.ok(Map.of("articleId", updatedId));
    }

    @PostMapping("/{articleId}/publish")
    @Operation(summary = "아티클 발행",
            description = "status=PUBLISHED. publishedAt이 null이면 현재 시각으로 채우고 이후 호출에서는 보존된다 (idempotent).")
    public ResponseEntity<Void> publish(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId) {
        articleService.publish(articleId);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/archive")
    @Operation(summary = "아티클 아카이브", description = "status=ARCHIVED (idempotent).")
    public ResponseEntity<Void> archive(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId) {
        articleService.archive(articleId);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/review")
    @Operation(summary = "사람 검수 완료 표시", description = "humanReviewed=true (idempotent).")
    public ResponseEntity<Void> review(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId) {
        articleService.markReviewed(articleId);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/images/presigned-urls")
    @Operation(summary = "아티클 이미지 presigned PUT URL 발급 (articleId 기반)",
            description = """
                    articleId path variable이 가리키는 아티클 소속 이미지 업로드 URL을 발급한다.
                    아티클이 없으면 ARTICLE_NOT_FOUND. DRAFT/PUBLISHED/ARCHIVED 어떤 상태에서도 어드민은 발급 가능하다.

                    image/jpeg, image/png, image/webp만 허용. fileSize 10MB 이하.
                    uploadKey와 imageKey 모두 서버에서 생성한다 — 프론트가 path를 결정하지 못한다.

                    응답은 uploadKey(원본 업로드 위치) + imageKey(변환 후 .webp 위치) + presignedUrl 3개.
                    프론트는 presignedUrl로 uploadKey에 PUT하고, DB(coverImageKey) 또는 MDX의 <ArticleImage imageKey="..."/>에는
                    반드시 imageKey만 저장한다 — uploadKey는 변환 전 원본이라 화면 노출 대상이 아니다.

                    주의(V1): fileSize 10MB 제한은 요청값에 대한 사전 검증일 뿐 presigned PUT 자체가 실제 업로드 크기를
                    강제하지 않는다. 어드민 전용이라 V1에서 허용하며, 강제가 필요해지면 presigned POST + content-length-range
                    또는 업로드 후 HEAD/finalize 검증을 별도로 도입한다.

                    Key 패턴:
                      uploadKey = original/images/articles/{articleId}/{uuid}.{ext}
                      imageKey  = images/articles/{articleId}/{uuid}.webp""")
    public ResponseEntity<ArticleImagePresignedUrlResponse> issueImagePresignedUrl(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId,
            @RequestBody @Valid ArticleImagePresignedUrlRequest request) {
        return ResponseEntity.ok(imageUploadService.issuePresignedUrl(articleId, request));
    }

    @PostMapping("/{articleId}/images/finalize")
    @Operation(summary = "아티클 이미지 업로드 완료 확인 (S3 HEAD)",
            description = """
                    프론트가 S3 PUT을 끝낸 후, 변환된 .webp가 S3에 실제로 존재하는지 확인한다 (Lambda 변환 완료 검증).
                    DB 상태는 변경하지 않고 단순히 존재 여부만 본다.

                    검증 순서: ① articleId 존재 ② 각 imageKey가 images/articles/{articleId}/ 로 시작 + .webp 종료 ③ S3 HEAD.
                    모두 존재하면 200 + ready=true.
                    하나라도 없으면 409 + ARTICLE_IMAGES_NOT_READY (응답에 missingKeys 포함) — 프론트는 짧게 폴링하거나 사용자에게 다시 시도 안내.""")
    public ResponseEntity<ArticleImageFinalizeResponse> finalizeImages(
            @Parameter(description = "아티클 ID") @PathVariable Long articleId,
            @RequestBody @Valid ArticleImageFinalizeRequest request) {
        return ResponseEntity.ok(imageUploadService.finalizeImages(articleId, request.getImageKeys()));
    }
}
