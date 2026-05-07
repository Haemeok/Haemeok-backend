package com.jdc.recipe_service.controller.admin;

import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateRequest;
import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleUpdateRequest;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.article.CurationArticleCreateResult;
import com.jdc.recipe_service.service.article.CurationArticleImageUploadService;
import com.jdc.recipe_service.service.article.CurationArticleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
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
 *
 * <p><b>articleId path 정책</b>: 모든 path variable은 HashID 문자열로만 받는다. 전역 {@code @DecodeId}는
 * 마이그레이션 호환을 위해 raw 숫자도 받아주지만, 큐레이션 아티클은 신규 도메인이라 처음부터 strict하게
 * HashID만 허용한다. 숫자 path(예: {@code /42/publish})는 {@link #decodeArticleId} 에서 400으로 거부.
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
    private final Hashids hashids;

    @PostMapping
    @Operation(summary = "아티클 생성",
            description = """
                    DRAFT 상태로 새 아티클을 생성한다. recipeIds는 audit/soft link로 저장되며 요청은 HashID 문자열 배열로 받는다.
                    응답의 articleId는 HashID 문자열, slug는 백엔드가 확정한 최종 slug. base slug가 이미 존재하면
                    자동으로 -2/-3/... suffix가 붙으므로 프론트는 응답 slug를 그대로 사용해야 한다.""")
    public ResponseEntity<CurationArticleCreateResponse> create(
            @RequestBody @Valid CurationArticleCreateRequest request) {
        CurationArticleCreateResult result = articleService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(
                CurationArticleCreateResponse.builder()
                        .articleId(result.id())
                        .slug(result.slug())
                        .build());
    }

    @GetMapping
    @Operation(summary = "아티클 목록 조회",
            description = "status/category/q(title LIKE) 모두 optional. Page 기반으로 총 개수가 응답에 포함된다. 응답의 id는 모두 HashID 문자열.")
    public ResponseEntity<Page<CurationArticleSummaryResponse>> list(
            @Parameter(description = "발행 상태") @RequestParam(required = false) ArticleStatus status,
            @Parameter(description = "카테고리") @RequestParam(required = false) String category,
            @Parameter(description = "제목 LIKE 검색어") @RequestParam(required = false) String q,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {
        return ResponseEntity.ok(articleService.search(status, category, q, pageable));
    }

    @GetMapping("/{articleId}")
    @Operation(summary = "아티클 상세 조회", description = "articleId는 HashID 문자열로 받는다. 숫자 입력은 400 거부. 응답의 id, recipeIds 모두 HashID 문자열.")
    public ResponseEntity<CurationArticleResponse> get(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId) {
        return ResponseEntity.ok(articleService.get(decodeArticleId(articleId)));
    }

    @PutMapping("/{articleId}")
    @Operation(summary = "아티클 수정",
            description = "본문/메타데이터 + recipeIds 전체 교체. slug는 변경 불가. 본문이 바뀌면 humanReviewed가 false로 초기화된다. articleId path와 body recipeIds 모두 HashID 문자열.")
    public ResponseEntity<Map<String, String>> update(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId,
            @RequestBody @Valid CurationArticleUpdateRequest request) {
        Long updatedId = articleService.update(decodeArticleId(articleId), request);
        return ResponseEntity.ok(Map.of("articleId", hashids.encode(updatedId)));
    }

    @PostMapping("/{articleId}/publish")
    @Operation(summary = "아티클 발행",
            description = "status=PUBLISHED. publishedAt이 null이면 현재 시각으로 채우고 이후 호출에서는 보존된다 (idempotent).")
    public ResponseEntity<Void> publish(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId) {
        articleService.publish(decodeArticleId(articleId));
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/archive")
    @Operation(summary = "아티클 아카이브", description = "status=ARCHIVED (idempotent).")
    public ResponseEntity<Void> archive(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId) {
        articleService.archive(decodeArticleId(articleId));
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/review")
    @Operation(summary = "사람 검수 완료 표시", description = "humanReviewed=true (idempotent).")
    public ResponseEntity<Void> review(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId) {
        articleService.markReviewed(decodeArticleId(articleId));
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{articleId}/images/presigned-urls")
    @Operation(summary = "아티클 이미지 presigned PUT URL 발급 (articleHashId 기반)",
            description = """
                    articleId path(HashID)가 가리키는 아티클 소속 이미지 업로드 URL을 발급한다.
                    아티클이 없으면 ARTICLE_NOT_FOUND. DRAFT/PUBLISHED/ARCHIVED 어떤 상태에서도 어드민은 발급 가능하다.

                    image/jpeg, image/png, image/webp만 허용. fileSize 10MB 이하.
                    uploadKey와 imageKey 모두 서버에서 생성한다 — 프론트가 path를 결정하지 못한다.

                    응답은 uploadKey(원본 업로드 위치) + imageKey(변환 후 .webp 위치) + presignedUrl 3개.
                    프론트는 presignedUrl로 uploadKey에 PUT하고, DB(coverImageKey) 또는 MDX의 <ArticleImage imageKey="..."/>에는
                    반드시 imageKey만 저장한다 — uploadKey는 변환 전 원본이라 화면 노출 대상이 아니다.

                    주의(V1): fileSize 10MB 제한은 요청값에 대한 사전 검증일 뿐 presigned PUT 자체가 실제 업로드 크기를
                    강제하지 않는다. 어드민 전용이라 V1에서 허용하며, 강제가 필요해지면 presigned POST + content-length-range
                    또는 업로드 후 HEAD/finalize 검증을 별도로 도입한다.

                    Key 패턴 (articleHashId segment 기반):
                      uploadKey = original/images/articles/{articleHashId}/{uuid}.{ext}
                      imageKey  = images/articles/{articleHashId}/{uuid}.webp""")
    public ResponseEntity<ArticleImagePresignedUrlResponse> issueImagePresignedUrl(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId,
            @RequestBody @Valid ArticleImagePresignedUrlRequest request) {
        return ResponseEntity.ok(imageUploadService.issuePresignedUrl(decodeArticleId(articleId), request));
    }

    @PostMapping("/{articleId}/images/finalize")
    @Operation(summary = "아티클 이미지 업로드 완료 확인 (S3 HEAD)",
            description = """
                    프론트가 S3 PUT을 끝낸 후, 변환된 .webp가 S3에 실제로 존재하는지 확인한다 (Lambda 변환 완료 검증).
                    DB 상태는 변경하지 않고 단순히 존재 여부만 본다.

                    검증 순서: ① articleId 존재 ② 각 imageKey가 images/articles/{articleHashId}/ 로 시작 + .webp 종료 ③ S3 HEAD.
                    모두 존재하면 200 + ready=true.
                    하나라도 없으면 409 + ARTICLE_IMAGES_NOT_READY (응답에 missingKeys 포함) — 프론트는 짧게 폴링하거나 사용자에게 다시 시도 안내.""")
    public ResponseEntity<ArticleImageFinalizeResponse> finalizeImages(
            @Parameter(description = "아티클 ID (HashID)") @PathVariable String articleId,
            @RequestBody @Valid ArticleImageFinalizeRequest request) {
        return ResponseEntity.ok(imageUploadService.finalizeImages(decodeArticleId(articleId), request.getImageKeys()));
    }

    /**
     * 큐레이션 아티클 전용 strict HashID 디코더.
     *
     * <p>전역 {@code @DecodeId} resolver는 마이그레이션 호환을 위해 숫자 문자열도 그대로 Long으로 받아주지만,
     * 큐레이션 아티클은 신규 도메인이라 처음부터 HashID만 받는다는 정책이 명확하다. 여기서는:
     * <ul>
     *   <li>입력이 순수 숫자({@code "42"})면 INVALID_INPUT_VALUE로 400 거부</li>
     *   <li>HashID 디코드 실패면 마찬가지로 400 거부</li>
     *   <li>유효한 HashID만 raw Long으로 반환</li>
     * </ul>
     *
     * <p>전역 resolver를 바꾸지 않는 이유: 다른 도메인(레시피/유저 등)은 lenient 동작에 의존할 수 있다.
     */
    private Long decodeArticleId(String hashId) {
        if (hashId == null || hashId.isBlank()) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "articleId가 비어 있습니다.");
        }
        if (hashId.matches("^[0-9]+$")) {
            // 숫자만 박은 path는 raw Long을 그대로 보내려는 시도 — 정책상 거부.
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "articleId는 HashID 문자열로 입력해야 합니다 (숫자 입력 거부): " + hashId);
        }
        long[] decoded = hashids.decode(hashId);
        if (decoded.length == 0) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "유효하지 않은 articleId(HashID): " + hashId);
        }
        return decoded[0];
    }
}
