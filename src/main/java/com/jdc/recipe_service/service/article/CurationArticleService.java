package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateRequest;
import com.jdc.recipe_service.domain.dto.article.CurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleUpdateRequest;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.entity.article.CurationArticleRecipeRef;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRecipeRefRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 큐레이션 아티클 어드민 유스케이스.
 *
 * <p>V1에서는 status 전이 제약을 두지 않는다 (publish/archive 모두 idempotent). 향후 더 엄격한 정책이 필요해지면
 * ARTICLE_INVALID_STATUS_TRANSITION을 사용해 게이트한다.
 *
 * <p><b>create slug 정책 (V1.1)</b>: base slug가 이미 존재하면 자동으로 {@code -2}, {@code -3} ... suffix를 붙여
 * unique한 slug로 저장한다. 첫 번째 글은 base slug 그대로. select-then-insert 패턴이라 race 가능성이 있어
 * insert 단계에서 unique 제약이 깨지면 새 트랜잭션으로 짧게 재시도한다 ({@link #MAX_CREATE_ATTEMPTS}회).
 * race 자체는 매우 드물다 (admin 1명 운영, AI batch 직렬 호출).
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class CurationArticleService {

    private final CurationArticleRepository articleRepo;
    private final CurationArticleRecipeRefRepository refRepo;
    private final RecipeRepository recipeRepo;
    private final PlatformTransactionManager transactionManager;

    /**
     * race 발생 시 재시도 횟수. 동시 admin 5명이 같은 base slug로 동시에 누르는 경우까지만 안전하면 충분.
     * 실제 운영에선 1~2회로도 차고 넘침 (대부분 첫 attempt에서 select 단계가 충돌을 발견).
     */
    private static final int MAX_CREATE_ATTEMPTS = 5;

    /** entity의 slug 컬럼 max length. 자동 suffix 붙여도 이걸 넘지 않도록 base를 자른다. */
    private static final int SLUG_COLUMN_MAX_LENGTH = 255;

    /** 자동 suffix 영역 — {@code "-999999"}까지 7자. base를 자를 때 이만큼 여유를 둔다. */
    private static final int SLUG_SUFFIX_RESERVE = 7;

    /** suffix 시도 상한. base가 이미 100만 개 가까이 있는 게 아니라면 절대 도달 안 함. */
    private static final int MAX_SUFFIX_NUMBER = 999_999;

    /**
     * public {@code /api/curation-articles/{slug}}와 같은 prefix 아래 literal route가 잡고 있는 segment 목록.
     * 이 값들이 slug로 들어오면 {@code GET /{slug}} 매핑이 영영 도달 불가능해지므로 create 시점에 차단한다.
     *
     * <p>새 literal route(예: 추후 {@code /api/curation-articles/feed} 같은 것)가
     * {@link com.jdc.recipe_service.controller.CurationArticleController}에 추가되면 그 segment를 여기에도 추가해야 한다.
     */
    private static final Set<String> RESERVED_SLUGS = Set.of("sitemap");

    private TransactionTemplate writeTxTemplate;

    @PostConstruct
    void initTxTemplate() {
        // 매 attempt마다 격리된 새 트랜잭션이 필요하다 — DataIntegrityViolationException 발생 시 트랜잭션은
        // rollback-only로 마크되어 같은 트랜잭션 내 재시도가 안 된다.
        this.writeTxTemplate = new TransactionTemplate(transactionManager);
        this.writeTxTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }

    // ── Read ──

    public CurationArticleResponse get(Long articleId) {
        CurationArticle article = loadArticle(articleId);
        List<Long> refs = refRepo.findRecipeIdsByArticleId(articleId);
        return CurationArticleResponse.of(article, refs);
    }

    public Page<CurationArticleSummaryResponse> search(ArticleStatus status,
                                                       String category,
                                                       String q,
                                                       Pageable pageable) {
        return articleRepo.search(status, nullIfBlank(category), nullIfBlank(q), pageable)
                .map(CurationArticleSummaryResponse::of);
    }

    // ── Write ──

    /**
     * Create 진입점. retry loop은 격리된 트랜잭션 단위로 돈다 — 중간 attempt가 실패해도 부분 저장이 남지 않는다.
     * RESERVED slug, recipe ref 검증은 attempt 안에서 함께 수행해 retry마다 같은 검증이 일관되게 적용된다.
     *
     * <p>retry 대상은 {@link SlugUniqueViolationException} 한 가지뿐이다 — slug 외 다른 DB 제약 위반은
     * retry해도 같은 결과가 나오므로 그대로 propagate해야 원인이 가려지지 않는다.
     */
    public CurationArticleCreateResult create(CurationArticleCreateRequest req) {
        for (int attempt = 1; attempt <= MAX_CREATE_ATTEMPTS; attempt++) {
            try {
                return writeTxTemplate.execute(status -> doCreate(req));
            } catch (SlugUniqueViolationException e) {
                // race로 slug unique 충돌 — 다음 attempt에서 새 select가 점유된 slug를 보고 다음 candidate를 고른다.
                log.warn("curation article create slug race detected: baseSlug={}, attempt={}/{}",
                        req.getSlug(), attempt, MAX_CREATE_ATTEMPTS);
            }
        }
        // 5번 모두 실패 — 동시 충돌이 비정상적으로 잦거나 select 단계 결과가 stale. 운영자에게 명시적 에러로 알린다.
        throw new CustomException(ErrorCode.ARTICLE_SLUG_DUPLICATE);
    }

    private CurationArticleCreateResult doCreate(CurationArticleCreateRequest req) {
        if (RESERVED_SLUGS.contains(req.getSlug())) {
            throw new CustomException(ErrorCode.ARTICLE_SLUG_RESERVED);
        }
        validateRecipeIds(req.getRecipeIds());

        String finalSlug = pickAvailableSlug(req.getSlug());

        CurationArticle article = CurationArticle.builder()
                .slug(finalSlug)
                .title(req.getTitle())
                .description(req.getDescription())
                .coverImageKey(req.getCoverImageKey())
                .contentMdx(req.getContentMdx())
                .category(req.getCategory())
                .generatedBy(req.getGeneratedBy())
                .build();

        CurationArticle saved;
        try {
            // saveAndFlush로 즉시 unique constraint를 강제 — 트랜잭션 commit이 아니라 여기서 IntegrityViolation을 받아
            // 재시도 루프가 잡을 수 있게 한다.
            saved = articleRepo.saveAndFlush(article);
        } catch (DataIntegrityViolationException e) {
            if (isSlugConstraintViolation(e)) {
                // outer retry가 잡을 marker exception. 같은 base를 가지고 새 candidate를 시도한다.
                throw new SlugUniqueViolationException(e);
            }
            // slug 외 다른 제약 위반 (예: NOT NULL, FK)은 retry 의미가 없다. 원본을 그대로 propagate해서
            // GlobalExceptionHandler의 906 DATA_INTEGRITY_VIOLATION으로 매핑되도록 한다.
            throw e;
        }

        saveRefs(saved, req.getRecipeIds());
        return new CurationArticleCreateResult(saved.getId(), saved.getSlug());
    }

    /**
     * 예외 메시지에 컬럼명 {@code slug}가 포함되는지로 식별한다. MySQL은
     * {@code "Duplicate entry 'foo' for key 'curation_articles.slug'"}, H2는
     * {@code "Unique index ... ON ... (\"SLUG\")"} 형태라 둘 다 케이스 무시 후 'slug' substring으로 매칭된다.
     *
     * <p>prod MySQL에는 Flyway V20260504_001에 의해 {@code uk_curation_articles_slug} 이름이 명시돼 있어
     * {@link org.hibernate.exception.ConstraintViolationException#getConstraintName()} 기반 식별이 가능하다.
     * 다만 Hibernate auto-DDL을 쓰는 테스트/로컬 환경에서는 DB dialect에 따라 constraint 이름이나 메시지 형식이
     * 달라질 수 있어, V1에서는 메시지 substring 매칭으로 통일했다 — 후속 enhancement 후보.
     */
    private static boolean isSlugConstraintViolation(DataIntegrityViolationException e) {
        Throwable cause = e.getMostSpecificCause();
        String msg = cause != null ? cause.getMessage() : null;
        return msg != null && msg.toLowerCase().contains("slug");
    }

    /** outer retry loop이 식별할 marker. 메시지나 errorCode는 사용하지 않으므로 단순 wrap. */
    private static final class SlugUniqueViolationException extends RuntimeException {
        SlugUniqueViolationException(Throwable cause) { super(cause); }
    }

    /**
     * base slug가 비어 있으면 그대로, 이미 점유돼 있으면 가장 작은 미사용 {@code base-N}을 반환한다.
     * 컬럼 길이를 넘지 않도록 base를 미리 잘라둔다 (잘린 경우 마지막 hyphen 제거 — 의미 없는 hyphen 방지).
     */
    private String pickAvailableSlug(String baseSlug) {
        String base = trimBaseForSuffix(baseSlug);
        Set<String> used = new HashSet<>(articleRepo.findSlugsStartingWith(base));

        if (!used.contains(base)) {
            return base;
        }
        for (int suffix = 2; suffix <= MAX_SUFFIX_NUMBER; suffix++) {
            String candidate = base + "-" + suffix;
            if (!used.contains(candidate)) {
                return candidate;
            }
        }
        // 거의 도달 불가 — base 하나가 100만 개 가까이 있는 비정상 상태.
        throw new CustomException(ErrorCode.ARTICLE_SLUG_DUPLICATE);
    }

    private static String trimBaseForSuffix(String base) {
        int maxBase = SLUG_COLUMN_MAX_LENGTH - SLUG_SUFFIX_RESERVE;
        if (base.length() <= maxBase) return base;
        String trimmed = base.substring(0, maxBase);
        // 잘린 끝이 hyphen이면 제거 — "foo-bar-" 같은 댕글링 hyphen 방지
        while (trimmed.endsWith("-")) {
            trimmed = trimmed.substring(0, trimmed.length() - 1);
        }
        return trimmed;
    }

    @Transactional
    public Long update(Long articleId, CurationArticleUpdateRequest req) {
        CurationArticle article = loadArticle(articleId);
        validateRecipeIds(req.getRecipeIds());

        article.updateContent(
                req.getTitle(),
                req.getDescription(),
                req.getContentMdx(),
                req.getCoverImageKey(),
                req.getCategory(),
                req.getGeneratedBy()
        );

        refRepo.deleteByArticleId(articleId);
        saveRefs(article, req.getRecipeIds());

        return article.getId();
    }

    @Transactional
    public void publish(Long articleId) {
        loadArticle(articleId).publish();
    }

    @Transactional
    public void archive(Long articleId) {
        loadArticle(articleId).archive();
    }

    @Transactional
    public void markReviewed(Long articleId) {
        loadArticle(articleId).markReviewed();
    }


    private CurationArticle loadArticle(Long articleId) {
        return articleRepo.findById(articleId)
                .orElseThrow(() -> new CustomException(ErrorCode.ARTICLE_NOT_FOUND));
    }

    private void validateRecipeIds(List<Long> recipeIds) {
        if (recipeIds == null || recipeIds.isEmpty()) return;

        if (recipeIds.stream().anyMatch(id -> id == null || id <= 0)) {
            throw new CustomException(ErrorCode.ARTICLE_INVALID_RECIPE_REF);
        }

        List<Long> distinct = recipeIds.stream().distinct().toList();
        long foundCount = recipeRepo.findAllById(distinct).size();
        if (foundCount != distinct.size()) {
            throw new CustomException(ErrorCode.ARTICLE_INVALID_RECIPE_REF);
        }
    }

    private void saveRefs(CurationArticle article, List<Long> recipeIds) {
        if (recipeIds == null || recipeIds.isEmpty()) return;
        List<CurationArticleRecipeRef> refs = recipeIds.stream()
                .distinct()
                .map(rid -> CurationArticleRecipeRef.builder()
                        .article(article)
                        .recipeId(rid)
                        .build())
                .toList();
        refRepo.saveAll(refs);
    }

    private static String nullIfBlank(String s) {
        return (s == null || s.isBlank()) ? null : s;
    }
}
