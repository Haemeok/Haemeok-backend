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
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

/**
 * 큐레이션 아티클 어드민 유스케이스.
 *
 * <p>V1에서는 status 전이 제약을 두지 않는다 (publish/archive 모두 idempotent). 향후 더 엄격한 정책이 필요해지면
 * ARTICLE_INVALID_STATUS_TRANSITION을 사용해 게이트한다.
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class CurationArticleService {

    private final CurationArticleRepository articleRepo;
    private final CurationArticleRecipeRefRepository refRepo;
    private final RecipeRepository recipeRepo;

    /**
     * public {@code /api/curation-articles/{slug}}와 같은 prefix 아래 literal route가 잡고 있는 segment 목록.
     * 이 값들이 slug로 들어오면 {@code GET /{slug}} 매핑이 영영 도달 불가능해지므로 create 시점에 차단한다.
     *
     * <p>새 literal route(예: 추후 {@code /api/curation-articles/feed} 같은 것)가
     * {@link com.jdc.recipe_service.controller.CurationArticleController}에 추가되면 그 segment를 여기에도 추가해야 한다.
     */
    private static final Set<String> RESERVED_SLUGS = Set.of("sitemap");

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

    @Transactional
    public Long create(CurationArticleCreateRequest req) {
        if (RESERVED_SLUGS.contains(req.getSlug())) {
            throw new CustomException(ErrorCode.ARTICLE_SLUG_RESERVED);
        }
        if (articleRepo.existsBySlug(req.getSlug())) {
            throw new CustomException(ErrorCode.ARTICLE_SLUG_DUPLICATE);
        }
        validateRecipeIds(req.getRecipeIds());

        CurationArticle article = CurationArticle.builder()
                .slug(req.getSlug())
                .title(req.getTitle())
                .description(req.getDescription())
                .coverImageKey(req.getCoverImageKey())
                .contentMdx(req.getContentMdx())
                .category(req.getCategory())
                .generatedBy(req.getGeneratedBy())
                .build();
        CurationArticle saved = articleRepo.save(article);

        saveRefs(saved, req.getRecipeIds());
        return saved.getId();
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
