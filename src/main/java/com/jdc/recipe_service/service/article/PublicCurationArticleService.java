package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRecipeRefRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * Public 큐레이션 아티클 조회 유스케이스.
 *
 * <p>admin {@link CurationArticleService}와 클래스를 분리해 PUBLISHED 외 상태가 절대 노출되지 않도록 한다 —
 * 단일 service에서 분기를 두면 향후 메서드 추가 시 PUBLISHED 필터를 빠뜨릴 위험이 있다.
 *
 * <p>정렬은 service 레이어에서 publishedAt DESC, id DESC로 강제한다 — 외부에서 sort 파라미터를 조작해
 * 다른 정렬을 받아갈 수 없게 한다 (캐시/CDN 키 안정성에도 도움).
 *
 * <p>page size는 public API 특성상 임의의 큰 값(예: size=100000)으로 인한 서버 부담을 막기 위해 service에서
 * {@link #MAX_PAGE_SIZE}로 clamp한다.
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class PublicCurationArticleService {

    static final int MAX_PAGE_SIZE = 50;

    private final CurationArticleRepository articleRepo;
    private final CurationArticleRecipeRefRepository refRepo;

    public Page<PublicCurationArticleSummaryResponse> listPublished(String category, Pageable pageable) {
        int clampedSize = Math.min(Math.max(pageable.getPageSize(), 1), MAX_PAGE_SIZE);
        Pageable enforced = PageRequest.of(
                pageable.getPageNumber(),
                clampedSize,
                Sort.by(Sort.Order.desc("publishedAt"), Sort.Order.desc("id"))
        );
        return articleRepo
                .search(ArticleStatus.PUBLISHED, nullIfBlank(category), null, enforced)
                .map(PublicCurationArticleSummaryResponse::of);
    }

    public PublicCurationArticleResponse getBySlug(String slug) {
        CurationArticle article = articleRepo.findBySlugAndStatus(slug, ArticleStatus.PUBLISHED)
                .orElseThrow(() -> new CustomException(ErrorCode.ARTICLE_NOT_FOUND));
        List<Long> recipeIds = refRepo.findRecipeIdsByArticleId(article.getId());
        return PublicCurationArticleResponse.of(article, recipeIds);
    }

    private static String nullIfBlank(String s) {
        return (s == null || s.isBlank()) ? null : s;
    }
}
