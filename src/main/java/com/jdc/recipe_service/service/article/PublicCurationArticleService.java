package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.CurationArticleRecommendationResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSitemapResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.projection.article.CurationArticleRecommendationProjection;
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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    /** 추천 size 정책. 4 ≤ size ≤ 12, default 6. */
    static final int RECOMMENDATION_DEFAULT_SIZE = 6;
    static final int RECOMMENDATION_MIN_SIZE = 4;
    static final int RECOMMENDATION_MAX_SIZE = 12;

    /** sameCategory cap. 절반 vs 3 중 작은 값. (safeSize=12면 6이 아니라 3이 cap) */
    static final int SAME_CATEGORY_HARD_CAP = 3;

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

    /**
     * 아티클 상세 페이지 하단 "추천" 카드용 — sameCategory + explore 두 갈래로 나눠 deterministic random으로 섞는다.
     *
     * <p>같은 {@code currentArticleId}에 대해선 추천 순서가 안정적이고(새로고침해도 동일), 다른 article로 가면 다른 추천이
     * 나오도록 score 함수에 두 id를 모두 넣는다. {@code ORDER BY RAND()}는 매 요청마다 결과가 바뀌고 인덱스도 안 타므로 금지.
     *
     * <p>safeSize는 [4, 12]로 clamp. sameCategoryTarget = min(3, safeSize/2) — single category가 추천을 독점하지 않게
     * 의도적으로 cap. category가 비어있으면 sameCategoryTarget=0, 전부 explore에서 채운다. sameCategory 후보가 부족하면
     * 모자란 만큼 explore가 더 채운다.
     */
    public List<CurationArticleRecommendationResponse> listRecommendations(String slug, Integer size) {
        CurationArticle current = articleRepo.findBySlugAndStatus(slug, ArticleStatus.PUBLISHED)
                .orElseThrow(() -> new CustomException(ErrorCode.ARTICLE_NOT_FOUND));

        int safeSize = clampSize(size);
        int sameCategoryTarget = computeSameCategoryTarget(current.getCategory(), safeSize);

        List<CurationArticleRecommendationProjection> sameCategoryPicked =
                pickSameCategory(current, sameCategoryTarget);

        int exploreTarget = safeSize - sameCategoryPicked.size();
        List<CurationArticleRecommendationProjection> explorePicked =
                pickExplore(current, exploreTarget, sameCategoryPicked);

        List<CurationArticleRecommendationProjection> merged = new ArrayList<>(sameCategoryPicked.size() + explorePicked.size());
        merged.addAll(sameCategoryPicked);
        merged.addAll(explorePicked);

        return merged.stream()
                .map(CurationArticleRecommendationResponse::of)
                .toList();
    }

    private int clampSize(Integer size) {
        if (size == null) return RECOMMENDATION_DEFAULT_SIZE;
        if (size < RECOMMENDATION_MIN_SIZE) return RECOMMENDATION_MIN_SIZE;
        if (size > RECOMMENDATION_MAX_SIZE) return RECOMMENDATION_MAX_SIZE;
        return size;
    }

    private int computeSameCategoryTarget(String currentCategory, int safeSize) {
        if (currentCategory == null || currentCategory.isBlank()) return 0;
        return Math.min(SAME_CATEGORY_HARD_CAP, safeSize / 2);
    }

    private List<CurationArticleRecommendationProjection> pickSameCategory(CurationArticle current, int target) {
        if (target <= 0) return List.of();
        List<CurationArticleRecommendationProjection> candidates = articleRepo.findRecommendationCandidatesByCategory(
                current.getCategory(), current.getId());
        return shuffleDeterministic(candidates, current.getId()).stream()
                .limit(target)
                .toList();
    }

    private List<CurationArticleRecommendationProjection> pickExplore(
            CurationArticle current,
            int target,
            List<CurationArticleRecommendationProjection> alreadyPicked) {
        if (target <= 0) return List.of();
        Set<Long> excludeIds = new HashSet<>();
        for (CurationArticleRecommendationProjection p : alreadyPicked) excludeIds.add(p.getId());

        List<CurationArticleRecommendationProjection> candidates = articleRepo.findRecommendationExploreCandidates(
                current.getId());
        List<CurationArticleRecommendationProjection> filtered = new ArrayList<>(candidates.size());
        for (CurationArticleRecommendationProjection p : candidates) {
            if (!excludeIds.contains(p.getId())) filtered.add(p);
        }
        return shuffleDeterministic(filtered, current.getId()).stream()
                .limit(target)
                .toList();
    }

    /**
     * deterministic 정렬. splitmix64 기반 비선형 mixer로 score를 만든다.
     *
     * <p>왜 단순 hash가 아닌가: {@code Objects.hash(curr, cand, slug)}는 polynomial hash가 linear라서
     * 두 후보의 score 차이에서 curr 항이 상쇄돼 ordering에 영향을 못 준다. {@code String concat + hashCode}도
     * 짧은 입력에선 비슷한 ordering 패턴을 만들기 쉽다. splitmix64는 단방향 비선형 bit-mixing이라
     * 입력 1비트 변화에도 출력이 골고루 흩어진다 → currentArticleId가 바뀌면 후보 간 상대 순서도 함께 바뀐다.
     *
     * <p>동률(score 충돌)이 나도 candidateId secondary로 안정 정렬.
     */
    private static List<CurationArticleRecommendationProjection> shuffleDeterministic(
            List<CurationArticleRecommendationProjection> candidates, Long currentArticleId) {
        List<CurationArticleRecommendationProjection> copy = new ArrayList<>(candidates);
        copy.sort(Comparator
                .comparingLong((CurationArticleRecommendationProjection p) ->
                        deterministicScore(currentArticleId, p.getId(), p.getSlug()))
                .thenComparingLong(CurationArticleRecommendationProjection::getId));
        return copy;
    }

    private static long deterministicScore(long currentId, long candidateId, String candidateSlug) {
        long h = splitmix64(currentId);
        h = splitmix64(h ^ candidateId);
        h = splitmix64(h ^ (candidateSlug != null ? candidateSlug.hashCode() : 0));
        return h;
    }

    /** splitmix64 — 단방향 비선형 bit-mixer. avalanche 좋음. */
    private static long splitmix64(long x) {
        x = (x ^ (x >>> 30)) * 0xbf58476d1ce4e5b9L;
        x = (x ^ (x >>> 27)) * 0x94d049bb133111ebL;
        return x ^ (x >>> 31);
    }

    /**
     * sitemap.xml 생성용 — PUBLISHED 아티클의 slug + updatedAt만 노출한다.
     * id/본문/status는 노출하지 않는다 (검색엔진 크롤러용 외부면이라 필요 최소 필드만).
     * 정렬과 status 필터는 repository query에서 강제된다.
     */
    public List<CurationArticleSitemapResponse> listSitemap() {
        return articleRepo.findAllForSitemap().stream()
                .map(p -> CurationArticleSitemapResponse.builder()
                        .slug(p.getSlug())
                        .updatedAt(p.getUpdatedAt())
                        .build())
                .toList();
    }

    private static String nullIfBlank(String s) {
        return (s == null || s.isBlank()) ? null : s;
    }
}
