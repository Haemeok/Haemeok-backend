package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.CurationArticleRecommendationResponse;
import com.jdc.recipe_service.domain.dto.article.CurationArticleSitemapResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.projection.article.CurationArticleRecommendationProjection;
import com.jdc.recipe_service.domain.projection.article.CurationArticleSitemapProjection;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRecipeRefRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
class PublicCurationArticleServiceTest {

    @Mock private CurationArticleRepository articleRepo;
    @Mock private CurationArticleRecipeRefRepository refRepo;

    @InjectMocks private PublicCurationArticleService publicService;

    @Test
    @DisplayName("listPublished는 status=PUBLISHED + q=null로 search를 부르고, sort를 publishedAt DESC, id DESC로 강제한다")
    void listPublished_enforcesPublishedAndSort() {
        Pageable userProvided = PageRequest.of(2, 10, Sort.by(Sort.Order.asc("title")));
        ArgumentCaptor<Pageable> pageableCaptor = ArgumentCaptor.forClass(Pageable.class);
        given(articleRepo.search(eq(ArticleStatus.PUBLISHED), eq("diet"), isNull(), pageableCaptor.capture()))
                .willReturn(new PageImpl<>(List.of()));

        Page<PublicCurationArticleSummaryResponse> result =
                publicService.listPublished("diet", userProvided);

        assertThat(result).isEmpty();
        Pageable used = pageableCaptor.getValue();
        // 페이지/사이즈는 그대로 유지
        assertThat(used.getPageNumber()).isEqualTo(2);
        assertThat(used.getPageSize()).isEqualTo(10);
        // 사용자가 보낸 title ASC 정렬은 무시되고 publishedAt DESC, id DESC가 강제된다
        Sort sort = used.getSort();
        assertThat(sort.getOrderFor("publishedAt"))
                .isNotNull()
                .extracting(Sort.Order::getDirection)
                .isEqualTo(Sort.Direction.DESC);
        assertThat(sort.getOrderFor("id"))
                .isNotNull()
                .extracting(Sort.Order::getDirection)
                .isEqualTo(Sort.Direction.DESC);
        assertThat(sort.getOrderFor("title")).isNull();
    }

    @Test
    @DisplayName("listPublished의 category가 빈 문자열이면 null로 변환되어 search에 전달된다")
    void listPublished_blankCategoryBecomesNull() {
        given(articleRepo.search(eq(ArticleStatus.PUBLISHED), isNull(), isNull(), org.mockito.ArgumentMatchers.any()))
                .willReturn(new PageImpl<>(List.of()));

        publicService.listPublished("   ", PageRequest.of(0, 20));
        // mock matcher가 null category를 만족하면 통과 (설정한 stub이 매칭됨)
    }

    @Test
    @DisplayName("getBySlug는 PUBLISHED 상태만 조회 — DRAFT/ARCHIVED slug는 ARTICLE_NOT_FOUND")
    void getBySlug_unpublishedReturns404() {
        // findBySlugAndStatus(slug, PUBLISHED)는 DRAFT/ARCHIVED인 글을 못 찾는다 → empty Optional
        given(articleRepo.findBySlugAndStatus("hidden-glug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.empty());

        assertThatThrownBy(() -> publicService.getBySlug("hidden-glug"))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_NOT_FOUND);
    }

    @Test
    @DisplayName("listPublished는 size를 50으로 clamp한다 (public API 부담 방지)")
    void listPublished_clampsPageSizeTo50() {
        ArgumentCaptor<Pageable> pageableCaptor = ArgumentCaptor.forClass(Pageable.class);
        given(articleRepo.search(eq(ArticleStatus.PUBLISHED), isNull(), isNull(), pageableCaptor.capture()))
                .willReturn(new PageImpl<>(List.of()));

        // 1000을 요청해도 service에서 50으로 자른다
        publicService.listPublished(null, PageRequest.of(0, 1000));

        assertThat(pageableCaptor.getValue().getPageSize()).isEqualTo(50);
    }

    @Test
    @DisplayName("listSitemap은 repository.findAllForSitemap을 호출하고 projection을 DTO로 매핑한다 (slug + updatedAt만)")
    void listSitemap_mapsProjectionToDto() {
        // status 필터 + 정렬은 repository @Query 안에서 강제되므로 service 단위 테스트는
        // (a) repository를 정확히 1회 호출하고 (b) projection의 slug/updatedAt이 DTO에 그대로
        // 매핑되며 (c) projection 순서가 DTO 응답 순서로 보존되는지 검증한다.
        CurationArticleSitemapProjection p1 = sitemapProjection("a-1", LocalDateTime.of(2026, 5, 5, 10, 0));
        CurationArticleSitemapProjection p2 = sitemapProjection("a-2", LocalDateTime.of(2026, 5, 4, 9, 0));
        given(articleRepo.findAllForSitemap()).willReturn(List.of(p1, p2));

        List<CurationArticleSitemapResponse> result = publicService.listSitemap();

        assertThat(result).hasSize(2);
        assertThat(result.get(0).getSlug()).isEqualTo("a-1");
        assertThat(result.get(0).getUpdatedAt()).isEqualTo(LocalDateTime.of(2026, 5, 5, 10, 0));
        assertThat(result.get(1).getSlug()).isEqualTo("a-2");
        assertThat(result.get(1).getUpdatedAt()).isEqualTo(LocalDateTime.of(2026, 5, 4, 9, 0));
        org.mockito.Mockito.verify(articleRepo, org.mockito.Mockito.times(1)).findAllForSitemap();
    }

    @Test
    @DisplayName("listSitemap은 빈 결과를 빈 List로 반환한다 (null 아님)")
    void listSitemap_emptyReturnsEmptyList() {
        given(articleRepo.findAllForSitemap()).willReturn(List.of());

        List<CurationArticleSitemapResponse> result = publicService.listSitemap();

        assertThat(result).isNotNull().isEmpty();
    }

    private CurationArticleSitemapProjection sitemapProjection(String slug, LocalDateTime updatedAt) {
        return new CurationArticleSitemapProjection() {
            @Override public String getSlug() { return slug; }
            @Override public LocalDateTime getUpdatedAt() { return updatedAt; }
        };
    }

    @Test
    @DisplayName("getBySlug는 PUBLISHED 글에 대해 본문/recipeIds를 함께 응답한다")
    void getBySlug_returnsArticleWithRecipeIds() {
        CurationArticle article = CurationArticle.builder()
                .slug("summer-diet")
                .title("여름 다이어트")
                .contentMdx("# body")
                .status(ArticleStatus.PUBLISHED)
                .build();
        ReflectionTestUtils.setField(article, "id", 7L);

        given(articleRepo.findBySlugAndStatus("summer-diet", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(article));
        given(refRepo.findRecipeIdsByArticleId(7L)).willReturn(List.of(11L, 12L));

        PublicCurationArticleResponse resp = publicService.getBySlug("summer-diet");

        assertThat(resp.getId()).isEqualTo(7L);
        assertThat(resp.getSlug()).isEqualTo("summer-diet");
        assertThat(resp.getTitle()).isEqualTo("여름 다이어트");
        assertThat(resp.getContentMdx()).isEqualTo("# body");
        assertThat(resp.getRecipeIds()).containsExactly(11L, 12L);
    }

    // ─────────────────────────────────────────────────────────────────────
    // listRecommendations
    // ─────────────────────────────────────────────────────────────────────

    private CurationArticle currentArticle(long id, String category) {
        CurationArticle a = CurationArticle.builder()
                .slug("current-slug")
                .title("current title")
                .contentMdx("# x")
                .category(category)
                .status(ArticleStatus.PUBLISHED)
                .build();
        ReflectionTestUtils.setField(a, "id", id);
        return a;
    }

    private CurationArticleRecommendationProjection rec(long id, String slug, String title,
                                                        String coverImageKey, String category) {
        return new CurationArticleRecommendationProjection() {
            @Override public Long getId() { return id; }
            @Override public String getSlug() { return slug; }
            @Override public String getTitle() { return title; }
            @Override public String getCoverImageKey() { return coverImageKey; }
            @Override public String getCategory() { return category; }
        };
    }

    @Test
    @DisplayName("listRecommendations: current article이 PUBLISHED가 아니면 ARTICLE_NOT_FOUND")
    void listRecommendations_currentNotPublished_returnsArticleNotFound() {
        // findBySlugAndStatus(slug, PUBLISHED)가 DRAFT/ARCHIVED 글에 대해 empty Optional → 동일하게 not found.
        given(articleRepo.findBySlugAndStatus("missing-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.empty());

        assertThatThrownBy(() -> publicService.listRecommendations("missing-slug", null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.ARTICLE_NOT_FOUND);
    }

    @Test
    @DisplayName("listRecommendations: size=null이면 default 6, sameCategoryTarget=3, exploreTarget=3")
    void listRecommendations_defaultSize_splitsThreeAndThree() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        // sameCategory 후보 5개 → 3개 추출
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of(
                        rec(11L, "s11", "t11", "ck11", "diet"),
                        rec(12L, "s12", "t12", "ck12", "diet"),
                        rec(13L, "s13", "t13", "ck13", "diet"),
                        rec(14L, "s14", "t14", "ck14", "diet"),
                        rec(15L, "s15", "t15", "ck15", "diet")
                ));
        // explore 후보 5개 (12, 13은 sameCategory 후보와 id 겹침 — dedup 검증)
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(List.of(
                        rec(12L, "s12", "t12", "ck12", "diet"),
                        rec(13L, "s13", "t13", "ck13", "diet"),
                        rec(20L, "s20", "t20", "ck20", "winter"),
                        rec(21L, "s21", "t21", "ck21", "winter"),
                        rec(22L, "s22", "t22", "ck22", "spring")
                ));

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", null);

        assertThat(result).hasSize(6);
        // sameCategory 3개 + explore 3개. sameCategory에서 뽑힌 id가 explore에서 다시 안 나와야 한다.
        long uniqueSlugs = result.stream().map(CurationArticleRecommendationResponse::getSlug).distinct().count();
        assertThat(uniqueSlugs).isEqualTo(6);
    }

    @Test
    @DisplayName("listRecommendations: size=2면 4로 clamp (sameCategoryTarget=min(3, 2)=2, exploreTarget=2)")
    void listRecommendations_belowMin_clampsToFour() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of(
                        rec(11L, "s11", "t11", null, "diet"),
                        rec(12L, "s12", "t12", null, "diet"),
                        rec(13L, "s13", "t13", null, "diet"),
                        rec(14L, "s14", "t14", null, "diet")
                ));
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(List.of(
                        rec(20L, "s20", "t20", null, "winter"),
                        rec(21L, "s21", "t21", null, "winter"),
                        rec(22L, "s22", "t22", null, "spring")
                ));

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", 2);

        assertThat(result).hasSize(4);
    }

    @Test
    @DisplayName("listRecommendations: size=20이면 12로 clamp (sameCategoryTarget=min(3, 6)=3, exploreTarget=9)")
    void listRecommendations_aboveMax_clampsToTwelve() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        // sameCategory 후보 5개 → 최대 3개만 사용
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of(
                        rec(11L, "s11", "t11", null, "diet"),
                        rec(12L, "s12", "t12", null, "diet"),
                        rec(13L, "s13", "t13", null, "diet"),
                        rec(14L, "s14", "t14", null, "diet"),
                        rec(15L, "s15", "t15", null, "diet")
                ));
        // explore 후보를 충분히 줘서 12를 채운다
        List<CurationArticleRecommendationProjection> exploreList = new java.util.ArrayList<>();
        for (long i = 20; i < 35; i++) {
            exploreList.add(rec(i, "s" + i, "t" + i, null, "winter"));
        }
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(exploreList);

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", 20);

        assertThat(result).hasSize(12);
    }

    @Test
    @DisplayName("listRecommendations: category가 null이면 sameCategory 호출 안 하고 explore만 사용")
    void listRecommendations_nullCategory_skipsSameCategoryQuery() {
        CurationArticle current = currentArticle(7L, null);
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        List<CurationArticleRecommendationProjection> exploreList = new java.util.ArrayList<>();
        for (long i = 20; i < 30; i++) {
            exploreList.add(rec(i, "s" + i, "t" + i, null, null));
        }
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(exploreList);

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", null);

        assertThat(result).hasSize(6);
        // category null이면 sameCategory query 자체를 호출하지 않는다 (DB round trip 절감)
        org.mockito.Mockito.verify(articleRepo, org.mockito.Mockito.never())
                .findRecommendationCandidatesByCategory(any(), any());
    }

    @Test
    @DisplayName("listRecommendations: sameCategory 부족 시 explore가 부족분만큼 추가로 채운다")
    void listRecommendations_sameCategoryShort_exploreFillsGap() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        // sameCategory 후보 1개 (target=3보다 적음)
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of(rec(11L, "s11", "t11", null, "diet")));
        // explore 후보 6개 — sameCategory 부족분 만큼 더 채워서 size=6 채워야 한다
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(List.of(
                        rec(20L, "s20", "t20", null, "winter"),
                        rec(21L, "s21", "t21", null, "winter"),
                        rec(22L, "s22", "t22", null, "winter"),
                        rec(23L, "s23", "t23", null, "winter"),
                        rec(24L, "s24", "t24", null, "winter"),
                        rec(25L, "s25", "t25", null, "winter")
                ));

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", 6);

        // sameCategory 1 + explore 5 = 6
        assertThat(result).hasSize(6);
        long sameCategoryCount = result.stream()
                .filter(r -> "diet".equals(r.getCategory()))
                .count();
        assertThat(sameCategoryCount).isEqualTo(1);
    }

    @Test
    @DisplayName("listRecommendations: 후보가 부족하면 가능한 만큼만 반환 (0개면 빈 배열)")
    void listRecommendations_noCandidates_returnsEmpty() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of());
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(List.of());

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", null);

        assertThat(result).isNotNull().isEmpty();
    }

    @Test
    @DisplayName("listRecommendations: 같은 입력은 같은 추천 순서를 반환한다 (deterministic)")
    void listRecommendations_isDeterministic() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        // 같은 후보 set
        List<CurationArticleRecommendationProjection> sameCat = List.of(
                rec(11L, "s11", "t11", null, "diet"),
                rec(12L, "s12", "t12", null, "diet"),
                rec(13L, "s13", "t13", null, "diet"),
                rec(14L, "s14", "t14", null, "diet"));
        List<CurationArticleRecommendationProjection> explore = List.of(
                rec(20L, "s20", "t20", null, "winter"),
                rec(21L, "s21", "t21", null, "winter"),
                rec(22L, "s22", "t22", null, "spring"),
                rec(23L, "s23", "t23", null, "spring"));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(sameCat);
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(explore);

        List<String> firstCall = publicService.listRecommendations("current-slug", null)
                .stream().map(CurationArticleRecommendationResponse::getSlug).toList();
        List<String> secondCall = publicService.listRecommendations("current-slug", null)
                .stream().map(CurationArticleRecommendationResponse::getSlug).toList();

        assertThat(firstCall).isEqualTo(secondCall);
    }

    @Test
    @DisplayName("listRecommendations: 다른 currentArticleId면 다른 추천 분포가 나올 수 있다 (deterministic but distinct)")
    void listRecommendations_differentCurrentArticle_canProduceDifferentOrder() {
        // 후보 set은 동일하지만 current article id가 다르면 score 함수 결과가 달라져 정렬 결과가 달라질 수 있다.
        // (반드시 다르다는 보장은 아니므로, 보통 케이스에서 다르게 나오는 시드를 사용한다.)
        List<CurationArticleRecommendationProjection> sameCat = List.of(
                rec(11L, "s11", "t11", null, "diet"),
                rec(12L, "s12", "t12", null, "diet"),
                rec(13L, "s13", "t13", null, "diet"),
                rec(14L, "s14", "t14", null, "diet"));
        List<CurationArticleRecommendationProjection> explore = List.of(
                rec(20L, "s20", "t20", null, "winter"),
                rec(21L, "s21", "t21", null, "winter"),
                rec(22L, "s22", "t22", null, "spring"),
                rec(23L, "s23", "t23", null, "spring"));

        CurationArticle current1 = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("a", ArticleStatus.PUBLISHED)).willReturn(Optional.of(current1));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L))).willReturn(sameCat);
        given(articleRepo.findRecommendationExploreCandidates(eq(7L))).willReturn(explore);
        List<String> orderForId7 = publicService.listRecommendations("a", null)
                .stream().map(CurationArticleRecommendationResponse::getSlug).toList();

        CurationArticle current2 = currentArticle(99L, "diet");
        given(articleRepo.findBySlugAndStatus("b", ArticleStatus.PUBLISHED)).willReturn(Optional.of(current2));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(99L))).willReturn(sameCat);
        given(articleRepo.findRecommendationExploreCandidates(eq(99L))).willReturn(explore);
        List<String> orderForId99 = publicService.listRecommendations("b", null)
                .stream().map(CurationArticleRecommendationResponse::getSlug).toList();

        // 동일 후보 set, 동일 size로 호출했지만 currentId가 달라서 score 분포가 달라짐 → 결과 순서가 다를 수 있다.
        // (이 시드 조합은 실제로 다르게 나오도록 잡았다.)
        assertThat(orderForId7).isNotEqualTo(orderForId99);
    }

    @Test
    @DisplayName("listRecommendations: 응답에는 sameCategory 카드가 explore 카드보다 앞에 온다 (안정 순서)")
    void listRecommendations_sameCategoryAppearsBeforeExplore() {
        CurationArticle current = currentArticle(7L, "diet");
        given(articleRepo.findBySlugAndStatus("current-slug", ArticleStatus.PUBLISHED))
                .willReturn(Optional.of(current));
        given(articleRepo.findRecommendationCandidatesByCategory(eq("diet"), eq(7L)))
                .willReturn(List.of(
                        rec(11L, "diet-1", "t11", null, "diet"),
                        rec(12L, "diet-2", "t12", null, "diet"),
                        rec(13L, "diet-3", "t13", null, "diet")));
        given(articleRepo.findRecommendationExploreCandidates(eq(7L)))
                .willReturn(List.of(
                        rec(20L, "winter-1", "t20", null, "winter"),
                        rec(21L, "winter-2", "t21", null, "winter"),
                        rec(22L, "winter-3", "t22", null, "winter")));

        List<CurationArticleRecommendationResponse> result =
                publicService.listRecommendations("current-slug", null);

        // 처음 3개는 모두 same category, 마지막 3개는 explore
        assertThat(result).hasSize(6);
        for (int i = 0; i < 3; i++) {
            assertThat(result.get(i).getCategory()).isEqualTo("diet");
        }
        for (int i = 3; i < 6; i++) {
            assertThat(result.get(i).getCategory()).isEqualTo("winter");
        }
    }
}
