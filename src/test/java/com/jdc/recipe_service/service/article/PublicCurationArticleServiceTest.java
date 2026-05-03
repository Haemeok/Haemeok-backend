package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleResponse;
import com.jdc.recipe_service.domain.dto.article.PublicCurationArticleSummaryResponse;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
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

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
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
}
