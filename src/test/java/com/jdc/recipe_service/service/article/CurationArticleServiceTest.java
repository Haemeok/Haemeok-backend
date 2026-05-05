package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.CurationArticleCreateRequest;
import com.jdc.recipe_service.domain.dto.article.CurationArticleUpdateRequest;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.entity.article.CurationArticleRecipeRef;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRecipeRefRepository;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CurationArticleServiceTest {

    @Mock private CurationArticleRepository articleRepo;
    @Mock private CurationArticleRecipeRefRepository refRepo;
    @Mock private RecipeRepository recipeRepo;

    @InjectMocks private CurationArticleService articleService;

    private CurationArticle draftArticle;

    @BeforeEach
    void setUp() {
        draftArticle = CurationArticle.builder()
                .slug("summer-diet")
                .title("여름 다이어트")
                .contentMdx("# body")
                .status(ArticleStatus.DRAFT)
                .humanReviewed(false)
                .build();
        ReflectionTestUtils.setField(draftArticle, "id", 100L);
    }

    @Nested
    @DisplayName("create")
    class Create {

        @Test
        @DisplayName("slug이 이미 존재하면 ARTICLE_SLUG_DUPLICATE 예외를 던진다")
        void throwsWhenSlugDuplicate() {
            given(articleRepo.existsBySlug("summer-diet")).willReturn(true);

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("t")
                    .contentMdx("c")
                    .build();

            assertThatThrownBy(() -> articleService.create(req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_SLUG_DUPLICATE);

            verify(articleRepo, never()).save(any());
        }

        @Test
        @DisplayName("recipeIds 중 존재하지 않는 ID가 있으면 ARTICLE_INVALID_RECIPE_REF 예외를 던진다")
        void throwsWhenRecipeRefInvalid() {
            given(articleRepo.existsBySlug("summer-diet")).willReturn(false);
            given(recipeRepo.findAllById(List.of(1L, 2L)))
                    .willReturn(List.of(Recipe.builder().build()));

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("t")
                    .contentMdx("c")
                    .recipeIds(List.of(1L, 2L))
                    .build();

            assertThatThrownBy(() -> articleService.create(req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_INVALID_RECIPE_REF);

            verify(articleRepo, never()).save(any());
            verify(refRepo, never()).saveAll(any());
        }

        @Test
        @DisplayName("recipeIds에 null이 섞이면 findAllById를 부르지 않고 ARTICLE_INVALID_RECIPE_REF로 차단된다")
        void throwsWhenRecipeIdContainsNull() {
            given(articleRepo.existsBySlug("summer-diet")).willReturn(false);

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("t")
                    .contentMdx("c")
                    // List.of는 null을 거부하므로 Arrays.asList 사용
                    .recipeIds(Arrays.asList(1L, null))
                    .build();

            assertThatThrownBy(() -> articleService.create(req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_INVALID_RECIPE_REF);

            verify(recipeRepo, never()).findAllById(any());
            verify(articleRepo, never()).save(any());
        }

        @Test
        @DisplayName("recipeIds에 비양수(0/음수)가 섞이면 ARTICLE_INVALID_RECIPE_REF로 차단된다")
        void throwsWhenRecipeIdNonPositive() {
            given(articleRepo.existsBySlug("summer-diet")).willReturn(false);

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("t")
                    .contentMdx("c")
                    .recipeIds(List.of(1L, 0L, -3L))
                    .build();

            assertThatThrownBy(() -> articleService.create(req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_INVALID_RECIPE_REF);

            verify(recipeRepo, never()).findAllById(any());
            verify(articleRepo, never()).save(any());
        }

        @Test
        @DisplayName("slug이 reserved 목록(sitemap)이면 ARTICLE_SLUG_RESERVED 예외 (route 충돌 방지)")
        void create_reservedSlug_returns400() {
            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("sitemap")
                    .title("t")
                    .contentMdx("c")
                    .build();

            assertThatThrownBy(() -> articleService.create(req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_SLUG_RESERVED);

            // reserved 검증이 가장 먼저 — DB(existsBySlug)나 recipeRepo가 호출되지 않는다
            verify(articleRepo, never()).existsBySlug(any());
            verify(recipeRepo, never()).findAllById(any());
            verify(articleRepo, never()).save(any());
        }

        @Test
        @DisplayName("정상 생성 시 article 저장 + refs도 저장한다")
        void savesArticleAndRefs() {
            given(articleRepo.existsBySlug("summer-diet")).willReturn(false);
            given(recipeRepo.findAllById(List.of(1L, 2L)))
                    .willReturn(List.of(Recipe.builder().build(), Recipe.builder().build()));
            given(articleRepo.save(any(CurationArticle.class))).willReturn(draftArticle);

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("여름 다이어트")
                    .contentMdx("# body")
                    .recipeIds(List.of(1L, 2L))
                    .build();

            Long id = articleService.create(req);

            assertThat(id).isEqualTo(100L);
            ArgumentCaptor<List<CurationArticleRecipeRef>> captor =
                    ArgumentCaptor.forClass(List.class);
            verify(refRepo).saveAll(captor.capture());
            assertThat(captor.getValue()).hasSize(2);
            assertThat(captor.getValue())
                    .extracting(CurationArticleRecipeRef::getRecipeId)
                    .containsExactlyInAnyOrder(1L, 2L);
        }
    }

    @Nested
    @DisplayName("update")
    class Update {

        @Test
        @DisplayName("본문이 바뀌면 humanReviewed가 false로 리셋된다")
        void resetsHumanReviewed() {
            draftArticle.markReviewed();
            assertThat(draftArticle.isHumanReviewed()).isTrue();
            given(articleRepo.findById(100L)).willReturn(Optional.of(draftArticle));

            CurationArticleUpdateRequest req = CurationArticleUpdateRequest.builder()
                    .title("새 제목")
                    .contentMdx("# new body")
                    .build();

            articleService.update(100L, req);

            assertThat(draftArticle.isHumanReviewed()).isFalse();
            assertThat(draftArticle.getTitle()).isEqualTo("새 제목");
        }

        @Test
        @DisplayName("recipeIds가 바뀌면 기존 refs를 모두 삭제한 뒤 새 refs를 저장한다")
        void replacesRecipeRefs() {
            given(articleRepo.findById(100L)).willReturn(Optional.of(draftArticle));
            given(recipeRepo.findAllById(List.of(7L, 8L)))
                    .willReturn(List.of(Recipe.builder().build(), Recipe.builder().build()));

            CurationArticleUpdateRequest req = CurationArticleUpdateRequest.builder()
                    .title("t")
                    .contentMdx("c")
                    .recipeIds(List.of(7L, 8L))
                    .build();

            articleService.update(100L, req);

            verify(refRepo).deleteByArticleId(100L);
            ArgumentCaptor<List<CurationArticleRecipeRef>> captor =
                    ArgumentCaptor.forClass(List.class);
            verify(refRepo).saveAll(captor.capture());
            assertThat(captor.getValue())
                    .extracting(CurationArticleRecipeRef::getRecipeId)
                    .containsExactlyInAnyOrder(7L, 8L);
        }

        @Test
        @DisplayName("아티클이 없으면 ARTICLE_NOT_FOUND")
        void throwsWhenArticleMissing() {
            given(articleRepo.findById(999L)).willReturn(Optional.empty());

            CurationArticleUpdateRequest req = CurationArticleUpdateRequest.builder()
                    .title("t").contentMdx("c").build();

            assertThatThrownBy(() -> articleService.update(999L, req))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_NOT_FOUND);
        }
    }

    @Nested
    @DisplayName("publish")
    class Publish {

        @Test
        @DisplayName("DRAFT에서 publish 호출 시 status=PUBLISHED + publishedAt이 채워진다")
        void firstPublishSetsPublishedAt() {
            given(articleRepo.findById(100L)).willReturn(Optional.of(draftArticle));

            articleService.publish(100L);

            assertThat(draftArticle.getStatus()).isEqualTo(ArticleStatus.PUBLISHED);
            assertThat(draftArticle.getPublishedAt()).isNotNull();
        }

        @Test
        @DisplayName("이미 PUBLISHED 상태에서 publish 재호출해도 publishedAt이 보존된다 (idempotent)")
        void repeatedPublishPreservesPublishedAt() {
            // 첫 발행
            given(articleRepo.findById(100L)).willReturn(Optional.of(draftArticle));
            articleService.publish(100L);
            LocalDateTime firstPublishedAt = draftArticle.getPublishedAt();
            assertThat(firstPublishedAt).isNotNull();

            // 재발행 — 시간이 다르게 흐르도록 미세 대기 대신 직접 검증
            articleService.publish(100L);

            assertThat(draftArticle.getStatus()).isEqualTo(ArticleStatus.PUBLISHED);
            assertThat(draftArticle.getPublishedAt())
                    .as("두 번째 publish는 publishedAt을 덮어쓰지 않는다")
                    .isEqualTo(firstPublishedAt);

            verify(articleRepo, times(2)).findById(100L);
        }
    }
}
