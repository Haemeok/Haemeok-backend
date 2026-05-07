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
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CurationArticleServiceTest {

    @Mock private CurationArticleRepository articleRepo;
    @Mock private CurationArticleRecipeRefRepository refRepo;
    @Mock private RecipeRepository recipeRepo;
    @Mock private PlatformTransactionManager transactionManager;

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
    @DisplayName("create — 자동 slug suffix 정책")
    class Create {

        /**
         * @PostConstruct가 단위 테스트에선 호출되지 않으므로 writeTxTemplate을 직접 주입한다.
         * fake template은 callback을 그대로 실행해 retry 로직만 검증하고, 트랜잭션 격리 자체는 통합 테스트가 잡는다.
         */
        @BeforeEach
        void wireTxTemplate() {
            TransactionTemplate fakeTxTemplate = mock(TransactionTemplate.class);
            given(fakeTxTemplate.execute(any())).willAnswer(inv -> {
                TransactionCallback<?> callback = inv.getArgument(0);
                return callback.doInTransaction(null);
            });
            ReflectionTestUtils.setField(articleService, "writeTxTemplate", fakeTxTemplate);
        }

        private CurationArticle savedArticle(long id, String slug) {
            CurationArticle a = CurationArticle.builder()
                    .slug(slug)
                    .title("t")
                    .contentMdx("c")
                    .status(ArticleStatus.DRAFT)
                    .build();
            ReflectionTestUtils.setField(a, "id", id);
            return a;
        }

        private CurationArticleCreateRequest reqWithSlug(String slug) {
            return CurationArticleCreateRequest.builder()
                    .slug(slug).title("t").contentMdx("c").build();
        }

        @Test
        @DisplayName("base slug가 비어있으면 base 그대로 저장한다 (suffix 없음)")
        void assignsBaseSlugWhenAvailable() {
            given(articleRepo.findSlugsStartingWith("summer-diet")).willReturn(List.of());
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willAnswer(inv -> {
                        CurationArticle a = inv.getArgument(0);
                        ReflectionTestUtils.setField(a, "id", 100L);
                        return a;
                    });

            CurationArticleCreateResult result = articleService.create(reqWithSlug("summer-diet"));

            assertThat(result.id()).isEqualTo(100L);
            assertThat(result.slug()).isEqualTo("summer-diet");
            ArgumentCaptor<CurationArticle> captor = ArgumentCaptor.forClass(CurationArticle.class);
            verify(articleRepo).saveAndFlush(captor.capture());
            assertThat(captor.getValue().getSlug()).isEqualTo("summer-diet");
        }

        @Test
        @DisplayName("base가 점유돼 있으면 -2 suffix를 붙여 저장한다")
        void assignsSuffixTwoWhenBaseTaken() {
            given(articleRepo.findSlugsStartingWith("summer-diet"))
                    .willReturn(List.of("summer-diet"));
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willReturn(savedArticle(101L, "summer-diet-2"));

            CurationArticleCreateResult result = articleService.create(reqWithSlug("summer-diet"));

            assertThat(result.slug()).isEqualTo("summer-diet-2");
            ArgumentCaptor<CurationArticle> captor = ArgumentCaptor.forClass(CurationArticle.class);
            verify(articleRepo).saveAndFlush(captor.capture());
            assertThat(captor.getValue().getSlug()).isEqualTo("summer-diet-2");
        }

        @Test
        @DisplayName("base + base-2가 점유돼 있으면 -3을 사용한다 (가장 작은 미사용 suffix)")
        void assignsNextAvailableSuffix() {
            given(articleRepo.findSlugsStartingWith("summer-diet"))
                    .willReturn(List.of("summer-diet", "summer-diet-2"));
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willReturn(savedArticle(102L, "summer-diet-3"));

            CurationArticleCreateResult result = articleService.create(reqWithSlug("summer-diet"));

            assertThat(result.slug()).isEqualTo("summer-diet-3");
        }

        @Test
        @DisplayName("점유된 suffix가 비연속이어도 가장 작은 미사용을 채운다 — base + -3 점유 시 -2를 사용")
        void fillsLowestAvailableSuffix() {
            // base, base-3, base-5가 점유 — 가장 작은 미사용은 base-2
            given(articleRepo.findSlugsStartingWith("summer-diet"))
                    .willReturn(List.of("summer-diet", "summer-diet-3", "summer-diet-5"));
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willReturn(savedArticle(103L, "summer-diet-2"));

            CurationArticleCreateResult result = articleService.create(reqWithSlug("summer-diet"));

            assertThat(result.slug()).isEqualTo("summer-diet-2");
        }

        @Test
        @DisplayName("race로 첫 saveAndFlush가 IntegrityViolation이면 새 트랜잭션에서 재시도해 성공한다")
        void retriesOnRace() {
            // 첫 attempt: select 결과 [base] → candidate = base-2 → INSERT race fail
            // 두 번째 attempt: select 결과 [base, base-2] → candidate = base-3 → 성공
            given(articleRepo.findSlugsStartingWith("summer-diet"))
                    .willReturn(List.of("summer-diet"))
                    .willReturn(List.of("summer-diet", "summer-diet-2"));
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willThrow(new DataIntegrityViolationException("unique slug race"))
                    .willReturn(savedArticle(104L, "summer-diet-3"));

            CurationArticleCreateResult result = articleService.create(reqWithSlug("summer-diet"));

            assertThat(result.slug()).isEqualTo("summer-diet-3");
            verify(articleRepo, times(2)).saveAndFlush(any());
        }

        @Test
        @DisplayName("retry 한도(5회) 모두 실패하면 ARTICLE_SLUG_DUPLICATE를 던진다")
        void exhaustsRetriesAndThrows() {
            given(articleRepo.findSlugsStartingWith("summer-diet")).willReturn(List.of());
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    // 메시지에 'slug' 포함 — slug constraint violation으로 식별돼 retry 대상
                    .willThrow(new DataIntegrityViolationException(
                            "Duplicate entry 'summer-diet' for key 'curation_articles.slug'"));

            assertThatThrownBy(() -> articleService.create(reqWithSlug("summer-diet")))
                    .isInstanceOf(CustomException.class)
                    .extracting("errorCode")
                    .isEqualTo(ErrorCode.ARTICLE_SLUG_DUPLICATE);

            verify(articleRepo, times(5)).saveAndFlush(any());
        }

        @Test
        @DisplayName("slug 외 DB 제약 위반(NOT NULL 등)은 retry 없이 그대로 propagate해 원인이 가려지지 않는다")
        void nonSlugConstraintViolationIsNotRetried() {
            // 메시지에 'slug'가 없어 slug constraint violation이 아님 — outer retry는 잡지 않는다.
            DataIntegrityViolationException nonSlugViolation =
                    new DataIntegrityViolationException(
                            "Cannot insert NULL into column 'content_mdx'");
            given(articleRepo.findSlugsStartingWith("summer-diet")).willReturn(List.of());
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willThrow(nonSlugViolation);

            assertThatThrownBy(() -> articleService.create(reqWithSlug("summer-diet")))
                    .isInstanceOf(DataIntegrityViolationException.class)
                    .hasMessageContaining("content_mdx");

            // retry 안 일어나야 한다 — saveAndFlush는 정확히 1번 호출
            verify(articleRepo, times(1)).saveAndFlush(any());
        }

        @Test
        @DisplayName("매우 긴 base slug는 컬럼 길이를 넘지 않도록 잘리고 suffix가 붙는다")
        void trimsBaseToFitColumnLength() {
            // base 길이 250자 → 자른 max는 248 (255 - 7 reserve). 잘린 끝이 hyphen이면 추가 trim.
            String longBase = "a".repeat(250);
            String trimmed = "a".repeat(248);
            given(articleRepo.findSlugsStartingWith(trimmed))
                    .willReturn(List.of(trimmed));
            given(articleRepo.saveAndFlush(any(CurationArticle.class)))
                    .willAnswer(inv -> {
                        CurationArticle a = inv.getArgument(0);
                        ReflectionTestUtils.setField(a, "id", 105L);
                        return a;
                    });

            CurationArticleCreateResult result = articleService.create(reqWithSlug(longBase));

            assertThat(result.slug()).isEqualTo(trimmed + "-2");
            assertThat(result.slug().length()).isLessThanOrEqualTo(255);
        }

        @Test
        @DisplayName("recipeIds 중 존재하지 않는 ID가 있으면 ARTICLE_INVALID_RECIPE_REF 예외를 던진다")
        void throwsWhenRecipeRefInvalid() {
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

            verify(articleRepo, never()).saveAndFlush(any());
            verify(refRepo, never()).saveAll(any());
        }

        @Test
        @DisplayName("recipeIds에 null이 섞이면 findAllById를 부르지 않고 ARTICLE_INVALID_RECIPE_REF로 차단된다")
        void throwsWhenRecipeIdContainsNull() {
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
            verify(articleRepo, never()).saveAndFlush(any());
        }

        @Test
        @DisplayName("recipeIds에 비양수(0/음수)가 섞이면 ARTICLE_INVALID_RECIPE_REF로 차단된다")
        void throwsWhenRecipeIdNonPositive() {
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
            verify(articleRepo, never()).saveAndFlush(any());
        }

        @Test
        @DisplayName("slug이 reserved 목록(sitemap)이면 ARTICLE_SLUG_RESERVED — DB select도 호출되지 않는다")
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

            // reserved 검증이 가장 먼저 — slug 후보 select / recipe ref / saveAndFlush 어느 것도 호출되지 않는다
            verify(articleRepo, never()).findSlugsStartingWith(any());
            verify(recipeRepo, never()).findAllById(any());
            verify(articleRepo, never()).saveAndFlush(any());
        }

        @Test
        @DisplayName("정상 생성 시 article 저장 + refs도 저장하고 result에 id+slug 반환")
        void savesArticleAndRefs() {
            given(articleRepo.findSlugsStartingWith("summer-diet")).willReturn(List.of());
            given(recipeRepo.findAllById(List.of(1L, 2L)))
                    .willReturn(List.of(Recipe.builder().build(), Recipe.builder().build()));
            given(articleRepo.saveAndFlush(any(CurationArticle.class))).willReturn(draftArticle);

            CurationArticleCreateRequest req = CurationArticleCreateRequest.builder()
                    .slug("summer-diet")
                    .title("여름 다이어트")
                    .contentMdx("# body")
                    .recipeIds(List.of(1L, 2L))
                    .build();

            CurationArticleCreateResult result = articleService.create(req);

            assertThat(result.id()).isEqualTo(100L);
            assertThat(result.slug()).isEqualTo("summer-diet");
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
