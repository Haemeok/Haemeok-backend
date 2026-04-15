package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeIngredient;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.hibernate.SessionFactory;
import org.hibernate.stat.Statistics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.MySQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@Testcontainers
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "spring.jpa.properties.hibernate.generate_statistics=true",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class RecipeQueryRepositoryImplTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private EntityManager em;

    @Autowired
    private RecipeRepository repo;

    @Autowired
    private JPAQueryFactory queryFactory;

    private Ingredient target;
    private Ingredient other;
    private User author;

    @BeforeEach
    void setUp() {
        author = User.builder()
                .provider("test")
                .oauthId("oauth-1")
                .nickname("tester")
                .role(Role.USER)
                .build();
        em.persist(author);

        target = Ingredient.builder().name("대파").category("채소").build();
        other = Ingredient.builder().name("양파").category("채소").build();
        em.persist(target);
        em.persist(other);
        em.flush();
    }

    @Test
    @DisplayName("findTopByIngredientId: popularityScore DESC, id DESC 정렬 & limit 적용")
    void findTopByIngredientId_sortsByPopularityScoreDescAndRespectsLimit() {
        // given: 같은 재료(target)를 사용하는 레시피 3개를 popularityScore를 달리해서 저장
        Recipe low = persistRecipe("low", 10L, RecipeImageStatus.READY);
        Recipe mid = persistRecipe("mid", 50L, RecipeImageStatus.READY);
        Recipe high = persistRecipe("high", 100L, RecipeImageStatus.READY);
        linkIngredient(low, target);
        linkIngredient(mid, target);
        linkIngredient(high, target);
        em.flush();
        em.clear();

        // when: limit=2 요청
        List<RecipeSimpleDto> result = repo.findTopByIngredientId(target.getId(), 2);

        // then: high, mid 순서로 2개만 반환
        assertThat(result).extracting(RecipeSimpleDto::getTitle)
                .containsExactly("high", "mid");
    }

    @Test
    @DisplayName("findTopByIngredientId: isPrivate=true 레시피는 제외")
    void findTopByIngredientId_excludesPrivateRecipes() {
        Recipe pub = persistRecipe("public", 10L, RecipeImageStatus.READY);
        Recipe priv = persistRecipe("private", 999L, RecipeImageStatus.READY);
        priv.updateIsPrivate(true);
        linkIngredient(pub, target);
        linkIngredient(priv, target);
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientId(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("public");
    }

    @Test
    @DisplayName("findTopByIngredientId: imageStatus=READY 또는 NULL만 포함, PENDING은 제외")
    void findTopByIngredientId_filtersByImageStatus() {
        Recipe ready = persistRecipe("ready", 30L, RecipeImageStatus.READY);
        Recipe nullStatus = persistRecipe("nullStatus", 20L, null);
        Recipe pending = persistRecipe("pending", 999L, RecipeImageStatus.PENDING);
        linkIngredient(ready, target);
        linkIngredient(nullStatus, target);
        linkIngredient(pending, target);
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientId(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle)
                .containsExactly("ready", "nullStatus");
    }

    @Test
    @DisplayName("findTopByIngredientId: 다른 재료만 사용하는 레시피는 제외")
    void findTopByIngredientId_filtersByIngredientId() {
        Recipe onTarget = persistRecipe("onTarget", 10L, RecipeImageStatus.READY);
        Recipe onOther = persistRecipe("onOther", 999L, RecipeImageStatus.READY);
        linkIngredient(onTarget, target);
        linkIngredient(onOther, other);
        em.flush();
        em.clear();

        List<RecipeSimpleDto> result = repo.findTopByIngredientId(target.getId(), 10);

        assertThat(result).extracting(RecipeSimpleDto::getTitle).containsExactly("onTarget");
    }

    @Test
    @DisplayName("findTopByIngredientId: count 쿼리 없이 단일 SELECT만 실행한다")
    void findTopByIngredientId_singleQueryNoCount() {
        Recipe r = persistRecipe("one", 10L, RecipeImageStatus.READY);
        linkIngredient(r, target);
        em.flush();
        em.clear();

        Statistics stats = em.getEntityManagerFactory()
                .unwrap(SessionFactory.class)
                .getStatistics();
        stats.clear();

        repo.findTopByIngredientId(target.getId(), 10);

        assertThat(stats.getPrepareStatementCount())
                .as("findTopByIngredientId는 SELECT 1회만 실행해야 한다 (count 쿼리 없음)")
                .isEqualTo(1L);
    }

    @Test
    @DisplayName("perf baseline: 동일 조건을 Page 방식으로 구현하면 SELECT+COUNT 2회가 실행된다")
    void findTopByIngredientId_perfBaseline_pageStyleEmitsTwoQueries() {
        Recipe r = persistRecipe("one", 10L, RecipeImageStatus.READY);
        linkIngredient(r, target);
        em.flush();
        em.clear();

        Statistics stats = em.getEntityManagerFactory()
                .unwrap(SessionFactory.class)
                .getStatistics();
        stats.clear();

        QRecipe recipe = QRecipe.recipe;
        QRecipeIngredient ri = QRecipeIngredient.recipeIngredient;

        queryFactory.select(recipe.id)
                .from(recipe)
                .join(recipe.ingredients, ri)
                .where(recipe.isPrivate.isFalse(),
                        recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()),
                        ri.ingredient.id.eq(target.getId()))
                .groupBy(recipe.id)
                .orderBy(recipe.popularityScore.desc(), recipe.id.desc())
                .offset(0)
                .limit(10)
                .fetch();

        queryFactory.select(recipe.countDistinct())
                .from(recipe)
                .join(recipe.ingredients, ri)
                .where(recipe.isPrivate.isFalse(),
                        recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()),
                        ri.ingredient.id.eq(target.getId()))
                .fetchOne();

        assertThat(stats.getPrepareStatementCount())
                .as("Page 방식(SELECT + COUNT)은 2회 SQL이 필요함 — 현재 구현(1회)이 50%% 적음")
                .isEqualTo(2L);
    }

    private Recipe persistRecipe(String title, Long popularityScore, RecipeImageStatus imageStatus) {
        Recipe recipe = Recipe.builder()
                .user(author)
                .title(title)
                .dishType(DishType.FRYING)
                .popularityScore(popularityScore)
                .imageStatus(imageStatus)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private void linkIngredient(Recipe recipe, Ingredient ingredient) {
        RecipeIngredient ri = RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .quantity("1")
                .unit("개")
                .build();
        em.persist(ri);
    }
}
