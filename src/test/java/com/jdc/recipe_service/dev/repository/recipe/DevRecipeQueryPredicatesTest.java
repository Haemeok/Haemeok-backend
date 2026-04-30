package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeQueryPredicates의 SQL 결과 검증.
 *
 * A2/A3 dev repository fallback 경로의 RESTRICTED 누수 차단이 핵심 invariant.
 * QueryDSL 표현이 {@link com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy} 의미와
 * 실제 SQL 결과로 일치하는지를 H2로 고정한다.
 *
 * 프로젝트 testing rule(`@DataJpaTest` + H2 기본). MySQL 전용 기능을 안 쓰는 enum filter라 H2로 충분.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevRecipeQueryPredicatesTest {

    @Autowired EntityManager em;
    @Autowired JPAQueryFactory queryFactory;

    private User owner;
    private User other;

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        other = persistUser("other-oauth", "other");
        em.flush();
    }

    @Test
    @DisplayName("publicListedActive: ACTIVE+PUBLIC+LISTED만 SELECT — RESTRICTED/PRIVATE/non-ACTIVE 누수 없음")
    void publicListedActive_returnsOnlyTriple() {
        persistRecipe(owner, "pub",        RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv",       RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "hidden",     RecipeLifecycleStatus.HIDDEN,  RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        persistRecipe(owner, "deleted",    RecipeLifecycleStatus.DELETED, RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        QRecipe r = QRecipe.recipe;
        List<String> titles = queryFactory.select(r.title)
                .from(r)
                .where(DevRecipeQueryPredicates.publicListedActive(r))
                .fetch();

        assertThat(titles).containsExactly("pub");
    }

    @Test
    @DisplayName("accessibleBy(other): non-owner는 RESTRICTED/PRIVATE 절대 못 봄 (RESTRICTED 누수 차단의 핵심)")
    void accessibleBy_nonOwner_excludesRestrictedAndPrivate() {
        persistRecipe(owner, "pub",        RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv",       RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush();
        em.clear();

        QRecipe r = QRecipe.recipe;
        List<String> titles = queryFactory.select(r.title)
                .from(r)
                .where(DevRecipeQueryPredicates.accessibleBy(r, other.getId()))
                .fetch();

        assertThat(titles).containsExactly("pub");
    }

    @Test
    @DisplayName("accessibleBy(owner): owner는 ACTIVE PRIVATE/RESTRICTED 자신의 레시피 모두 SELECT — non-ACTIVE는 owner도 차단")
    void accessibleBy_owner_seesPrivateRestricted_butNotNonActive() {
        persistRecipe(owner, "pub",        RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv",       RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE,  RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "hidden",     RecipeLifecycleStatus.HIDDEN,  RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "banned",     RecipeLifecycleStatus.BANNED,  RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        QRecipe r = QRecipe.recipe;
        List<String> titles = queryFactory.select(r.title)
                .from(r)
                .where(DevRecipeQueryPredicates.accessibleBy(r, owner.getId()))
                .fetch();

        // ACTIVE 3개만 — owner라도 HIDDEN/BANNED는 admin 우회 방지로 거부
        assertThat(titles).containsExactlyInAnyOrder("pub", "priv", "restricted");
    }

    @Test
    @DisplayName("accessibleBy(null): anonymous는 publicListedActive와 동일 SELECT")
    void accessibleBy_anonymous_matchesPublicListedActive() {
        persistRecipe(owner, "pub",        RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv",       RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush();
        em.clear();

        QRecipe r = QRecipe.recipe;
        List<String> titles = queryFactory.select(r.title)
                .from(r)
                .where(DevRecipeQueryPredicates.accessibleBy(r, null))
                .fetch();

        assertThat(titles).containsExactly("pub");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder()
                .provider("test")
                .oauthId(oauthId)
                .nickname(nickname)
                .role(Role.USER)
                .build();
        em.persist(user);
        return user;
    }

    private Recipe persistRecipe(User user, String title,
                                  RecipeLifecycleStatus lifecycle,
                                  RecipeVisibility visibility,
                                  RecipeListingStatus listing) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .build();
        em.persist(recipe);
        return recipe;
    }
}
