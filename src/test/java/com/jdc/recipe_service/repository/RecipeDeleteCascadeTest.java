package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.entity.recipe.RecipeAccess;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.type.recipe.RecipeAccessRole;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import jakarta.persistence.EntityManager;
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

import static org.assertj.core.api.Assertions.assertThat;

/**
 * RecipeService.deleteRecipe는 recipeRepository.deleteByIdDirectly(JPQL bulk delete)에 의존하므로
 * JPA cascade를 우회하고 오직 DB FK CASCADE에만 기댄다. recipe_youtube_info와 recipe_access는
 * 과거 NO ACTION FK로 남아 있어 자식 row가 있으면 FK violation으로 삭제가 실패했다.
 *
 * V20260417_001 마이그레이션이 두 FK를 ON DELETE CASCADE로 통일했고, 두 entity의 @OnDelete 선언이
 * 로컬/테스트 환경에서 동일한 schema를 보장한다. 이 테스트는 그 회귀를 고정한다.
 */
@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@Testcontainers
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class RecipeDeleteCascadeTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private EntityManager em;

    @Autowired
    private RecipeRepository recipeRepository;

    @Test
    @DisplayName("recipe_youtube_info row가 있어도 deleteByIdDirectly는 FK violation 없이 성공한다")
    void deletesRecipeWithYoutubeInfoRow() {
        // given
        User author = persistUser("yt_owner");
        Recipe recipe = persistRecipe(author, false);
        persistYoutubeInfo(recipe, "video-abc-123");
        em.flush();
        em.clear();

        // when
        recipeRepository.deleteByIdDirectly(recipe.getId());
        em.flush();
        em.clear();

        // then
        assertThat(countRecipes(recipe.getId())).isZero();
        assertThat(countYoutubeInfo(recipe.getId()))
                .as("FK CASCADE로 자식 recipe_youtube_info row도 함께 삭제돼야 한다")
                .isZero();
    }

    @Test
    @DisplayName("recipe_access row가 있어도 deleteByIdDirectly는 FK violation 없이 성공한다")
    void deletesRecipeWithAccessRow() {
        // given
        User owner = persistUser("acc_owner");
        User collaborator = persistUser("acc_viewer");
        Recipe recipe = persistRecipe(owner, true);
        persistRecipeAccess(recipe, collaborator, RecipeAccessRole.VIEWER);
        em.flush();
        em.clear();

        // when
        recipeRepository.deleteByIdDirectly(recipe.getId());
        em.flush();
        em.clear();

        // then
        assertThat(countRecipes(recipe.getId())).isZero();
        assertThat(countRecipeAccess(recipe.getId()))
                .as("FK CASCADE로 자식 recipe_access row도 함께 삭제돼야 한다")
                .isZero();
    }

    @Test
    @DisplayName("youtube_info와 recipe_access를 동시에 가진 레시피도 한 번의 delete로 정리된다")
    void deletesRecipeWithBothChildRows() {
        // given
        User owner = persistUser("combo_owner");
        User viewer = persistUser("combo_viewer");
        Recipe recipe = persistRecipe(owner, true);
        persistYoutubeInfo(recipe, "video-combo-456");
        persistRecipeAccess(recipe, viewer, RecipeAccessRole.VIEWER);
        em.flush();
        em.clear();

        // when
        recipeRepository.deleteByIdDirectly(recipe.getId());
        em.flush();
        em.clear();

        // then
        assertThat(countRecipes(recipe.getId())).isZero();
        assertThat(countYoutubeInfo(recipe.getId())).isZero();
        assertThat(countRecipeAccess(recipe.getId())).isZero();
    }

    private long countRecipes(Long recipeId) {
        return (long) em.createQuery(
                        "SELECT COUNT(r) FROM Recipe r WHERE r.id = :id", Long.class)
                .setParameter("id", recipeId)
                .getSingleResult();
    }

    private long countYoutubeInfo(Long recipeId) {
        return (long) em.createQuery(
                        "SELECT COUNT(y) FROM RecipeYoutubeInfo y WHERE y.recipe.id = :id", Long.class)
                .setParameter("id", recipeId)
                .getSingleResult();
    }

    private long countRecipeAccess(Long recipeId) {
        return (long) em.createQuery(
                        "SELECT COUNT(a) FROM RecipeAccess a WHERE a.recipe.id = :id", Long.class)
                .setParameter("id", recipeId)
                .getSingleResult();
    }

    private User persistUser(String oauthId) {
        User u = User.builder()
                .provider("google")
                .oauthId(oauthId)
                .nickname(oauthId)
                .role(Role.USER)
                .build();
        em.persist(u);
        return u;
    }

    private Recipe persistRecipe(User user, boolean isPrivate) {
        Recipe r = Recipe.builder()
                .user(user)
                .title("삭제 회귀 테스트용")
                .dishType(DishType.SOUP_STEW)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.YOUTUBE)
                .isPrivate(isPrivate)
                .build();
        em.persist(r);
        return r;
    }

    private RecipeYoutubeInfo persistYoutubeInfo(Recipe recipe, String videoId) {
        RecipeYoutubeInfo info = RecipeYoutubeInfo.builder()
                .recipe(recipe)
                .videoId(videoId)
                .build();
        em.persist(info);
        return info;
    }

    private RecipeAccess persistRecipeAccess(Recipe recipe, User user, RecipeAccessRole role) {
        RecipeAccess access = RecipeAccess.builder()
                .recipe(recipe)
                .user(user)
                .role(role)
                .build();
        em.persist(access);
        return access;
    }
}
