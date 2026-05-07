package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.MySQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 타인 프로필 사용자 레시피 목록 ({@code findByUserIdAndIsPrivateFalse})의 V1.x discovery 정책 회귀.
 *
 * <p>이전엔 derived query라 isPrivate=false만 검사 — PUBLIC+UNLISTED(link-only) 리믹스가 타인 프로필에 노출되는 누수.
 * PR 3에서 명시적 {@code @Query}로 4-enum 필터를 추가했고, 이 테스트가 PUBLIC+UNLISTED 제외를 잠근다.
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
class RecipeRepositoryUserListDiscoveryTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired private EntityManager em;
    @Autowired private RecipeRepository repo;

    private User author;

    @BeforeEach
    void setUp() {
        author = User.builder().provider("google").oauthId("author")
                .nickname("작성자").role(Role.USER).build();
        em.persist(author);
        em.flush();
    }

    @Test
    @DisplayName("findByUserIdAndIsPrivateFalse: 타인 프로필 정책 — PUBLIC+UNLISTED(link-only) 리믹스는 제외 (discovery 단일 정책)")
    void findByUserIdAndIsPrivateFalse_excludesPublicUnlistedRemixes() {
        // 통과해야 하는 row: ACTIVE + PUBLIC + LISTED + isPrivate=false
        persistRecipe("public-listed", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, false);

        // 누수 회귀의 핵심: PUBLIC+UNLISTED(link-only)는 isPrivate=false라도 타인 프로필 목록에서 빠져야 한다.
        // derived query 시절엔 이 row가 노출되던 누수.
        persistRecipe("public-unlisted-remix", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED, false);

        // 다른 차원 제외 케이스
        persistRecipe("private",   RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED, true);
        persistRecipe("hidden",    RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED,   false);
        persistRecipe("deleted",   RecipeLifecycleStatus.DELETED, RecipeVisibility.PUBLIC,    RecipeListingStatus.LISTED,   false);
        em.flush();
        em.clear();

        Page<Recipe> page = repo.findByUserIdAndIsPrivateFalse(author.getId(), PageRequest.of(0, 50));

        assertThat(page.getContent())
                .extracting(Recipe::getTitle)
                .containsExactly("public-listed");
        assertThat(page.getTotalElements()).isEqualTo(1L);
    }

    private Recipe persistRecipe(String title,
                                  RecipeLifecycleStatus lifecycle,
                                  RecipeVisibility visibility,
                                  RecipeListingStatus listing,
                                  boolean isPrivate) {
        Recipe recipe = Recipe.builder()
                .user(author)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .isPrivate(isPrivate)
                .build();
        em.persist(recipe);
        return recipe;
    }
}
