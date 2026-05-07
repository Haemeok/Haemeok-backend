package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
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
 * H6 invariant: countRemixesByOriginRecipeId와 findRemixesByOriginRecipeId의 WHERE 절은
 * 반드시 같은 row 집합을 산출해야 한다. 둘 중 하나만 수정되어 조건이 어긋나면
 * status DTO의 remixCount와 /remixes 리스트 크기가 실시간으로 불일치하게 된다.
 *
 * 이 테스트는 각 필터 차원(visibility/listing/lifecycle/isPrivate)을 섞은 fixture에 대해
 * count == totalElements 를 고정해서 사일런트 드리프트를 회귀로 잡는다.
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
class RecipeRepositoryRemixCountTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private EntityManager em;

    @Autowired
    private RecipeRepository repo;

    private User officialUser;
    private User remixer;
    private Recipe origin;

    @BeforeEach
    void setUp() {
        officialUser = User.builder().provider("google").oauthId("official")
                .nickname("공식계정").role(Role.ADMIN).build();
        em.persist(officialUser);

        remixer = User.builder().provider("google").oauthId("remixer")
                .nickname("리믹서").role(Role.USER).build();
        em.persist(remixer);

        origin = persistRecipe(officialUser, null,
                RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeLifecycleStatus.ACTIVE, false);
        em.flush();
    }

    @Test
    @DisplayName("H6: count와 find의 WHERE 절이 동일한 집합을 산출한다 — V1.x 정책 (PUBLIC+UNLISTED link-only도 포함)")
    void countRemixes_matchesFindRemixesWhereClause() {
        // given: 모든 필터를 통과하는 row (ACTIVE + PUBLIC + isPrivate=false)
        // V1.x 정책: 리믹스는 listingStatus 무관 — LISTED/UNLISTED 모두 통과 (link-only도 원본의 리믹스 목록에 포함)
        persistRemix(RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,   RecipeLifecycleStatus.ACTIVE, false);
        persistRemix(RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,   RecipeLifecycleStatus.ACTIVE, false);
        persistRemix(RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,   RecipeLifecycleStatus.ACTIVE, false);
        persistRemix(RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED, RecipeLifecycleStatus.ACTIVE, false);

        // 각 필터 차원별로 제외되어야 하는 케이스 (PRIVATE / RESTRICTED / non-ACTIVE / isPrivate=true)
        persistRemix(RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED, RecipeLifecycleStatus.ACTIVE,  true);
        persistRemix(RecipeVisibility.RESTRICTED, RecipeListingStatus.LISTED,   RecipeLifecycleStatus.ACTIVE,  false);
        persistRemix(RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED,   RecipeLifecycleStatus.HIDDEN,  false);
        persistRemix(RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED,   RecipeLifecycleStatus.DELETED, false);
        persistRemix(RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED,   RecipeLifecycleStatus.ACTIVE,  true);
        em.flush();
        em.clear();

        // when
        long count = repo.countRemixesByOriginRecipeId(origin.getId());
        Page<RecipeSimpleDto> page = repo.findRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 50));

        // then
        assertThat(count)
                .as("countRemixes와 findRemixes는 동일한 WHERE 조건을 써야 한다 (H6 invariant)")
                .isEqualTo(page.getTotalElements())
                .isEqualTo(4L);
        assertThat(page.getContent())
                .as("필터를 통과한 4건 (PUBLIC+LISTED 3 + PUBLIC+UNLISTED 1)")
                .hasSize(4);
    }

    @Test
    @DisplayName("원본에 매칭되는 리믹스가 없으면 count는 0이다")
    void countRemixes_noMatches_returnsZero() {
        em.flush();
        em.clear();

        long count = repo.countRemixesByOriginRecipeId(origin.getId());
        Page<RecipeSimpleDto> page = repo.findRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(count).isZero();
        assertThat(page.getTotalElements()).isZero();
    }

    private Recipe persistRemix(RecipeVisibility visibility, RecipeListingStatus listing,
                                RecipeLifecycleStatus lifecycle, boolean isPrivate) {
        return persistRecipe(remixer, origin, visibility, listing, lifecycle, isPrivate);
    }

    private Recipe persistRecipe(User user, Recipe originRecipe,
                                 RecipeVisibility visibility, RecipeListingStatus listing,
                                 RecipeLifecycleStatus lifecycle, boolean isPrivate) {
        Recipe r = Recipe.builder()
                .user(user)
                .title("t")
                .dishType(DishType.SOUP_STEW)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.YOUTUBE)
                .isPrivate(isPrivate)
                .originRecipe(originRecipe)
                .build();
        em.persist(r);
        return r;
    }
}
