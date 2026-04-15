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
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.MySQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * findFirstOfficialByYoutubeUrl: 같은 youtubeUrl을 가진 복제본이 섞여 있어도
 * 반드시 공식 원본(officialUserId 소유 + originRecipe NULL)만 반환하는지를 고정한다.
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
class RecipeRepositoryYoutubeUrlTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private EntityManager em;

    @Autowired
    private RecipeRepository repo;

    private static final String URL = "https://www.youtube.com/watch?v=abc123";

    private User officialUser;
    private User requester;

    @BeforeEach
    void setUp() {
        officialUser = User.builder().provider("google").oauthId("official")
                .nickname("공식계정").role(Role.ADMIN).build();
        em.persist(officialUser);

        requester = User.builder().provider("google").oauthId("user-1")
                .nickname("일반유저").role(Role.USER).build();
        em.persist(requester);
        em.flush();
    }

    @Test
    @DisplayName("공식 원본만 있을 때 정상 조회된다")
    void returnsOfficialWhenOnlyOfficialExists() {
        Recipe official = persistRecipe(officialUser, URL, null, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();

        Optional<Recipe> found = repo.findFirstOfficialByYoutubeUrl(URL, officialUser.getId());

        assertThat(found).isPresent();
        assertThat(found.get().getId()).isEqualTo(official.getId());
    }

    @Test
    @DisplayName("같은 URL의 유저 복제본이 섞여 있어도 공식 원본을 반환한다")
    void ignoresUserCloneMixedIn() {
        Recipe official = persistRecipe(officialUser, URL, null, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(requester, URL, official, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        em.flush();

        Optional<Recipe> found = repo.findFirstOfficialByYoutubeUrl(URL, officialUser.getId());

        assertThat(found).isPresent();
        assertThat(found.get().getId()).isEqualTo(official.getId());
    }

    @Test
    @DisplayName("공식 원본이 없고 복제본만 있으면 빈 결과를 반환한다")
    void returnsEmptyWhenOnlyClonesExist() {
        Recipe official = persistRecipe(officialUser, "https://www.youtube.com/watch?v=other", null,
                RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe clone = persistRecipe(requester, URL, official, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        em.flush();

        Optional<Recipe> found = repo.findFirstOfficialByYoutubeUrl(URL, officialUser.getId());

        assertThat(found).isEmpty();
    }

    @Test
    @DisplayName("다른 유저가 소유한 YOUTUBE 레시피(복제본 아님)도 공식 원본으로 간주하지 않는다")
    void ignoresNonOfficialUserRecipe() {
        persistRecipe(requester, URL, null, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();

        Optional<Recipe> found = repo.findFirstOfficialByYoutubeUrl(URL, officialUser.getId());

        assertThat(found).isEmpty();
    }

    private Recipe persistRecipe(User user, String youtubeUrl, Recipe originRecipe,
                                 RecipeVisibility visibility, RecipeListingStatus listing) {
        Recipe r = Recipe.builder()
                .user(user)
                .title("t")
                .dishType(DishType.SOUP_STEW)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.YOUTUBE)
                .youtubeUrl(youtubeUrl)
                .isPrivate(visibility == RecipeVisibility.PRIVATE)
                .originRecipe(originRecipe)
                .build();
        em.persist(r);
        return r;
    }
}
