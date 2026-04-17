package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeBookItemRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.Role;
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

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * countAccessibleByUserIdGroupByBookId invariant: countAccessibleByBookIdAndUserId와 동일한
 * 접근성 필터(r.isPrivate = false OR r.user.id = :userId)를 모든 폴더에 대해 한 번에 집계해야 한다.
 * 폴더 목록 응답(/api/me/recipe-books)과 폴더 상세 응답(/api/me/recipe-books/{id})의 recipeCount
 * 의미가 어긋나지 않도록 회귀로 고정한다.
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
class RecipeBookItemAccessibleCountTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired
    private EntityManager em;

    @Autowired
    private RecipeBookItemRepository itemRepo;

    @Test
    @DisplayName("유저의 모든 폴더에서 접근 가능한 레시피만 book_id별로 집계한다")
    void groupsAccessibleItemsByBookId() {
        // given
        User owner = persistUser("google", "owner", "폴더주인");
        User stranger = persistUser("google", "stranger", "타인");

        RecipeBook defaultBook = persistBook(owner, "저장한 레시피", true, 0);
        RecipeBook customBook = persistBook(owner, "한식 모음", false, 1);
        RecipeBook emptyBook = persistBook(owner, "빈 폴더", false, 2);

        // 공개 레시피 (타인 작성): 접근 가능
        Recipe publicByStranger = persistRecipe(stranger, false);
        // 비공개 레시피 (타인 작성): 접근 불가
        Recipe privateByStranger = persistRecipe(stranger, true);
        // 비공개 레시피 (본인 작성): 본인이므로 접근 가능
        Recipe privateByOwner = persistRecipe(owner, true);

        // defaultBook: 공개 1 + 타인 비공개 1 + 본인 비공개 1 = 접근 가능 2
        persistItem(defaultBook, publicByStranger);
        persistItem(defaultBook, privateByStranger);
        persistItem(defaultBook, privateByOwner);

        // customBook: 타인 비공개 1 = 접근 가능 0
        persistItem(customBook, privateByStranger);

        em.flush();
        em.clear();

        // when
        List<Object[]> rows = itemRepo.countAccessibleByUserIdGroupByBookId(owner.getId());
        Map<Long, Long> countByBookId = rows.stream().collect(Collectors.toMap(
                row -> ((Number) row[0]).longValue(),
                row -> ((Number) row[1]).longValue()));

        // then
        assertThat(countByBookId.get(defaultBook.getId()))
                .as("공개 1 + 본인 비공개 1 = 2 (타인 비공개는 제외)")
                .isEqualTo(2L);
        assertThat(countByBookId.get(customBook.getId()))
                .as("타인 비공개만 담긴 폴더는 GROUP BY에서 0 row가 아니라 아예 결과에 안 나온다")
                .isNull();
        assertThat(countByBookId.get(emptyBook.getId()))
                .as("비어 있는 폴더도 결과에 안 나온다")
                .isNull();
    }

    @Test
    @DisplayName("타인 폴더는 집계 대상에서 제외된다")
    void excludesOtherUsersBooks() {
        User owner = persistUser("google", "owner2", "주인2");
        User otherOwner = persistUser("google", "other", "다른주인");

        RecipeBook ownerBook = persistBook(owner, "내 폴더", true, 0);
        RecipeBook otherBook = persistBook(otherOwner, "남의 폴더", true, 0);

        Recipe publicRecipe = persistRecipe(owner, false);
        persistItem(ownerBook, publicRecipe);
        persistItem(otherBook, publicRecipe);

        em.flush();
        em.clear();

        List<Object[]> rows = itemRepo.countAccessibleByUserIdGroupByBookId(owner.getId());

        assertThat(rows).hasSize(1);
        assertThat(((Number) rows.get(0)[0]).longValue()).isEqualTo(ownerBook.getId());
        assertThat(((Number) rows.get(0)[1]).longValue()).isEqualTo(1L);
    }

    private User persistUser(String provider, String oauthId, String nickname) {
        User u = User.builder().provider(provider).oauthId(oauthId)
                .nickname(nickname).role(Role.USER).build();
        em.persist(u);
        return u;
    }

    private RecipeBook persistBook(User user, String name, boolean isDefault, int displayOrder) {
        RecipeBook b = RecipeBook.builder()
                .user(user).name(name).isDefault(isDefault).displayOrder(displayOrder).build();
        em.persist(b);
        return b;
    }

    private Recipe persistRecipe(User user, boolean isPrivate) {
        Recipe r = Recipe.builder()
                .user(user)
                .title("t")
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

    private RecipeBookItem persistItem(RecipeBook book, Recipe recipe) {
        RecipeBookItem item = RecipeBookItem.builder().book(book).recipe(recipe).build();
        em.persist(item);
        return item;
    }
}
