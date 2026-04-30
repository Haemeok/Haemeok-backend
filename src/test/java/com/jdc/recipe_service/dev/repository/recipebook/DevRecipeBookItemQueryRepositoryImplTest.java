package com.jdc.recipe_service.dev.repository.recipebook;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
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
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDateTime;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeBookItemQueryRepositoryImpl 검증 (H2).
 *
 *  1. 본인 ACTIVE PRIVATE/RESTRICTED → 노출 (owner 분기)
 *  2. 다른 사람 RESTRICTED/PRIVATE → 차단 (운영보다 엄격)
 *  3. non-ACTIVE는 본인/타인 무관 차단
 *  4. imageReady (PENDING/FAILED 차단)
 *  5. count = content size 일관 + group count 정확
 *  6. default 정렬: item.createdAt DESC (폴더 추가 시각)
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevRecipeBookItemQueryRepositoryImpl.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevRecipeBookItemQueryRepositoryImplTest {

    @Autowired EntityManager em;
    @Autowired DevRecipeBookItemQueryRepository repo;

    private User owner;
    private User other;
    private RecipeBook book;

    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        other = persistUser("other-oauth", "other");
        book = persistBook(owner, "my-book");
        em.flush();
    }

    @Test
    @DisplayName("본인 ACTIVE PRIVATE/RESTRICTED → 노출 + 다른 사람 RESTRICTED/PRIVATE → 차단 (dev 정책)")
    void accessibleByPolicy_ownVsOthers() {
        Recipe ownPub = persistRecipe(owner, "own-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe ownPriv = persistRecipe(owner, "own-priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        Recipe ownRestricted = persistRecipe(owner, "own-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        Recipe othersPub = persistRecipe(other, "others-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe othersPriv = persistRecipe(other, "others-priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        Recipe othersRestricted = persistRecipe(other, "others-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);

        for (Recipe r : new Recipe[]{ownPub, ownPriv, ownRestricted, othersPub, othersPriv, othersRestricted}) {
            persistItem(book, r);
        }
        em.flush(); em.clear();

        Slice<RecipeBookItem> result = repo.findAccessibleDevByBookIdAndUserId(book.getId(), owner.getId(), PAGE_10);

        // 본인 거 3개 + 다른 사람 PUBLIC 1개 = 4개. 다른 사람 PRIVATE/RESTRICTED는 차단.
        assertThat(result.getContent()).extracting(it -> it.getRecipe().getTitle())
                .containsExactlyInAnyOrder("own-pub", "own-priv", "own-restricted", "others-pub");

        int count = repo.countAccessibleDevByBookIdAndUserId(book.getId(), owner.getId());
        assertThat(count).as("count = content size 일관성").isEqualTo(4);
    }

    @Test
    @DisplayName("non-ACTIVE는 본인/타인 무관 차단 (HIDDEN/BANNED — admin 우회 방지)")
    void nonActive_blocked() {
        Recipe ownHidden = persistRecipe(owner, "own-hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe othersBanned = persistRecipe(other, "others-banned", RecipeLifecycleStatus.BANNED, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe ok = persistRecipe(other, "ok", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        for (Recipe r : new Recipe[]{ownHidden, othersBanned, ok}) {
            persistItem(book, r);
        }
        em.flush(); em.clear();

        Slice<RecipeBookItem> result = repo.findAccessibleDevByBookIdAndUserId(book.getId(), owner.getId(), PAGE_10);

        assertThat(result.getContent()).extracting(it -> it.getRecipe().getTitle()).containsExactly("ok");
    }

    @Test
    @DisplayName("imageReady: PENDING/FAILED 차단 (READY/null만)")
    void imageNotReady_blocked() {
        Recipe ready = persistRecipeWithImage(owner, "ready", RecipeImageStatus.READY);
        Recipe nullImage = persistRecipeWithImage(owner, "nullImage", null);
        Recipe pending = persistRecipeWithImage(owner, "pending", RecipeImageStatus.PENDING);
        Recipe failed = persistRecipeWithImage(owner, "failed", RecipeImageStatus.FAILED);
        for (Recipe r : new Recipe[]{ready, nullImage, pending, failed}) {
            persistItem(book, r);
        }
        em.flush(); em.clear();

        Slice<RecipeBookItem> result = repo.findAccessibleDevByBookIdAndUserId(book.getId(), owner.getId(), PAGE_10);

        assertThat(result.getContent()).extracting(it -> it.getRecipe().getTitle())
                .containsExactlyInAnyOrder("ready", "nullImage");
    }

    @Test
    @DisplayName("repo ownership 가드: 다른 사람 bookId가 들어와도 빈 결과 + count 0 (footgun 방지 — service ownership check 잊혀도 누수 0)")
    void otherUsersBookId_returnsEmptyAndZeroCount() {
        // other가 만든 book에 PUBLIC 레시피 추가
        RecipeBook othersBook = persistBook(other, "others-book");
        Recipe pub = persistRecipe(other, "others-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistItem(othersBook, pub);
        em.flush(); em.clear();

        // owner가 othersBook.id로 직접 호출 — repo ownership 가드가 막음 (service check 거치기 전)
        Slice<RecipeBookItem> result = repo.findAccessibleDevByBookIdAndUserId(othersBook.getId(), owner.getId(), PAGE_10);
        int count = repo.countAccessibleDevByBookIdAndUserId(othersBook.getId(), owner.getId());

        assertThat(result.getContent()).as("다른 사람 bookId → empty").isEmpty();
        assertThat(count).as("다른 사람 bookId → count 0").isZero();
    }

    @Test
    @DisplayName("group count: 사용자의 모든 book에 대해 dev 정책 통과 아이템 수 정확히 집계")
    void groupCount_perBook() {
        RecipeBook book2 = persistBook(owner, "second-book");
        em.flush();

        Recipe r1 = persistRecipe(owner, "r1", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe r2 = persistRecipe(other, "r2-others-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        Recipe r3 = persistRecipe(owner, "r3-own-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        // book1: r1 + r2(others restricted, 차단) → 정책 통과 1
        persistItem(book, r1);
        persistItem(book, r2);
        // book2: r3(own restricted, 통과) → 정책 통과 1
        persistItem(book2, r3);
        em.flush(); em.clear();

        Map<Long, Integer> counts = repo.countAccessibleDevByUserIdGroupByBookId(owner.getId());

        assertThat(counts).containsEntry(book.getId(), 1);
        assertThat(counts).containsEntry(book2.getId(), 1);
    }

    @Test
    @DisplayName("default 정렬: item.createdAt DESC (폴더 추가 시각, 레시피 생성일과 무관)")
    void defaultSort_byItemCreatedAtDesc() {
        // recipe 생성 순서와 item 추가 순서를 반대로
        Recipe oldRecipe = persistRecipe(owner, "old-recipe", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe newRecipe = persistRecipe(owner, "new-recipe", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        RecipeBookItem earlierItem = persistItemAndReturn(book, newRecipe);  // newRecipe 먼저 폴더에 추가
        RecipeBookItem laterItem = persistItemAndReturn(book, oldRecipe);    // oldRecipe 나중에 폴더에 추가
        em.flush();

        // recipe.createdAt 강제: oldRecipe가 옛날, newRecipe가 최근
        forceCreatedAt("Recipe", oldRecipe.getId(), LocalDateTime.of(2026, 1, 1, 0, 0));
        forceCreatedAt("Recipe", newRecipe.getId(), LocalDateTime.of(2026, 5, 1, 0, 0));
        // item.createdAt 강제: laterItem(oldRecipe 추가 시각)이 더 최신
        forceCreatedAt("RecipeBookItem", earlierItem.getId(), LocalDateTime.of(2026, 6, 1, 0, 0));
        forceCreatedAt("RecipeBookItem", laterItem.getId(), LocalDateTime.of(2026, 6, 2, 0, 0));
        em.clear();

        Slice<RecipeBookItem> result = repo.findAccessibleDevByBookIdAndUserId(book.getId(), owner.getId(), PAGE_10);

        // item.createdAt DESC: 나중에 폴더에 추가한 oldRecipe가 먼저.
        // (recipe.createdAt 기준이면 newRecipe가 먼저였을 것 — 회귀 시 순서 반대로 깨짐)
        assertThat(result.getContent()).extracting(it -> it.getRecipe().getTitle())
                .as("default sort=createdAt → item.createdAt DESC, NOT recipe.createdAt")
                .containsExactly("old-recipe", "new-recipe");
    }

    // ---------- fixtures ----------

    private User persistUser(String oauthId, String nickname) {
        User user = User.builder().provider("test").oauthId(oauthId).nickname(nickname).role(Role.USER).build();
        em.persist(user);
        return user;
    }

    private RecipeBook persistBook(User user, String name) {
        RecipeBook book = RecipeBook.builder().user(user).name(name).isDefault(false).displayOrder(0).build();
        em.persist(book);
        return book;
    }

    private Recipe persistRecipe(User user, String title,
                                  RecipeLifecycleStatus lifecycle, RecipeVisibility visibility, RecipeListingStatus listing) {
        return persistRecipeWithImageAndOwner(user, title, lifecycle, visibility, listing, RecipeImageStatus.READY);
    }

    private Recipe persistRecipeWithImage(User user, String title, RecipeImageStatus imageStatus) {
        return persistRecipeWithImageAndOwner(user, title, RecipeLifecycleStatus.ACTIVE,
                RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, imageStatus);
    }

    private Recipe persistRecipeWithImageAndOwner(User user, String title,
                                                   RecipeLifecycleStatus lifecycle, RecipeVisibility visibility,
                                                   RecipeListingStatus listing, RecipeImageStatus imageStatus) {
        Recipe recipe = Recipe.builder()
                .user(user).title(title).dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle).visibility(visibility).listingStatus(listing)
                .source(RecipeSourceType.USER).imageStatus(imageStatus)
                .build();
        em.persist(recipe);
        return recipe;
    }

    private void persistItem(RecipeBook book, Recipe recipe) {
        persistItemAndReturn(book, recipe);
    }

    private RecipeBookItem persistItemAndReturn(RecipeBook book, Recipe recipe) {
        RecipeBookItem item = RecipeBookItem.builder().book(book).recipe(recipe).build();
        em.persist(item);
        return item;
    }

    private void forceCreatedAt(String entityName, Long id, LocalDateTime ts) {
        em.createQuery("UPDATE " + entityName + " e SET e.createdAt = :ts WHERE e.id = :id")
                .setParameter("ts", ts).setParameter("id", id).executeUpdate();
    }
}
