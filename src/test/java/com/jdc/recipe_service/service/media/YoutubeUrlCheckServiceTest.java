package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
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
import org.springframework.test.context.TestPropertySource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * findFirstStrictPublicYoutubeRecipe SQL strict 필터 회귀 테스트 (H2).
 *
 * 핵심 invariants (모두 SQL push-down):
 *  - 같은 URL에 older PRIVATE row + later PUBLIC row → PUBLIC row 매칭 (Java first-row 필터의 회귀 차단)
 *  - imageStatus=PENDING/FAILED 차단 (imageKey만 보면 부족)
 *  - source != YOUTUBE / non-official user / originRecipe non-null / non-ACTIVE / non-PUBLIC / non-LISTED 모두 차단
 *
 * service 레이어({@link YoutubeUrlCheckService})는 이 repo method를 그대로 위임 + URL canonicalize만 추가하므로
 * 필터 회귀를 잡으려면 repo 레벨에서 SQL이 진짜 필터링하는지 보는 게 정확하다.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop"
})
class YoutubeUrlCheckServiceTest {

    private static final String WATCH_URL = "https://www.youtube.com/watch?v=abc123";

    @Autowired EntityManager em;
    @Autowired RecipeRepository recipeRepository;

    private User officialUser;
    private User otherUser;
    private Long officialUserId;

    @BeforeEach
    void setUp() {
        officialUser = persistUser("official-oauth", "official");
        otherUser = persistUser("other-oauth", "other");
        em.flush();
        officialUserId = officialUser.getId();
    }

    @Test
    @DisplayName("strict 모두 통과 → recipe 반환")
    void allConditionsPass_returnsRecipe() {
        Recipe r = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        Optional<Recipe> result = recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst();

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(r.getId());
    }

    @Test
    @DisplayName("imageStatus=PENDING → empty (imageKey 있어도 차단)")
    void imagePending_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.PENDING, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("imageStatus=FAILED → empty")
    void imageFailed_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.FAILED, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("PRIVATE 차단")
    void privateVisibility_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("RESTRICTED 차단")
    void restrictedVisibility_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("HIDDEN(non-ACTIVE) 차단")
    void nonActive_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("UNLISTED 차단")
    void unlisted_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("source=USER (non-YOUTUBE) 차단")
    void nonYoutubeSource_returnsEmpty() {
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.USER, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("non-official user 차단")
    void nonOfficialUser_returnsEmpty() {
        persistYoutubeRecipe(otherUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("originRecipe 비-null (remix) 차단 — origin 없는 상태에서 remix만으로는 매칭 안 됨")
    void remixOnly_blockedByOriginRecipeNull() {
        // origin은 다른 url
        Recipe origin = persistYoutubeRecipe(officialUser, "https://www.youtube.com/watch?v=other",
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush();
        // 검사 대상 URL은 remix만 가짐
        persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, origin);
        em.flush(); em.clear();

        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("핵심 회귀: 같은 URL에 older PRIVATE + later PUBLIC → PUBLIC row 반환 (Java first-row 회귀 차단)")
    void multipleRowsOlderPrivateLaterPublic_returnsPublic() {
        // older PRIVATE
        Recipe older = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush();
        // later PUBLIC
        Recipe later = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        // 기존 버그(findFirstOfficial → first row만): older PRIVATE 잡혀서 Java filter로 탈락 → null
        // 수정: SQL filter가 PRIVATE 제외 → later PUBLIC 만 매칭
        Optional<Recipe> result = recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst();

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(later.getId());
        assertThat(result.get().getId()).isNotEqualTo(older.getId());
    }

    @Test
    @DisplayName("URL 매칭 자체 없음 → empty")
    void noMatch_returnsEmpty() {
        em.flush(); em.clear();
        assertThat(recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst()).isEmpty();
    }

    @Test
    @DisplayName("imageStatus=NULL legacy row → 매칭 (dev imageReady 컨벤션: READY OR NULL)")
    void imageStatusNull_returnsRecipe() {
        Recipe r = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                null /* imageStatus null */, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        Optional<Recipe> result = recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1)).stream().findFirst();

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(r.getId());
    }

    @Test
    @DisplayName("LIMIT 1 강제: strict 통과 row가 2개 이상이어도 가장 작은 id 하나만 반환 (IncorrectResultSizeDataAccessException 회피)")
    void multipleStrictPublicRows_returnsFirstByIdAsc() {
        Recipe first = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush();
        Recipe second = persistYoutubeRecipe(officialUser, WATCH_URL,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, RecipeSourceType.YOUTUBE, null);
        em.flush(); em.clear();

        // PageRequest.of(0, 1)로 LIMIT 1 강제 → 2개 이상이어도 예외 없이 1개만 반환
        var rows = recipeRepository.findStrictPublicYoutubeRecipes(WATCH_URL, officialUserId, PageRequest.of(0, 1));

        assertThat(rows).hasSize(1);
        assertThat(rows.get(0).getId()).isEqualTo(first.getId()); // ORDER BY id ASC → 작은 id
        assertThat(rows.get(0).getId()).isLessThan(second.getId());
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

    private Recipe persistYoutubeRecipe(User user, String youtubeUrl,
                                         RecipeLifecycleStatus lifecycle,
                                         RecipeVisibility visibility,
                                         RecipeListingStatus listing,
                                         RecipeImageStatus imageStatus,
                                         RecipeSourceType source,
                                         Recipe originRecipe) {
        Recipe.RecipeBuilder b = Recipe.builder()
                .user(user)
                .title("test-" + System.nanoTime())
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(source)
                .youtubeUrl(youtubeUrl)
                .imageStatus(imageStatus)
                .imageKey("fake-key")
                .isPrivate(visibility == RecipeVisibility.PRIVATE);
        if (originRecipe != null) b.originRecipe(originRecipe);
        Recipe recipe = b.build();
        em.persist(recipe);
        return recipe;
    }
}
