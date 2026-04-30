package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.entity.Recipe;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.TestPropertySource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeQueryRepositoryImplV2의 SQL 결과 검증 (H2).
 *
 * 검증:
 *  1. searchStatic + anonymous: ACTIVE+PUBLIC+LISTED만, 4 enum이 응답에 정확히 매핑
 *  2. searchStatic + non-owner: RESTRICTED/PRIVATE 누수 없음 + totalElements 정확
 *  3. searchStatic + owner: 자신의 PRIVATE/RESTRICTED 포함, 다른 owner의 RESTRICTED 제외
 *  4. searchStatic + non-ACTIVE: HIDDEN/BANNED/DELETED는 owner도 차단
 *  5. count query imageReadyCondition 일관성 (운영 V2 count 버그를 dev에서 수정한 것 검증)
 *  6. findAllByIds + stale ids: OpenSearch가 잘못 hit한 RESTRICTED를 DB가 차단 (MUST 수정의 핵심)
 *
 * @DataJpaTest + H2 — DevRecipeQueryRepositoryImplV2도 @Import.
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class, DevRecipeQueryRepositoryImplV2.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class DevRecipeQueryRepositoryImplV2Test {

    @Autowired EntityManager em;
    @Autowired DevRecipeQueryRepositoryV2 repo;

    private User owner;
    private User other;

    @BeforeEach
    void setUp() {
        owner = persistUser("owner-oauth", "owner");
        other = persistUser("other-oauth", "other");
        em.flush();
    }

    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    // ---------- searchStatic: 정책 적용 + 4 enum 매핑 ----------

    @Test
    @DisplayName("searchStatic anonymous: ACTIVE+PUBLIC+LISTED만 SELECT + 응답에 4 enum 정확히 매핑")
    void searchStatic_anonymous_publicListedActiveOnly_withDevFields() {
        persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.searchStatic(new RecipeSearchCondition(), PAGE_10, null);

        assertThat(result.getContent()).hasSize(1);
        assertThat(result.getTotalElements()).isEqualTo(1);

        DevRecipeSimpleStaticDto dto = result.getContent().get(0);
        assertThat(dto.getTitle()).isEqualTo("pub");
        // 4 enum이 응답에 String name으로 매핑됨 — 프론트에서 RESTRICTED 활성화 후 UI 분기에 사용
        assertThat(dto.getVisibility()).isEqualTo("PUBLIC");
        assertThat(dto.getListingStatus()).isEqualTo("LISTED");
        assertThat(dto.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(dto.getSource()).isEqualTo("USER");
    }

    @Test
    @DisplayName("searchStatic non-owner: RESTRICTED/PRIVATE 절대 안 보임 + totalElements 정확 (1)")
    void searchStatic_nonOwner_excludesRestrictedAndPrivate_totalAccurate() {
        persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.searchStatic(new RecipeSearchCondition(), PAGE_10, other.getId());

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("pub");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("searchStatic owner: 자신의 PRIVATE/RESTRICTED 포함, 다른 owner의 RESTRICTED는 제외")
    void searchStatic_owner_includesOwnRestricted_excludesOtherOwnerRestricted() {
        persistRecipe(owner, "owner-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "owner-priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        persistRecipe(owner, "owner-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        // 다른 owner의 RESTRICTED — owner는 자기 거 아니므로 못 봄 (PUBLIC+LISTED 분기로 떨어짐)
        persistRecipe(other, "other-restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        persistRecipe(other, "other-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.searchStatic(new RecipeSearchCondition(), PAGE_10, owner.getId());

        // owner 자신: pub + priv + restricted (3개) + 다른 사람 PUBLIC+LISTED 1개
        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle)
                .containsExactlyInAnyOrder("owner-pub", "owner-priv", "owner-restricted", "other-pub");
        assertThat(result.getTotalElements()).isEqualTo(4);
    }

    @Test
    @DisplayName("searchStatic non-ACTIVE: HIDDEN/BANNED/DELETED는 owner도 차단 (admin 우회 방지)")
    void searchStatic_nonActive_blocksEvenOwner() {
        persistRecipe(owner, "active-pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        persistRecipe(owner, "banned", RecipeLifecycleStatus.BANNED, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.searchStatic(new RecipeSearchCondition(), PAGE_10, owner.getId());

        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("active-pub");
        assertThat(result.getTotalElements()).isEqualTo(1);
    }

    @Test
    @DisplayName("searchStatic count: imageReady 미충족 row는 totalElements에서도 제외 (운영 V2 count 버그를 dev에서 수정한 것 검증)")
    void searchStatic_count_imageReadyConsistent_withContent() {
        persistRecipeWithImageStatus(owner, "ready", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.READY);
        persistRecipeWithImageStatus(owner, "pending", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC,
                RecipeListingStatus.LISTED, RecipeImageStatus.PENDING);
        em.flush();
        em.clear();

        Page<DevRecipeSimpleStaticDto> result = repo.searchStatic(new RecipeSearchCondition(), PAGE_10, null);

        // PENDING row는 content/count 모두에서 제외되어야 함 (운영 V2는 count에서 빠뜨려 부풀림 발생)
        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("ready");
        assertThat(result.getTotalElements())
                .as("count query에 imageReadyCondition 누락 시 2가 나옴 — 1이 정답 (content와 일치)")
                .isEqualTo(1);
    }

    // ---------- findAllByIds: OpenSearch stale hit 시뮬레이션 (MUST 수정 검증) ----------

    @Test
    @DisplayName("findAllByIds non-owner: stale ids (RESTRICTED/PRIVATE/HIDDEN) 가 와도 DB가 차단 — PUBLIC+LISTED+ACTIVE만 반환")
    void findAllByIds_nonOwner_stale_filtersByPolicy() {
        Recipe pub = persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe priv = persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        Recipe restricted = persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        Recipe hidden = persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        // OpenSearch dev index가 stale 상태로 4개 모두 hit한 시나리오 (visibility 변경 mirror 누락)
        List<DevRecipeSimpleStaticDto> result = repo.findAllByIds(
                List.of(pub.getId(), priv.getId(), restricted.getId(), hidden.getId()),
                other.getId());

        // DB 정책 재검증으로 PUBLIC+LISTED+ACTIVE만 살아남음
        assertThat(result).extracting(DevRecipeSimpleStaticDto::getTitle).containsExactly("pub");
    }

    @Test
    @DisplayName("findAllByIds owner: 자신의 PRIVATE/RESTRICTED는 stale ids로 와도 통과 (ACTIVE 한정)")
    void findAllByIds_owner_passesOwnRestrictedAndPrivate() {
        Recipe pub = persistRecipe(owner, "pub", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        Recipe priv = persistRecipe(owner, "priv", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED);
        Recipe restricted = persistRecipe(owner, "restricted", RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED);
        Recipe hidden = persistRecipe(owner, "hidden", RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED);
        em.flush();
        em.clear();

        List<DevRecipeSimpleStaticDto> result = repo.findAllByIds(
                List.of(pub.getId(), priv.getId(), restricted.getId(), hidden.getId()),
                owner.getId());

        // owner는 ACTIVE 자신 거 3개 모두 통과, HIDDEN은 owner라도 차단
        assertThat(result).extracting(DevRecipeSimpleStaticDto::getTitle)
                .containsExactlyInAnyOrder("pub", "priv", "restricted");
    }

    @Test
    @DisplayName("findAllByIds: 빈 ids → 빈 list (DB 호출 없는 가드)")
    void findAllByIds_emptyIds_returnsEmptyList() {
        assertThat(repo.findAllByIds(List.of(), owner.getId())).isEmpty();
        assertThat(repo.findAllByIds(List.of(), null)).isEmpty();
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
        return persistRecipeWithImageStatus(user, title, lifecycle, visibility, listing, RecipeImageStatus.READY);
    }

    private Recipe persistRecipeWithImageStatus(User user, String title,
                                                 RecipeLifecycleStatus lifecycle,
                                                 RecipeVisibility visibility,
                                                 RecipeListingStatus listing,
                                                 RecipeImageStatus imageStatus) {
        Recipe recipe = Recipe.builder()
                .user(user)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .build();
        em.persist(recipe);
        return recipe;
    }
}
