package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
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
import org.springframework.test.context.TestPropertySource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeRemixQueryRepository.findStrictRemixesByOriginRecipeId — H2 SQL 회귀 테스트.
 *
 * 핵심 invariants:
 *  - 운영 쿼리에 없던 imageReady (READY OR NULL) 조건이 적용됨 → PENDING/FAILED remix 제외
 *  - 4-enum strict (PUBLIC + LISTED + ACTIVE + isPrivate=false) 유지
 *  - origin이 다른 row 제외
 *  - count 쿼리도 동일 WHERE 적용 (page totalElements와 list.size 일치)
 */
@DataJpaTest
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop"
})
class DevRecipeRemixQueryRepositoryTest {

    @Autowired EntityManager em;
    @Autowired DevRecipeRemixQueryRepository repo;

    private User owner;
    private Recipe origin;

    @BeforeEach
    void setUp() {
        owner = persistUser();
        em.flush();
        origin = persistRecipe("origin", null,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        em.flush();
    }

    @Test
    @DisplayName("strict 모두 통과: PUBLIC+LISTED+ACTIVE+READY remix 매칭")
    void allConditionsPass_returnsRemix() {
        persistRecipe("remix-ok", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getTotalElements()).isEqualTo(1);
        assertThat(result.getContent()).extracting(RecipeSimpleDto::getTitle).containsExactly("remix-ok");
    }

    @Test
    @DisplayName("imageStatus=NULL legacy remix → 매칭 (dev 컨벤션: READY OR NULL)")
    void imageStatusNullLegacy_matches() {
        persistRecipe("remix-null", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                null);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getContent()).extracting(RecipeSimpleDto::getTitle).containsExactly("remix-null");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: imageStatus=PENDING remix 제외 (운영 쿼리는 통과시켰음)")
    void imageStatusPending_excluded() {
        persistRecipe("remix-pending", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.PENDING);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getTotalElements()).isZero();
    }

    @Test
    @DisplayName("imageStatus=FAILED remix 제외")
    void imageStatusFailed_excluded() {
        persistRecipe("remix-failed", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.FAILED);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getTotalElements()).isZero();
    }

    @Test
    @DisplayName("PRIVATE/RESTRICTED/non-ACTIVE remix 제외하지만 PUBLIC+UNLISTED(link-only)는 포함 (V1.x 리믹스 정책)")
    void nonViewableRemixesExcluded_butPublicUnlistedIncluded() {
        persistRecipe("private", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY);
        persistRecipe("restricted", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY);
        persistRecipe("hidden", origin,
                RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        // V1.x: 리믹스는 항상 PUBLIC+UNLISTED로 생성됨 — 이 케이스가 원본의 리믹스 목록에 포함되지 않으면 모든 리믹스가 빠진다.
        persistRecipe("public-unlisted", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        // PUBLIC+UNLISTED만 통과
        assertThat(result.getContent()).extracting(RecipeSimpleDto::getTitle)
                .containsExactly("public-unlisted");
    }

    @Test
    @DisplayName("다른 origin의 remix는 제외 (origin id 일치 조건)")
    void differentOrigin_excluded() {
        Recipe otherOrigin = persistRecipe("other-origin", null,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        persistRecipe("remix-other", otherOrigin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getTotalElements()).isZero();
    }

    @Test
    @DisplayName("count + list 쿼리 WHERE 일치: 혼합 데이터에서 totalElements == 매칭 row 수")
    void countAndListAgree() {
        // 매칭 2개
        persistRecipe("ok1", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        persistRecipe("ok2", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                null);
        // 비매칭 3개
        persistRecipe("pending", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.PENDING);
        persistRecipe("private", origin,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY);
        persistRecipe("hidden", origin,
                RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY);
        em.flush(); em.clear();

        Page<RecipeSimpleDto> result = repo.findStrictRemixesByOriginRecipeId(origin.getId(), PageRequest.of(0, 10));

        assertThat(result.getTotalElements()).isEqualTo(2);
        assertThat(result.getContent()).hasSize(2);
    }

    // ---------- fixtures ----------

    private User persistUser() {
        User user = User.builder()
                .provider("test")
                .oauthId("o")
                .nickname("nick")
                .role(Role.USER)
                .build();
        em.persist(user);
        return user;
    }

    private Recipe persistRecipe(String title, Recipe origin,
                                  RecipeLifecycleStatus lifecycle,
                                  RecipeVisibility visibility,
                                  RecipeListingStatus listing,
                                  RecipeImageStatus imageStatus) {
        Recipe.RecipeBuilder b = Recipe.builder()
                .user(owner)
                .title(title)
                .dishType(DishType.FRYING)
                .lifecycleStatus(lifecycle)
                .visibility(visibility)
                .listingStatus(listing)
                .source(RecipeSourceType.USER)
                .imageStatus(imageStatus)
                .imageKey("key")
                .isPrivate(visibility == RecipeVisibility.PRIVATE);
        if (origin != null) b.originRecipe(origin);
        Recipe recipe = b.build();
        em.persist(recipe);
        return recipe;
    }
}
