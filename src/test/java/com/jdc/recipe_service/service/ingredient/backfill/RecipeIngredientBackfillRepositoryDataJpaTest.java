package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;
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

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * findNormalizationBackfillTargets JPQL의 실제 H2 SQL 동작 잠금.
 *
 * <p>핵심 분기점:
 * <ul>
 *   <li>UNRESOLVED 최종 상태 → 제외</li>
 *   <li>final PARTIAL (amount 채워짐) → 제외</li>
 *   <li>old PARTIAL (3개 필드 모두 null) → 포함</li>
 *   <li>broken MAPPED (필드 일부 null) → 포함</li>
 *   <li>complete MAPPED (모두 채워짐) → 제외</li>
 *   <li>null status / unknown status → 포함</li>
 *   <li>CUSTOM → 제외</li>
 *   <li>ingredient_id null (custom row) → 제외</li>
 *   <li>raw_quantity_text/raw_unit_text null → 제외</li>
 *   <li>id <= lastId → 제외 (keyset 페이징)</li>
 * </ul>
 */
@DataJpaTest
@Import({JpaAuditingConfig.class, QuerydslConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class RecipeIngredientBackfillRepositoryDataJpaTest {

    @Autowired EntityManager em;
    @Autowired RecipeIngredientBackfillRepository repository;

    private Recipe recipe;
    private Ingredient garlic;

    @BeforeEach
    void setUp() {
        User owner = User.builder()
                .nickname("u").provider("test").oauthId("oid").build();
        em.persist(owner);

        recipe = Recipe.builder()
                .title("Test recipe").user(owner)
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.PUBLIC)
                .listingStatus(RecipeListingStatus.LISTED)
                .source(RecipeSourceType.USER)
                .popularityScore(0L)
                .build();
        em.persist(recipe);

        garlic = Ingredient.builder().name("마늘").build();
        em.persist(garlic);

        em.flush();
    }

    private RecipeIngredient persistRow(Ingredient ingredient,
                                          String rawQty, String rawUnit,
                                          String status,
                                          BigDecimal amountValue, Long unitId, BigDecimal grams) {
        RecipeIngredient ri = RecipeIngredient.builder()
                .recipe(recipe).ingredient(ingredient)
                .quantity(rawQty).unit(rawUnit)
                .rawName(ingredient != null ? ingredient.getName() : "rawX")
                .rawQuantityText(rawQty).rawUnitText(rawUnit)
                .resolutionStatus(status)
                .amountValue(amountValue)
                .ingredientUnitId(unitId)
                .normalizedGrams(grams)
                .build();
        em.persist(ri);
        return ri;
    }

    private Set<Long> queryTargetIds(long lastId) {
        return repository.findNormalizationBackfillTargets(lastId, PageRequest.of(0, 100))
                .stream().map(RecipeIngredient::getId).collect(Collectors.toSet());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: UNRESOLVED 최종 상태는 target에서 제외")
    void target_excludesUnresolvedFinal() {
        RecipeIngredient unresolved = persistRow(garlic, "약간", "쪽", "UNRESOLVED", null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).doesNotContain(unresolved.getId());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: final PARTIAL (amount 채워짐) 은 target에서 제외 — 시도 후 unit miss로 final 상태")
    void target_excludesFinalPartial() {
        // amount만 채워졌고 unit_id/grams는 null인 PARTIAL — 정규화 시도 후 단위 매칭 실패한 final 상태
        RecipeIngredient finalPartial = persistRow(garlic, "1", "봉지", "PARTIAL",
                new BigDecimal("1"), null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids)
                .as("amount이 채워진 PARTIAL은 reprocess하지 않음")
                .doesNotContain(finalPartial.getId());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: old PARTIAL (세 필드 모두 null) 은 target에 포함 — 1차 백필 시도조차 안 됨")
    void target_includesOldPartial() {
        RecipeIngredient oldPartial = persistRow(garlic, "3", "쪽", "PARTIAL", null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).contains(oldPartial.getId());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: broken MAPPED (필드 일부 null) 은 target에 포함")
    void target_includesBrokenMapped() {
        // amount만 채워지고 unit_id/grams 누락
        RecipeIngredient brokenAmountOnly = persistRow(garlic, "3", "쪽", "MAPPED",
                new BigDecimal("3"), null, null);
        // amount/unit_id 채우고 grams 누락
        RecipeIngredient brokenNoGrams = persistRow(garlic, "3", "쪽", "MAPPED",
                new BigDecimal("3"), 100L, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).contains(brokenAmountOnly.getId(), brokenNoGrams.getId());
    }

    @Test
    @DisplayName("complete MAPPED (4개 필드 모두 채워짐) 은 target에서 제외")
    void target_excludesCompleteMapped() {
        RecipeIngredient complete = persistRow(garlic, "3", "쪽", "MAPPED",
                new BigDecimal("3"), 100L, new BigDecimal("15.000"));
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).doesNotContain(complete.getId());
    }

    @Test
    @DisplayName("null status 는 target에 포함 — 1차 백필 누락")
    void target_includesNullStatus() {
        RecipeIngredient nullStatus = persistRow(garlic, "3", "쪽", null, null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).contains(nullStatus.getId());
    }

    @Test
    @DisplayName("unknown status 는 target에 포함 (대문자 정규화 실패 케이스 방어)")
    void target_includesUnknownStatus() {
        RecipeIngredient unknown = persistRow(garlic, "3", "쪽", "WEIRD_VALUE", null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).contains(unknown.getId());
    }

    @Test
    @DisplayName("CUSTOM 은 target에서 제외 — 사용자 의도 final")
    void target_excludesCustom() {
        RecipeIngredient custom = persistRow(garlic, "1", "티스푼", "CUSTOM", null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).doesNotContain(custom.getId());
    }

    @Test
    @DisplayName("ingredient_id null (custom row / C' bypass) 은 target에서 제외")
    void target_excludesIngredientNull() {
        RecipeIngredient orphan = RecipeIngredient.builder()
                .recipe(recipe).ingredient(null)
                .quantity("3").unit("쪽")
                .rawName("custom-ingredient")
                .rawQuantityText("3").rawUnitText("쪽")
                .customName("custom-ingredient").customUnit("쪽")
                .build();
        em.persist(orphan);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).doesNotContain(orphan.getId());
    }

    @Test
    @DisplayName("raw_quantity_text 또는 raw_unit_text 가 null이면 target 제외")
    void target_excludesMissingRawFields() {
        RecipeIngredient noQty = RecipeIngredient.builder()
                .recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘")
                .rawQuantityText(null).rawUnitText("쪽")
                .build();
        em.persist(noQty);
        RecipeIngredient noUnit = RecipeIngredient.builder()
                .recipe(recipe).ingredient(garlic)
                .quantity("3").unit("쪽").rawName("마늘")
                .rawQuantityText("3").rawUnitText(null)
                .build();
        em.persist(noUnit);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).doesNotContain(noQty.getId(), noUnit.getId());
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: keyset 페이징 — id <= lastId 인 row는 제외, 결과는 id ASC 정렬")
    void target_keysetPagingAndOrder() {
        RecipeIngredient r1 = persistRow(garlic, "1", "쪽", null, null, null, null);
        RecipeIngredient r2 = persistRow(garlic, "2", "쪽", null, null, null, null);
        RecipeIngredient r3 = persistRow(garlic, "3", "쪽", null, null, null, null);
        em.flush();

        // lastId = r1.id → r2, r3만
        List<RecipeIngredient> page = repository.findNormalizationBackfillTargets(
                r1.getId(), PageRequest.of(0, 100));
        List<Long> resultIds = page.stream().map(RecipeIngredient::getId).toList();
        assertThat(resultIds).containsExactly(r2.getId(), r3.getId());

        // batch size 제한
        List<RecipeIngredient> firstChunk = repository.findNormalizationBackfillTargets(
                0L, PageRequest.of(0, 2));
        assertThat(firstChunk).hasSize(2);
        assertThat(firstChunk.get(0).getId()).isEqualTo(r1.getId());
        assertThat(firstChunk.get(1).getId()).isEqualTo(r2.getId());
    }

    @Test
    @DisplayName("복합 분기: 다양한 row 섞어두고 target만 정확히 골라낸다 (MUST 통합)")
    void target_mixedRows_correctlyFiltered() {
        RecipeIngredient include1 = persistRow(garlic, "3", "쪽", null, null, null, null);  // null status
        RecipeIngredient include2 = persistRow(garlic, "3", "쪽", "PARTIAL", null, null, null);  // old PARTIAL
        RecipeIngredient include3 = persistRow(garlic, "3", "쪽", "MAPPED",
                new BigDecimal("3"), null, null);  // broken MAPPED
        RecipeIngredient exclude1 = persistRow(garlic, "약간", "쪽", "UNRESOLVED", null, null, null);
        RecipeIngredient exclude2 = persistRow(garlic, "1", "쪽", "PARTIAL",
                new BigDecimal("1"), null, null);  // final PARTIAL
        RecipeIngredient exclude3 = persistRow(garlic, "3", "쪽", "MAPPED",
                new BigDecimal("3"), 100L, new BigDecimal("15"));  // complete
        RecipeIngredient exclude4 = persistRow(garlic, "1", "티스푼", "CUSTOM", null, null, null);
        em.flush();

        Set<Long> ids = queryTargetIds(0L);
        assertThat(ids).containsExactlyInAnyOrder(include1.getId(), include2.getId(), include3.getId());
        assertThat(ids).doesNotContain(exclude1.getId(), exclude2.getId(), exclude3.getId(), exclude4.getId());
    }
}
