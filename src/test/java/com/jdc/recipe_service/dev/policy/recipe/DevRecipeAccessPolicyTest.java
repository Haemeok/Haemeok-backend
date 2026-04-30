package com.jdc.recipe_service.dev.policy.recipe;

import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeAccessPolicy 의미 검증.
 *
 * 두 층으로 잡는다:
 *  - **Exhaustive matrix** (가장 아래 nested 그룹): {@code RecipeLifecycleStatus.values() × RecipeVisibility.values()
 *    × RecipeListingStatus.values()} 24조합을 전수 순회하며 expected formula와 비교. enum 추가 시 자동으로
 *    새 조합 검증.
 *  - **Representative invariant cases** (위쪽 nested 그룹들): RESTRICTED 누수 방지 등 깨지면 시스템이 무너지는
 *    핵심 분기를 명시적으로 한 번 더 못박는다 (실패 메시지가 "왜 깨졌는지" 즉시 드러나도록).
 *
 * 핵심 invariant:
 *  1. publicListedActive: ACTIVE + PUBLIC + LISTED 한 조합만 true
 *  2. accessibleBy: non-ACTIVE는 owner도 false (admin 우회 방지)
 *  3. accessibleBy: ACTIVE면 owner는 visibility/listing 무관 true
 *  4. accessibleBy: ACTIVE + non-owner는 PUBLIC + LISTED만 true (RESTRICTED 누수 차단)
 *  5. accessibleBy: viewerId=null는 anonymous → publicListedActive와 동일
 */
class DevRecipeAccessPolicyTest {

    private static final Long OWNER_ID = 100L;
    private static final Long OTHER_VIEWER_ID = 200L;

    @Nested
    @DisplayName("isPublicListedActive")
    class PublicListedActive {

        @Test
        @DisplayName("ACTIVE + PUBLIC + LISTED만 true")
        void onlyTriple_isTrue() {
            assertThat(DevRecipeAccessPolicy.isPublicListedActive(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED))
                    .isTrue();
        }

        @ParameterizedTest(name = "lifecycle={0}, visibility={1}, listing={2} → false")
        @MethodSource("nonPublicListedActiveCombinations")
        @DisplayName("나머지 모든 조합은 false (non-ACTIVE / non-PUBLIC / non-LISTED 어느 하나라도 어긋나면 거부)")
        void anyDeviation_isFalse(RecipeLifecycleStatus lifecycle,
                                   RecipeVisibility visibility,
                                   RecipeListingStatus listing) {
            assertThat(DevRecipeAccessPolicy.isPublicListedActive(lifecycle, visibility, listing))
                    .isFalse();
        }

        static Stream<Arguments> nonPublicListedActiveCombinations() {
            return Stream.of(
                    // non-ACTIVE는 visibility/listing 어떻든 false
                    Arguments.of(RecipeLifecycleStatus.HIDDEN,  RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED),
                    Arguments.of(RecipeLifecycleStatus.BANNED,  RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED),
                    Arguments.of(RecipeLifecycleStatus.DELETED, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED),
                    // ACTIVE + non-PUBLIC
                    Arguments.of(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE,    RecipeListingStatus.LISTED),
                    Arguments.of(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.LISTED),
                    // ACTIVE + PUBLIC + UNLISTED — 핵심 invariant: RESTRICTED→UNLISTED라 PUBLIC+UNLISTED 자체는 비정상이지만 정책은 enum만 본다
                    Arguments.of(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED),
                    // ACTIVE + RESTRICTED + UNLISTED — RESTRICTED의 정상 상태이고, 검색에 절대 노출되면 안 됨
                    Arguments.of(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED),
                    // ACTIVE + PRIVATE + UNLISTED — PRIVATE의 정상 상태
                    Arguments.of(RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED)
            );
        }
    }

    @Nested
    @DisplayName("isAccessibleBy — non-ACTIVE는 owner도 차단")
    class NonActiveBlocksEvenOwner {

        @ParameterizedTest(name = "lifecycle={0} → owner라도 false")
        @MethodSource("nonActiveLifecycles")
        @DisplayName("HIDDEN/BANNED/DELETED는 admin 조치 우회 방지 — owner 자신도 접근 거부")
        void nonActive_evenOwner_isFalse(RecipeLifecycleStatus lifecycle) {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    lifecycle, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED, OWNER_ID, OWNER_ID))
                    .isFalse();
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    lifecycle, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED, OWNER_ID, OWNER_ID))
                    .isFalse();
        }

        static Stream<RecipeLifecycleStatus> nonActiveLifecycles() {
            return Stream.of(
                    RecipeLifecycleStatus.HIDDEN,
                    RecipeLifecycleStatus.BANNED,
                    RecipeLifecycleStatus.DELETED);
        }
    }

    @Nested
    @DisplayName("isAccessibleBy — ACTIVE + owner는 visibility/listing 무관")
    class OwnerSeesOwnRecipes {

        @ParameterizedTest(name = "ACTIVE + visibility={0} + listing={1} → owner는 true")
        @MethodSource("allVisibilityListingCombos")
        @DisplayName("ACTIVE 상태에서 owner는 PRIVATE/RESTRICTED/UNLISTED 자신의 레시피를 모두 볼 수 있다")
        void active_owner_alwaysTrue(RecipeVisibility visibility, RecipeListingStatus listing) {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, visibility, listing, OWNER_ID, OWNER_ID))
                    .isTrue();
        }

        static Stream<Arguments> allVisibilityListingCombos() {
            return Stream.of(
                    Arguments.of(RecipeVisibility.PUBLIC,     RecipeListingStatus.LISTED),
                    Arguments.of(RecipeVisibility.PUBLIC,     RecipeListingStatus.UNLISTED),
                    Arguments.of(RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED),
                    Arguments.of(RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED));
        }
    }

    @Nested
    @DisplayName("isAccessibleBy — ACTIVE + non-owner는 PUBLIC+LISTED만")
    class NonOwnerNeedsPublicListed {

        @Test
        @DisplayName("non-owner는 PUBLIC + LISTED만 접근 가능")
        void nonOwner_publicListed_isTrue() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                    OTHER_VIEWER_ID, OWNER_ID))
                    .isTrue();
        }

        @ParameterizedTest(name = "ACTIVE + visibility={0} + listing={1} → non-owner는 false")
        @MethodSource("nonPublicListedCombos")
        @DisplayName("PRIVATE/RESTRICTED/UNLISTED 중 어느 하나라도 있으면 non-owner는 차단 (RESTRICTED 누수 방지의 핵심 invariant)")
        void nonOwner_nonPublicListed_isFalse(RecipeVisibility visibility, RecipeListingStatus listing) {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, visibility, listing, OTHER_VIEWER_ID, OWNER_ID))
                    .isFalse();
        }

        static Stream<Arguments> nonPublicListedCombos() {
            return Stream.of(
                    Arguments.of(RecipeVisibility.PRIVATE,    RecipeListingStatus.UNLISTED),
                    Arguments.of(RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED),
                    Arguments.of(RecipeVisibility.PUBLIC,     RecipeListingStatus.UNLISTED));
        }
    }

    @Nested
    @DisplayName("isAccessibleBy — anonymous (viewerId=null)")
    class AnonymousMatchesPublicListedActive {

        @Test
        @DisplayName("viewerId=null + ACTIVE + PUBLIC + LISTED → true")
        void anonymous_publicListedActive_isTrue() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                    null, OWNER_ID))
                    .isTrue();
        }

        @Test
        @DisplayName("viewerId=null + RESTRICTED → false (RESTRICTED 누수 방지)")
        void anonymous_restricted_isFalse() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                    null, OWNER_ID))
                    .isFalse();
        }

        @Test
        @DisplayName("viewerId=null + PRIVATE → false")
        void anonymous_private_isFalse() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                    null, OWNER_ID))
                    .isFalse();
        }

        @Test
        @DisplayName("viewerId=null + non-ACTIVE → false (lifecycle 우선 차단)")
        void anonymous_nonActive_isFalse() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.HIDDEN, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                    null, OWNER_ID))
                    .isFalse();
        }
    }

    @Nested
    @DisplayName("isAccessibleBy — ownerId가 null인 비정상 데이터 방어")
    class NullOwnerId {

        @Test
        @DisplayName("ownerId=null이면 viewerId가 있어도 owner 매칭 안 됨 → public listed 분기로 떨어짐")
        void nullOwnerId_neverMatchesViewer() {
            assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                    RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PRIVATE, RecipeListingStatus.UNLISTED,
                    OWNER_ID, null))
                    .isFalse();
        }
    }

    // =========================================================================
    // Exhaustive matrix — enum.values() 전수 순회로 정의가 빠짐없이 적용되는지 보장
    // (enum 추가/수정 시 새 조합도 자동으로 검증됨)
    // =========================================================================

    @Nested
    @DisplayName("Exhaustive matrix — enum.values() 전수 순회")
    class ExhaustiveMatrix {

        @Test
        @DisplayName("isPublicListedActive: 4×3×2=24조합 모두 ACTIVE && PUBLIC && LISTED 공식과 일치")
        void publicListedActive_matrixMatchesFormula() {
            for (RecipeLifecycleStatus lifecycle : RecipeLifecycleStatus.values()) {
                for (RecipeVisibility visibility : RecipeVisibility.values()) {
                    for (RecipeListingStatus listing : RecipeListingStatus.values()) {
                        boolean expected = (lifecycle == RecipeLifecycleStatus.ACTIVE)
                                && (visibility == RecipeVisibility.PUBLIC)
                                && (listing == RecipeListingStatus.LISTED);
                        assertThat(DevRecipeAccessPolicy.isPublicListedActive(lifecycle, visibility, listing))
                                .as("lifecycle=%s, visibility=%s, listing=%s", lifecycle, visibility, listing)
                                .isEqualTo(expected);
                    }
                }
            }
        }

        @Test
        @DisplayName("isAccessibleBy(owner): 24조합 모두 'ACTIVE && (owner OR (PUBLIC && LISTED))' 공식과 일치 — owner 시점")
        void accessibleBy_owner_matrixMatchesFormula() {
            assertMatrixMatchesAccessibleByFormula(OWNER_ID, OWNER_ID, /*isOwner*/ true);
        }

        @Test
        @DisplayName("isAccessibleBy(non-owner): 24조합 모두 공식과 일치 — non-owner 시점 (RESTRICTED 누수 차단 매트릭스)")
        void accessibleBy_nonOwner_matrixMatchesFormula() {
            assertMatrixMatchesAccessibleByFormula(OTHER_VIEWER_ID, OWNER_ID, /*isOwner*/ false);
        }

        @Test
        @DisplayName("isAccessibleBy(anonymous): 24조합 모두 공식과 일치 — viewerId=null 시점")
        void accessibleBy_anonymous_matrixMatchesFormula() {
            assertMatrixMatchesAccessibleByFormula(null, OWNER_ID, /*isOwner*/ false);
        }

        private void assertMatrixMatchesAccessibleByFormula(Long viewerId, Long ownerId, boolean isOwner) {
            for (RecipeLifecycleStatus lifecycle : RecipeLifecycleStatus.values()) {
                for (RecipeVisibility visibility : RecipeVisibility.values()) {
                    for (RecipeListingStatus listing : RecipeListingStatus.values()) {
                        boolean expected;
                        if (lifecycle != RecipeLifecycleStatus.ACTIVE) {
                            // 정책 invariant #2: non-ACTIVE는 owner도 차단
                            expected = false;
                        } else if (isOwner) {
                            // 정책 invariant #3: ACTIVE && owner면 visibility/listing 무관 true
                            expected = true;
                        } else {
                            // 정책 invariant #4 & #5: ACTIVE && non-owner(or anonymous)는 PUBLIC && LISTED만
                            expected = (visibility == RecipeVisibility.PUBLIC)
                                    && (listing == RecipeListingStatus.LISTED);
                        }
                        assertThat(DevRecipeAccessPolicy.isAccessibleBy(
                                lifecycle, visibility, listing, viewerId, ownerId))
                                .as("viewerId=%s, ownerId=%s, lifecycle=%s, visibility=%s, listing=%s",
                                        viewerId, ownerId, lifecycle, visibility, listing)
                                .isEqualTo(expected);
                    }
                }
            }
        }
    }
}
