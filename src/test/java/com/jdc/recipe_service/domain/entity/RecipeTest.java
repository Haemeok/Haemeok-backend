package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;

class RecipeTest {

    @Nested
    @DisplayName("increaseLikeCount")
    class IncreaseLikeCount {

        @Test
        @DisplayName("likeCount와 popularityScore가 함께 1씩 증가한다")
        void bothCountersIncrement() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();

            // When
            recipe.increaseLikeCount();

            // Then
            assertThat(recipe.getLikeCount()).isEqualTo(1L);
            assertThat(recipe.getPopularityScore()).isEqualTo(1L);
        }

        @Test
        @DisplayName("likeCount나 popularityScore가 null이어도 0부터 시작해 증가한다")
        void nullValuesAreTreatedAsZero() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "likeCount", null);
            ReflectionTestUtils.setField(recipe, "popularityScore", null);

            // When
            recipe.increaseLikeCount();

            // Then
            assertThat(recipe.getLikeCount()).isEqualTo(1L);
            assertThat(recipe.getPopularityScore()).isEqualTo(1L);
        }
    }

    @Nested
    @DisplayName("decreaseLikeCount")
    class DecreaseLikeCount {

        @Test
        @DisplayName("likeCount와 popularityScore가 함께 1씩 감소한다")
        void bothCountersDecrement() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "likeCount", 5L);
            ReflectionTestUtils.setField(recipe, "popularityScore", 5L);

            // When
            recipe.decreaseLikeCount();

            // Then
            assertThat(recipe.getLikeCount()).isEqualTo(4L);
            assertThat(recipe.getPopularityScore()).isEqualTo(4L);
        }

        @Test
        @DisplayName("likeCount가 0이면 감소하지 않고 popularityScore도 유지된다")
        void zeroLikeCountIsNotDecremented() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "likeCount", 0L);
            ReflectionTestUtils.setField(recipe, "popularityScore", 3L);

            // When
            recipe.decreaseLikeCount();

            // Then
            assertThat(recipe.getLikeCount()).isEqualTo(0L);
            assertThat(recipe.getPopularityScore()).isEqualTo(3L);
        }

        @Test
        @DisplayName("popularityScore가 이미 0이면 더 이상 감소하지 않는다")
        void popularityScoreIsClampedAtZero() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "likeCount", 1L);
            ReflectionTestUtils.setField(recipe, "popularityScore", 0L);

            // When
            recipe.decreaseLikeCount();

            // Then
            assertThat(recipe.getLikeCount()).isEqualTo(0L);
            assertThat(recipe.getPopularityScore()).isEqualTo(0L);
        }
    }

    @Nested
    @DisplayName("increaseFavoriteCount")
    class IncreaseFavoriteCount {

        @Test
        @DisplayName("favoriteCount와 popularityScore가 함께 1씩 증가한다")
        void bothCountersIncrement() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();

            // When
            recipe.increaseFavoriteCount();

            // Then
            assertThat(recipe.getFavoriteCount()).isEqualTo(1L);
            assertThat(recipe.getPopularityScore()).isEqualTo(1L);
        }

        @Test
        @DisplayName("favoriteCount나 popularityScore가 null이어도 0부터 시작해 증가한다")
        void nullValuesAreTreatedAsZero() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "favoriteCount", null);
            ReflectionTestUtils.setField(recipe, "popularityScore", null);

            // When
            recipe.increaseFavoriteCount();

            // Then
            assertThat(recipe.getFavoriteCount()).isEqualTo(1L);
            assertThat(recipe.getPopularityScore()).isEqualTo(1L);
        }
    }

    @Nested
    @DisplayName("decreaseFavoriteCount")
    class DecreaseFavoriteCount {

        @Test
        @DisplayName("favoriteCount와 popularityScore가 함께 1씩 감소한다")
        void bothCountersDecrement() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "favoriteCount", 7L);
            ReflectionTestUtils.setField(recipe, "popularityScore", 7L);

            // When
            recipe.decreaseFavoriteCount();

            // Then
            assertThat(recipe.getFavoriteCount()).isEqualTo(6L);
            assertThat(recipe.getPopularityScore()).isEqualTo(6L);
        }

        @Test
        @DisplayName("favoriteCount가 0이면 감소하지 않는다")
        void zeroFavoriteCountIsNotDecremented() {
            // Given
            Recipe recipe = Recipe.builder().id(1L).build();
            ReflectionTestUtils.setField(recipe, "favoriteCount", 0L);
            ReflectionTestUtils.setField(recipe, "popularityScore", 2L);

            // When
            recipe.decreaseFavoriteCount();

            // Then
            assertThat(recipe.getFavoriteCount()).isEqualTo(0L);
            assertThat(recipe.getPopularityScore()).isEqualTo(2L);
        }
    }

    @Nested
    @DisplayName("applyVisibility (visibility/listingStatus/isPrivate 트리플 동기화)")
    class ApplyVisibility {

        @Test
        @DisplayName("PRIVATE: visibility=PRIVATE, listingStatus=UNLISTED, isPrivate=true")
        void privateAppliesAllThree() {
            // given
            Recipe recipe = Recipe.builder().id(1L).build();

            // when
            recipe.applyVisibility(RecipeVisibility.PRIVATE);

            // then
            assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PRIVATE);
            assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
            assertThat(recipe.getIsPrivate()).isTrue();
        }

        @Test
        @DisplayName("PUBLIC: visibility=PUBLIC, listingStatus=LISTED, isPrivate=false")
        void publicAppliesAllThree() {
            // given - 일부러 더러운 상태에서 시작 (PRIVATE 잔재가 정리되는지 확인)
            Recipe recipe = Recipe.builder()
                    .id(1L)
                    .visibility(RecipeVisibility.PRIVATE)
                    .listingStatus(RecipeListingStatus.UNLISTED)
                    .isPrivate(true)
                    .build();

            // when
            recipe.applyVisibility(RecipeVisibility.PUBLIC);

            // then
            assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.PUBLIC);
            assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.LISTED);
            assertThat(recipe.getIsPrivate()).isFalse();
        }

        @Test
        @DisplayName("RESTRICTED: visibility=RESTRICTED, listingStatus=UNLISTED, isPrivate=false")
        void restrictedAppliesAllThree() {
            // given - PUBLIC 상태에서 시작
            Recipe recipe = Recipe.builder()
                    .id(1L)
                    .visibility(RecipeVisibility.PUBLIC)
                    .listingStatus(RecipeListingStatus.LISTED)
                    .isPrivate(false)
                    .build();

            // when
            recipe.applyVisibility(RecipeVisibility.RESTRICTED);

            // then
            assertThat(recipe.getVisibility()).isEqualTo(RecipeVisibility.RESTRICTED);
            assertThat(recipe.getListingStatus()).isEqualTo(RecipeListingStatus.UNLISTED);
            assertThat(recipe.getIsPrivate()).isFalse();
        }
    }

    @Test
    @DisplayName("like·favorite 증감이 섞여도 popularityScore는 두 카운터의 합으로 수렴한다")
    void popularityScoreMatchesLikeAndFavoriteSum() {
        // Given
        Recipe recipe = Recipe.builder().id(1L).build();

        // When
        recipe.increaseLikeCount();
        recipe.increaseLikeCount();
        recipe.increaseFavoriteCount();
        recipe.decreaseLikeCount();
        recipe.increaseFavoriteCount();

        // Then
        assertThat(recipe.getLikeCount()).isEqualTo(1L);
        assertThat(recipe.getFavoriteCount()).isEqualTo(2L);
        assertThat(recipe.getPopularityScore())
                .isEqualTo(recipe.getLikeCount() + recipe.getFavoriteCount());
    }
}
