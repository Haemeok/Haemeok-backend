package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * YoutubeUrlCheckService URL canonicalize + watch/shorts fallback + repo 호출 시그니처 검증 (Mockito).
 *
 * 회귀 방지: H2 slice 테스트는 SQL 필터를 잠그지만 service의 URL 정규화/fallback 흐름과 repo method 인자 (LIMIT 1)는
 * 직접 검증하지 않는다. 이 테스트가 그 갭을 메운다.
 */
@ExtendWith(MockitoExtension.class)
class YoutubeUrlCheckServiceMockitoTest {

    private static final long OFFICIAL_USER_ID = 90121L;
    private static final String VIDEO_ID = "abc123";
    private static final String CANONICAL_WATCH = "https://www.youtube.com/watch?v=" + VIDEO_ID;
    private static final String CANONICAL_SHORTS = "https://www.youtube.com/shorts/" + VIDEO_ID;

    @Mock RecipeRepository recipeRepository;
    @Mock Recipe matchedRecipe;

    @InjectMocks YoutubeUrlCheckService service;

    @Test
    @DisplayName("invalid URL (youtube 도메인 아님) → INVALID_URL_FORMAT, repo 미호출")
    void invalidDomain_throwsAndSkipsRepo() {
        assertThatThrownBy(() -> service.checkUrlExistence("https://example.com/v=abc"))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);

        verifyNoMoreInteractions(recipeRepository);
    }

    @Test
    @DisplayName("videoId 추출 실패 (URL은 youtube이지만 v 파라미터 없음) → INVALID_URL_FORMAT")
    void noVideoId_throwsInvalidFormat() {
        // 예: https://www.youtube.com/feed/trending — pattern은 통과하지만 videoId 미추출
        assertThatThrownBy(() -> service.checkUrlExistence("https://www.youtube.com/feed/trending"))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);

        verifyNoMoreInteractions(recipeRepository);
    }

    @Test
    @DisplayName("watch URL 매칭 → repo는 canonical watch URL + LIMIT 1로 호출, recipe id 반환")
    void watchHit_callsRepoWithCanonicalWatchAndLimitOne() {
        given(matchedRecipe.getId()).willReturn(42L);
        given(recipeRepository.findStrictPublicYoutubeRecipes(eq(CANONICAL_WATCH), eq(OFFICIAL_USER_ID), any(Pageable.class)))
                .willReturn(List.of(matchedRecipe));

        Long result = service.checkUrlExistence(CANONICAL_WATCH);

        assertThat(result).isEqualTo(42L);
        // shorts fallback은 호출되지 않음
        verify(recipeRepository, times(1)).findStrictPublicYoutubeRecipes(any(), any(), any());
    }

    @Test
    @DisplayName("watch miss → shorts fallback 호출, shorts hit 시 그 id 반환")
    void watchMissShortsFallback_returnsShortsId() {
        given(matchedRecipe.getId()).willReturn(99L);
        given(recipeRepository.findStrictPublicYoutubeRecipes(eq(CANONICAL_WATCH), eq(OFFICIAL_USER_ID), any(Pageable.class)))
                .willReturn(List.of()); // watch miss
        given(recipeRepository.findStrictPublicYoutubeRecipes(eq(CANONICAL_SHORTS), eq(OFFICIAL_USER_ID), any(Pageable.class)))
                .willReturn(List.of(matchedRecipe)); // shorts hit

        Long result = service.checkUrlExistence(CANONICAL_WATCH);

        assertThat(result).isEqualTo(99L);
        verify(recipeRepository).findStrictPublicYoutubeRecipes(eq(CANONICAL_WATCH), eq(OFFICIAL_USER_ID), any(Pageable.class));
        verify(recipeRepository).findStrictPublicYoutubeRecipes(eq(CANONICAL_SHORTS), eq(OFFICIAL_USER_ID), any(Pageable.class));
    }

    @Test
    @DisplayName("watch miss + shorts miss → null")
    void allMiss_returnsNull() {
        given(recipeRepository.findStrictPublicYoutubeRecipes(any(), any(), any()))
                .willReturn(List.of());

        Long result = service.checkUrlExistence(CANONICAL_WATCH);

        assertThat(result).isNull();
        verify(recipeRepository, times(2)).findStrictPublicYoutubeRecipes(any(), any(), any());
    }

    @Test
    @DisplayName("shorts URL 입력도 같은 videoId로 canonicalize되어 watch/shorts 둘 다 시도")
    void shortsUrlInput_alsoCanonicalized() {
        given(matchedRecipe.getId()).willReturn(7L);
        given(recipeRepository.findStrictPublicYoutubeRecipes(eq(CANONICAL_WATCH), eq(OFFICIAL_USER_ID), any(Pageable.class)))
                .willReturn(List.of(matchedRecipe));

        // 입력 URL은 shorts 형태
        Long result = service.checkUrlExistence(CANONICAL_SHORTS);

        // canonicalize 후 watch URL로 먼저 매칭됨
        assertThat(result).isEqualTo(7L);
    }

    @Test
    @DisplayName("repo 호출 시 PageRequest.of(0, 1)이 전달되는지 확인 — LIMIT 1 강제 invariant")
    void repoCalledWithLimitOne() {
        given(recipeRepository.findStrictPublicYoutubeRecipes(any(), any(), any()))
                .willReturn(List.of());

        service.checkUrlExistence(CANONICAL_WATCH);

        ArgumentCaptor<Pageable> pageableCap = ArgumentCaptor.forClass(Pageable.class);
        verify(recipeRepository).findStrictPublicYoutubeRecipes(eq(CANONICAL_WATCH), eq(OFFICIAL_USER_ID), pageableCap.capture());
        assertThat(pageableCap.getValue().getPageSize()).isEqualTo(1);
        assertThat(pageableCap.getValue().getPageNumber()).isZero();
    }

    @Test
    @DisplayName("repo가 strict 통과 row 2개 반환 (race) → service는 stream().findFirst()로 첫 번째만 사용 → IncorrectResultSizeDataAccessException 회피")
    void repoReturnsTwoRows_serviceTakesFirst() {
        Recipe second = org.mockito.Mockito.mock(Recipe.class);
        given(matchedRecipe.getId()).willReturn(10L);
        given(recipeRepository.findStrictPublicYoutubeRecipes(any(), any(), any()))
                .willReturn(List.of(matchedRecipe, second));

        Long result = service.checkUrlExistence(CANONICAL_WATCH);

        assertThat(result).isEqualTo(10L);
        // second의 id는 호출되지 않음 (findFirst가 첫 번째만 사용)
        org.mockito.Mockito.verifyNoInteractions(second);
    }
}
