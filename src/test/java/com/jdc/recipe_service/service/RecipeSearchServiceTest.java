package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingInfoDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.user.CommentUserDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.mapper.UserMapper;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import com.jdc.recipe_service.util.S3Util;
import com.jdc.recipe_service.util.SearchProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.*;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeSearchServiceTest {

    @Mock private RecipeRepository recipeRepository;
    @Mock private RecipeLikeRepository recipeLikeRepository;
    @Mock private RecipeBookItemRepository recipeBookItemRepository;
    @Mock private RecipeCommentRepository recipeCommentRepository;
    @Mock private RecipeTagRepository recipeTagRepository;
    @Mock private RecipeIngredientRepository recipeIngredientRepository;
    @Mock private RecipeStepRepository recipeStepRepository;
    @Mock private com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository recipeStepIngredientRepository;
    @Mock private com.jdc.recipe_service.domain.repository.RecipeIngredientReportRepository recipeIngredientReportRepository;
    @Mock private RecipeRatingService recipeRatingService;
    @Mock private CommentService commentService;
    @Mock private RecipeLikeService recipeLikeService;
    @Mock private RecipeFavoriteService recipeFavoriteService;
    @Mock private OpenSearchService openSearchService;
    @Mock private S3Util s3Util;
    @Mock private UserRepository userRepository;
    @Mock private SearchProperties searchProperties;

    @InjectMocks
    private RecipeSearchService recipeSearchService;

    private RecipeSearchService spyService;

    private Long existingId;
    private Long missingId;
    private Long userId;
    private Recipe sampleRecipe;

    @BeforeEach
    void setUp() {
        existingId = 1L;
        missingId = 999L;
        userId = 100L;

        // Recipe 엔티티 기본 세팅 (dishType은 절대 null이 아니도록 MAIN 등 임의값 부여)
        sampleRecipe = Recipe.builder()
                .id(existingId)
                .user(User.builder()
                        .id(2L)
                        .nickname("author")
                        .profileImage("http://profile.test")
                        .build())
                .title("테스트 레시피")
                .description("레시피 설명")
                .dishType(DishType.FRYING)          // null 대신 임의의 DishType 지정
                .cookingTime(30)
                .avgRating(BigDecimal.valueOf(4.5))
                .ratingCount(20L)
                .imageKey("main-img-key")
                .youtubeUrl("http://youtube.test")
                .cookingTools(Set.of("도구1", "도구2"))
                .isAiGenerated(false)
                .servings(2)
                .isPrivate(false)
                .totalIngredientCost(2000)
                .marketPrice(5000)
                .imageStatus(null)
                .tags(new HashSet<>())
                .ingredients(new ArrayList<>())
                .steps(new ArrayList<>())
                .likes(new ArrayList<>())
                .build();

        // BaseTimeEntity 필드(createdAt/updatedAt)가 자동으로 채워지지 않으므로 리플렉션으로 직접 값 주입
        ReflectionTestUtils.setField(sampleRecipe, "createdAt", LocalDateTime.of(2025, 1, 1, 0, 0));
        ReflectionTestUtils.setField(sampleRecipe, "updatedAt", LocalDateTime.of(2025, 1, 2, 0, 0));

        // 레시피 서비스 인스턴스를 스파이로 감싼다.
        spyService = Mockito.spy(recipeSearchService);
    }

    @Test
    @DisplayName("getRecipeDetail: 존재하는 레시피 ID인 경우, 올바른 DTO 반환")
    void getRecipeDetail_Success() {
        // Given
        ReflectionTestUtils.setField(sampleRecipe, "likeCount", 10L);
        given(recipeRepository.findDetailWithFineDiningById(existingId)).willReturn(Optional.of(sampleRecipe));
        given(recipeLikeRepository.existsByRecipeIdAndUserId(existingId, userId)).willReturn(true);
        given(recipeBookItemRepository.existsByUserIdAndRecipeId(userId, existingId)).willReturn(false);
        given(recipeRatingService.getMyRating(existingId, userId)).willReturn(5.0);
        given(recipeRatingService.getRatingCount(existingId)).willReturn(20L);

        RecipeTag tagEntity = RecipeTag.builder()
                .id(1L).recipe(sampleRecipe).tag(TagType.QUICK).build();
        given(recipeTagRepository.findByRecipeId(existingId)).willReturn(List.of(tagEntity));

        RecipeIngredient ingrEntity = RecipeIngredient.builder()
                .id(1L).recipe(sampleRecipe).ingredient(null)
                .quantity("2").unit("개").price(2000)
                .customName("감자").customPrice(1000).customUnit("개").build();
        given(recipeIngredientRepository.findByRecipeId(existingId)).willReturn(List.of(ingrEntity));

        RecipeIngredientDto ingrDto = RecipeIngredientDto.builder()
                .id(1L).name("감자").quantity("2").unit("개").price(2000).calories(1.0).coupangLink(null)
                .build();

        try (MockedStatic<RecipeIngredientMapper> ingrMapper = Mockito.mockStatic(RecipeIngredientMapper.class)) {
            ingrMapper.when(() -> RecipeIngredientMapper.toDtoList(List.of(ingrEntity))).thenReturn(List.of(ingrDto));

            RecipeStep stepEntity = RecipeStep.builder()
                    .id(1L).recipe(sampleRecipe).stepNumber(1)
                    .instruction("손질").imageKey("step-img-key").action(null).build();
            stepEntity.getStepIngredients().clear();
            given(recipeStepRepository.findByRecipeIdOrderByStepNumber(existingId)).willReturn(List.of(stepEntity));
            given(recipeStepIngredientRepository.findByStepIdIn(List.of(stepEntity.getId()))).willReturn(List.of());
            given(recipeIngredientReportRepository.findVerifiedMemberIds(existingId)).willReturn(List.of());

            String stepImageUrl = "https://bucket.region.amazonaws.com/step-img-key";
            doReturn(stepImageUrl).when(spyService).generateImageUrl("step-img-key");

            RecipeStepIngredientDto usedIngrDto = new RecipeStepIngredientDto(1L, "감자", "2", "개");
            try (MockedStatic<StepIngredientMapper> stepIngrMapper = Mockito.mockStatic(StepIngredientMapper.class)) {
                stepIngrMapper.when(() -> StepIngredientMapper.toDtoList(stepEntity.getStepIngredients()))
                        .thenReturn(List.of(usedIngrDto));

                RecipeStepDto stepDto = new RecipeStepDto(1, "손질", stepImageUrl, "step-img-key", null, null, List.of(usedIngrDto));
                try (MockedStatic<RecipeStepMapper> stepMapper = Mockito.mockStatic(RecipeStepMapper.class)) {
                    stepMapper.when(() -> RecipeStepMapper.toDto(stepEntity, List.of(usedIngrDto), stepImageUrl, "step-img-key"))
                            .thenReturn(stepDto);

                    CommentDto commentDto = CommentDto.builder()
                            .id(1L).content("댓글 내용").createdAt(LocalDateTime.of(2025, 6, 2, 0, 0))
                            .author(CommentUserDto.builder().id(2L).nickname("commenter").profileImage("http://profile.commenter").build())
                            .likeCount(0).likedByCurrentUser(false).replyCount(0).build();
                    given(commentService.getTop3CommentsWithLikes(existingId, userId)).willReturn(List.of(commentDto));
                    given(recipeCommentRepository.countByRecipeId(existingId)).willReturn(1L);

                    UserDto authorDto = UserDto.builder()
                            .id(sampleRecipe.getUser().getId()).nickname("author").profileImage("http://profile.test").build();
                    try (MockedStatic<UserMapper> userMapper = Mockito.mockStatic(UserMapper.class)) {
                        userMapper.when(() -> UserMapper.toSimpleDto(sampleRecipe.getUser())).thenReturn(authorDto);

                        // When
                        RecipeDetailDto actual = spyService.getRecipeDetail(existingId, userId);

                        // Then
                        assertThat(actual).isNotNull();
                        assertThat(actual.getId()).isEqualTo(existingId);
                        assertThat(actual.getTitle()).isEqualTo("테스트 레시피");
                        assertThat(actual.getDescription()).isEqualTo("레시피 설명");
                        assertThat(actual.isPrivate()).isFalse();
                        assertThat(actual.isAiGenerated()).isFalse();
                        assertThat(actual.getLikeCount()).isEqualTo(10L);
                        assertThat(actual.isLikedByCurrentUser()).isTrue();
                        assertThat(actual.isFavoriteByCurrentUser()).isFalse();

                        RecipeRatingInfoDto ratingInfo = actual.getRatingInfo();
                        assertThat(ratingInfo).isNotNull();
                        assertThat(ratingInfo.getAvgRating()).isEqualTo(BigDecimal.valueOf(4.5));
                        assertThat(ratingInfo.getMyRating()).isEqualTo(5L);
                        assertThat(ratingInfo.getRatingCount()).isEqualTo(20L);

                        assertThat(actual.getAuthor()).isNotNull();
                        assertThat(actual.getAuthor().getNickname()).isEqualTo("author");
                        assertThat(actual.getTags()).hasSize(1);
                        assertThat(actual.getTags().get(0)).isEqualTo(TagType.QUICK.getDisplayName());

                        assertThat(actual.getIngredients()).hasSize(1);
                        RecipeIngredientDto actIngr = actual.getIngredients().get(0);
                        assertThat(actIngr.getId()).isEqualTo(1L);
                        assertThat(actIngr.getName()).isEqualTo("감자");
                        assertThat(actIngr.getQuantity()).isEqualTo("2");
                        assertThat(actIngr.getUnit()).isEqualTo("개");
                        assertThat(actIngr.getPrice()).isEqualTo(2000);

                        assertThat(actual.getSteps()).hasSize(1);
                        RecipeStepDto actStep = actual.getSteps().get(0);
                        assertThat(actStep.getStepNumber()).isEqualTo(1);
                        assertThat(actStep.getInstruction()).isEqualTo("손질");
                        assertThat(actStep.getStepImageUrl()).isEqualTo(stepImageUrl);
                        assertThat(actStep.getIngredients()).hasSize(1);
                        assertThat(actStep.getIngredients().get(0).getName()).isEqualTo("감자");

                        assertThat(actual.getComments()).hasSize(1);
                        assertThat(actual.getComments().get(0).getContent()).isEqualTo("댓글 내용");
                        assertThat(actual.getCommentCount()).isEqualTo(1L);
                        assertThat(actual.getTotalIngredientCost()).isEqualTo(2000);
                        assertThat(actual.getMarketPrice()).isEqualTo(5000);
                        assertThat(actual.getSavings()).isEqualTo(3000);
                        assertThat(actual.getCreatedAt()).isNotNull();
                        assertThat(actual.getUpdatedAt()).isNotNull();

                        verify(recipeRepository, times(1)).findDetailWithFineDiningById(existingId);
                        verify(recipeLikeRepository, times(1)).existsByRecipeIdAndUserId(existingId, userId);
                        verify(recipeBookItemRepository, times(1)).existsByUserIdAndRecipeId(userId, existingId);
                        verify(recipeRatingService, times(1)).getMyRating(existingId, userId);
                        verify(recipeRatingService, times(1)).getRatingCount(existingId);
                        verify(recipeTagRepository, times(1)).findByRecipeId(existingId);
                        verify(recipeIngredientRepository, times(1)).findByRecipeId(existingId);
                        verify(recipeStepRepository, times(1)).findByRecipeIdOrderByStepNumber(existingId);
                        verify(spyService, times(1)).generateImageUrl("step-img-key");
                        verify(commentService, times(1)).getTop3CommentsWithLikes(existingId, userId);
                        verify(recipeCommentRepository, times(1)).countByRecipeId(existingId);
                    }
                }
            }
        }
    }

    @Test
    @DisplayName("getRecipeDetail: 없는 ID 조회 시, RECIPE_NOT_FOUND 예외 발생")
    void getRecipeDetail_NotFound() {
        // Given
        given(recipeRepository.findDetailWithFineDiningById(missingId)).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> spyService.getRecipeDetail(missingId, userId))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(recipeRepository, times(1)).findDetailWithFineDiningById(missingId);
        verifyNoMoreInteractions(
                recipeLikeRepository,
                recipeBookItemRepository,
                recipeRatingService,
                recipeTagRepository,
                recipeIngredientRepository,
                recipeStepRepository,
                commentService
        );
    }

    @Test
    @DisplayName("getRecipeDetail: PRIVATE 상태의 레시피를 다른 사용자가 조회 시, RECIPE_PRIVATE_ACCESS_DENIED 예외 발생")
    void getRecipeDetail_PrivateRecipe_OtherUser_throwException() {
        // Given
        sampleRecipe.updateIsPrivate(true);
        given(recipeRepository.findDetailWithFineDiningById(existingId)).willReturn(Optional.of(sampleRecipe));

        // When & Then
        assertThatThrownBy(() -> spyService.getRecipeDetail(existingId, userId))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED));

        verify(recipeRepository, times(1)).findDetailWithFineDiningById(existingId);
        verifyNoMoreInteractions(
                recipeLikeRepository,
                recipeBookItemRepository,
                recipeRatingService,
                recipeTagRepository,
                recipeIngredientRepository,
                recipeStepRepository,
                commentService
        );
    }

    @Test
    @DisplayName("getPopularRecipes: 주간 인기 레시피 조회 시, 기간별 좋아요 내림차순 정렬")
    void getPopularRecipes_Weekly_CallsCorrectRepositoryMethodWithSorting() {
        String period = "weekly";
        Pageable pageable = Pageable.unpaged();
        Long currentUserId = 1L;

        // Given
        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.findPopularRecipesSince(any(), any())).willReturn(mockPage);
        given(recipeRepository.search(any(), any(), any())).willReturn(mockPage);

        // When
        spyService.getPopularRecipes(period, pageable, currentUserId);

        // Then
        verify(recipeRepository, times(1)).findPopularRecipesSince(any(LocalDateTime.class), eq(pageable));
        verify(spyService, times(1)).addLikeInfoToPage(any(), eq(currentUserId));
    }

    @Test
    @DisplayName("getBudgetRecipes: 예산 레시피 조회 시, 원가 기준 오름차순 정렬")
    void getBudgetRecipes_CallsCorrectRepositoryMethodWithSorting() {
        Integer maxCost = 15000;
        Pageable pageable = Pageable.unpaged();
        Long currentUserId = 1L;

        // Given
        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.findBudgetRecipes(any(), any())).willReturn(mockPage);
        doReturn(mockPage).when(spyService).addLikeInfoToPage(any(), eq(currentUserId));

        // When
        spyService.getBudgetRecipes(maxCost, pageable, currentUserId);

        verify(recipeRepository, times(1)).findBudgetRecipes(eq(maxCost), eq(pageable));
        verify(spyService, times(1)).addLikeInfoToPage(any(), eq(currentUserId));
    }

    @Test
    @DisplayName("searchRecipes: Querydsl 사용 시, RecipeSearchCondition 객체가 Repository로 올바르게 전달되는지 확인")
    void searchRecipes_Querydsl_WithMaxCostAndSorting() {
        // Given
        given(searchProperties.getEngine()).willReturn("querydsl");

        String q = "파스타";
        Integer maxCost = 20000;
        Pageable pageable = PageRequest.of(0, 10, Sort.by("totalIngredientCost").ascending());
        Long userId = 1L;

        RecipeSearchCondition cond = new RecipeSearchCondition();
        cond.setQ(q);
        cond.setMaxCost(maxCost);

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.search(any(RecipeSearchCondition.class), eq(pageable), eq(userId))).willReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        verify(recipeRepository, times(1)).search(eq(cond), eq(pageable), eq(userId));
        assertThat(cond.getTitle()).isEqualTo("파스타");
    }

    @Test
    @DisplayName("searchRecipes: sort=popularityScore DESC 이면 searchAndSortByDynamicField(\"popularityScore\", ...) 경로로 라우팅된다")
    void searchRecipes_popularityScoreSort_routesToDynamicFieldPath() {
        // Given
        Pageable pageable = PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, "popularityScore"));
        RecipeSearchCondition cond = new RecipeSearchCondition();

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.searchAndSortByDynamicField(
                eq(cond), eq("popularityScore"), eq(Sort.Direction.DESC), eq(pageable), eq(userId)))
                .willReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        verify(recipeRepository, times(1)).searchAndSortByDynamicField(
                eq(cond), eq("popularityScore"), eq(Sort.Direction.DESC), eq(pageable), eq(userId));
        verify(recipeRepository, never()).search(any(), any(), any());
        verifyNoInteractions(openSearchService);
    }

    @Test
    @DisplayName("searchRecipes: sort=likeCount ASC 이면 searchAndSortByDynamicField(\"likeCount\", ...) 경로로 라우팅된다")
    void searchRecipes_likeCountSort_routesToDynamicFieldPath() {
        // Given
        Pageable pageable = PageRequest.of(0, 10, Sort.by(Sort.Direction.ASC, "likeCount"));
        RecipeSearchCondition cond = new RecipeSearchCondition();

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.searchAndSortByDynamicField(
                eq(cond), eq("likeCount"), eq(Sort.Direction.ASC), eq(pageable), eq(userId)))
                .willReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        verify(recipeRepository, times(1)).searchAndSortByDynamicField(
                eq(cond), eq("likeCount"), eq(Sort.Direction.ASC), eq(pageable), eq(userId));
        verify(recipeRepository, never()).search(any(), any(), any());
    }

    @Test
    @DisplayName("searchRecipes: sort=avgRating 이면 searchAndSortByDynamicField(\"avgRating\", ...) 경로로 라우팅된다")
    void searchRecipes_avgRatingSort_routesToDynamicFieldPath() {
        // Given
        Pageable pageable = PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, "avgRating"));
        RecipeSearchCondition cond = new RecipeSearchCondition();

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.searchAndSortByDynamicField(
                eq(cond), eq("avgRating"), eq(Sort.Direction.DESC), eq(pageable), eq(userId)))
                .willReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        verify(recipeRepository, times(1)).searchAndSortByDynamicField(
                eq(cond), eq("avgRating"), eq(Sort.Direction.DESC), eq(pageable), eq(userId));
    }

    @Test
    @DisplayName("addLikeInfoToPage: userId가 있으면 좋아요/즐겨찾기 조회 결과를 각 DTO에 독립적으로 주입한다")
    void addLikeInfoToPage_setsLikeAndFavoriteIndependently() {
        // given
        Long currentUserId = 10L;
        RecipeSimpleDto likedOnly = RecipeSimpleDto.builder().id(1L).imageUrl("img-1").build();
        RecipeSimpleDto favoritedOnly = RecipeSimpleDto.builder().id(2L).imageUrl("img-2").build();
        RecipeSimpleDto neither = RecipeSimpleDto.builder().id(3L).imageUrl("img-3").build();
        Page<RecipeSimpleDto> page = new PageImpl<>(List.of(likedOnly, favoritedOnly, neither));

        given(recipeLikeRepository.findRecipeIdsByUserIdAndRecipeIdIn(eq(currentUserId), anyList()))
                .willReturn(Set.of(1L));
        given(recipeBookItemRepository.findSavedRecipeIdsByUserIdAndRecipeIdIn(eq(currentUserId), anyList()))
                .willReturn(Set.of(2L));
        doReturn("img-1").when(spyService).generateImageUrl("img-1");
        doReturn("img-2").when(spyService).generateImageUrl("img-2");
        doReturn("img-3").when(spyService).generateImageUrl("img-3");

        // when
        Page<RecipeSimpleDto> result = spyService.addLikeInfoToPage(page, currentUserId);

        // then
        assertThat(result.getContent()).hasSize(3);
        assertThat(likedOnly.isLikedByCurrentUser()).isTrue();
        assertThat(likedOnly.isFavoriteByCurrentUser()).isFalse();
        assertThat(favoritedOnly.isLikedByCurrentUser()).isFalse();
        assertThat(favoritedOnly.isFavoriteByCurrentUser()).isTrue();
        assertThat(neither.isLikedByCurrentUser()).isFalse();
        assertThat(neither.isFavoriteByCurrentUser()).isFalse();

        verify(recipeLikeRepository, times(1)).findRecipeIdsByUserIdAndRecipeIdIn(eq(currentUserId), anyList());
        verify(recipeBookItemRepository, times(1)).findSavedRecipeIdsByUserIdAndRecipeIdIn(eq(currentUserId), anyList());
    }

    @Test
    @DisplayName("addLikeInfoToPage: userId가 null이면 좋아요/즐겨찾기 배치 조회를 건너뛴다")
    void addLikeInfoToPage_anonymousUser_skipsBatchQueries() {
        // given
        RecipeSimpleDto dto = RecipeSimpleDto.builder().id(1L).imageUrl("img-1").build();
        Page<RecipeSimpleDto> page = new PageImpl<>(List.of(dto));
        doReturn("img-1").when(spyService).generateImageUrl("img-1");

        // when
        Page<RecipeSimpleDto> result = spyService.addLikeInfoToPage(page, null);

        // then
        assertThat(result.getContent()).hasSize(1);
        assertThat(dto.isLikedByCurrentUser()).isFalse();
        assertThat(dto.isFavoriteByCurrentUser()).isFalse();
        verify(recipeLikeRepository, never()).findRecipeIdsByUserIdAndRecipeIdIn(any(), anyList());
        verify(recipeBookItemRepository, never()).findSavedRecipeIdsByUserIdAndRecipeIdIn(any(), anyList());
    }

    @Test
    @DisplayName("searchRecipes: sort=createdAt 이면 dynamic field 경로가 아닌 일반 검색 경로로 간다")
    void searchRecipes_createdAtSort_doesNotUseDynamicFieldPath() {
        // Given
        given(searchProperties.getEngine()).willReturn("querydsl");
        Pageable pageable = PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, "createdAt"));
        RecipeSearchCondition cond = new RecipeSearchCondition();

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        given(recipeRepository.search(eq(cond), eq(pageable), eq(userId))).willReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        verify(recipeRepository, never()).searchAndSortByDynamicField(any(), any(), any(), any(), any());
        verify(recipeRepository, times(1)).search(eq(cond), eq(pageable), eq(userId));
    }
}
