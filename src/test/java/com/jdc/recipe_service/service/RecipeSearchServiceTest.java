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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeSearchServiceTest {

    @Mock private RecipeRepository recipeRepository;
    @Mock private RecipeLikeRepository recipeLikeRepository;
    @Mock private RecipeFavoriteRepository recipeFavoriteRepository;
    @Mock private RecipeCommentRepository recipeCommentRepository;
    @Mock private RecipeTagRepository recipeTagRepository;
    @Mock private RecipeIngredientRepository recipeIngredientRepository;
    @Mock private RecipeStepRepository recipeStepRepository;
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
        // 1) recipeRepository.findWithUserById(existingId) 모킹
        when(recipeRepository.findWithUserById(existingId))
                .thenReturn(Optional.of(sampleRecipe));

        // 2) 좋아요·즐겨찾기 정보 모킹
        when(recipeLikeRepository.countByRecipeId(existingId)).thenReturn(10);
        when(recipeLikeRepository.existsByRecipeIdAndUserId(existingId, userId)).thenReturn(true);
        when(recipeFavoriteRepository.existsByRecipeIdAndUserId(existingId, userId)).thenReturn(false);

        // 3) RatingService 모킹
        when(recipeRatingService.getMyRating(existingId, userId)).thenReturn(5.0);
        when(recipeRatingService.getRatingCount(existingId)).thenReturn(20L);

        // 4) 태그 목록 모킹
        RecipeTag tagEntity = RecipeTag.builder()
                .id(1L)
                .recipe(sampleRecipe)
                .tag(TagType.QUICK)
                .build();
        when(recipeTagRepository.findByRecipeId(existingId))
                .thenReturn(List.of(tagEntity));

        // 5) 재료 목록 모킹
        RecipeIngredient ingrEntity = RecipeIngredient.builder()
                .id(1L)
                .recipe(sampleRecipe)
                .ingredient(null)
                .quantity("2")
                .unit("개")
                .price(2000)
                .customName("감자")
                .customPrice(1000)
                .customUnit("개")
                .build();
        when(recipeIngredientRepository.findByRecipeId(existingId))
                .thenReturn(List.of(ingrEntity));

        RecipeIngredientDto ingrDto = new RecipeIngredientDto(
                1L, "감자", "2", "개", 2000,1.0, null
        );
        try (MockedStatic<RecipeIngredientMapper> ingrMapper = Mockito.mockStatic(RecipeIngredientMapper.class)) {
            ingrMapper.when(() ->
                    RecipeIngredientMapper.toDtoList(List.of(ingrEntity))
            ).thenReturn(List.of(ingrDto));

            // 6) 단계 목록 모킹
            RecipeStep stepEntity = RecipeStep.builder()
                    .id(1L)
                    .recipe(sampleRecipe)
                    .stepNumber(1)
                    .instruction("손질")
                    .imageKey("step-img-key")
                    .action(null)
                    .build();
            stepEntity.getStepIngredients().clear();
            when(recipeStepRepository.findWithIngredientsByRecipeIdOrderByStepNumber(existingId))
                    .thenReturn(List.of(stepEntity));

            // generateImageUrl(...) 오버라이드: "step-img-key" 요청 시 stepImageUrl 리턴
            String stepImageUrl = "https://bucket.region.amazonaws.com/step-img-key";
            doReturn(stepImageUrl).when(spyService).generateImageUrl("step-img-key");

            RecipeStepIngredientDto usedIngrDto = new RecipeStepIngredientDto(
                    1L, "감자", "2", "개"
            );
            try (MockedStatic<StepIngredientMapper> stepIngrMapper = Mockito.mockStatic(StepIngredientMapper.class)) {
                stepIngrMapper.when(() ->
                        StepIngredientMapper.toDtoList(stepEntity.getStepIngredients())
                ).thenReturn(List.of(usedIngrDto));

                RecipeStepDto stepDto = new RecipeStepDto(
                        1,
                        "손질",
                        stepImageUrl,
                        "step-img-key",
                        null,
                        List.of(usedIngrDto)
                );
                try (MockedStatic<RecipeStepMapper> stepMapper = Mockito.mockStatic(RecipeStepMapper.class)) {
                    stepMapper.when(() ->
                            RecipeStepMapper.toDto(stepEntity, List.of(usedIngrDto), stepImageUrl, "step-img-key")
                    ).thenReturn(stepDto);

                    // 7) CommentService 모킹
                    CommentUserDto authorDtoComment = CommentUserDto.builder()
                            .id(2L)
                            .nickname("commenter")
                            .profileImage("http://profile.commenter")
                            .build();

                    CommentDto commentDto = CommentDto.builder()
                            .id(1L)
                            .content("댓글 내용")
                            .createdAt(LocalDateTime.of(2025, 6, 2, 0, 0))
                            .author(authorDtoComment)
                            .likeCount(0)
                            .likedByCurrentUser(false)
                            .replyCount(0)
                            .build();

                    when(commentService.getTop3CommentsWithLikes(existingId, userId))
                            .thenReturn(List.of(commentDto));
                    when(recipeCommentRepository.countByRecipeId(existingId)).thenReturn(1L);

                    // 8) UserMapper 모킹
                    UserDto authorDto = UserDto.builder()
                            .id(sampleRecipe.getUser().getId())
                            .nickname("author")
                            .profileImage("http://profile.test")
                            .build();
                    try (MockedStatic<UserMapper> userMapper = Mockito.mockStatic(UserMapper.class)) {
                        userMapper.when(() ->
                                UserMapper.toSimpleDto(sampleRecipe.getUser())
                        ).thenReturn(authorDto);

                        // --- 실제 서비스 호출 (스파이 사용) ---
                        RecipeDetailDto actual = spyService.getRecipeDetail(existingId, userId);

                        // DTO 기본 필드 검증
                        assertNotNull(actual);
                        assertEquals(existingId, actual.getId());
                        assertEquals("테스트 레시피", actual.getTitle());
                        assertEquals("레시피 설명", actual.getDescription());

                        // private=false → 접근 가능
                        assertFalse(actual.isPrivate());
                        assertFalse(actual.isAiGenerated());

                        // 좋아요·즐겨찾기 정보
                        assertEquals(10L, actual.getLikeCount());
                        assertTrue(actual.isLikedByCurrentUser());
                        assertFalse(actual.isFavoriteByCurrentUser());

                        // RatingInfo 검증
                        RecipeRatingInfoDto ratingInfo = actual.getRatingInfo();
                        assertNotNull(ratingInfo);
                        assertEquals(BigDecimal.valueOf(4.5), ratingInfo.getAvgRating());
                        assertEquals(5L, ratingInfo.getMyRating());
                        assertEquals(20L, ratingInfo.getRatingCount());

                        // author 검증
                        assertNotNull(actual.getAuthor());
                        assertEquals("author", actual.getAuthor().getNickname());

                        // tags: TagType.QUICK → displayName
                        assertEquals(1, actual.getTags().size());
                        assertEquals(TagType.QUICK.getDisplayName(), actual.getTags().get(0));

                        // ingredients
                        assertEquals(1, actual.getIngredients().size());
                        RecipeIngredientDto actIngr = actual.getIngredients().get(0);
                        assertEquals(1L, actIngr.getId());
                        assertEquals("감자", actIngr.getName());
                        assertEquals("2", actIngr.getQuantity());
                        assertEquals("개", actIngr.getUnit());
                        assertEquals(2000, actIngr.getPrice());

                        // steps
                        assertEquals(1, actual.getSteps().size());
                        RecipeStepDto actStep = actual.getSteps().get(0);
                        assertEquals(1, actStep.getStepNumber());
                        assertEquals("손질", actStep.getInstruction());
                        assertEquals(stepImageUrl, actStep.getStepImageUrl());
                        assertEquals(1, actStep.getIngredients().size());
                        assertEquals("감자", actStep.getIngredients().get(0).getName());

                        // comments
                        assertEquals(1, actual.getComments().size());
                        assertEquals("댓글 내용", actual.getComments().get(0).getContent());
                        assertEquals(1L, actual.getCommentCount());

                        // cost·price·savings
                        assertEquals(2000, actual.getTotalIngredientCost());
                        assertEquals(5000, actual.getMarketPrice());
                        assertEquals(3000, actual.getSavings());

                        // createdAt·updatedAt: 널이 아니어야 함
                        assertNotNull(actual.getCreatedAt());
                        assertNotNull(actual.getUpdatedAt());

                        // 내부 호출 verify
                        verify(recipeRepository, times(1)).findWithUserById(existingId);
                        verify(recipeLikeRepository, times(1)).countByRecipeId(existingId);
                        verify(recipeLikeRepository, times(1)).existsByRecipeIdAndUserId(existingId, userId);
                        verify(recipeFavoriteRepository, times(1)).existsByRecipeIdAndUserId(existingId, userId);
                        verify(recipeRatingService, times(1)).getMyRating(existingId, userId);
                        verify(recipeRatingService, times(1)).getRatingCount(existingId);
                        verify(recipeTagRepository, times(1)).findByRecipeId(existingId);
                        verify(recipeIngredientRepository, times(1)).findByRecipeId(existingId);
                        verify(recipeStepRepository, times(1))
                                .findWithIngredientsByRecipeIdOrderByStepNumber(existingId);
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
        when(recipeRepository.findWithUserById(missingId))
                .thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            spyService.getRecipeDetail(missingId, userId);
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(missingId);
        verifyNoMoreInteractions(
                recipeLikeRepository,
                recipeFavoriteRepository,
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
        // sampleRecipe의 isPrivate를 true로 변경
        sampleRecipe.updateIsPrivate(true);

        when(recipeRepository.findWithUserById(existingId))
                .thenReturn(Optional.of(sampleRecipe));

        // 작성자(user.id=2L)와 조회자(userId=100L)가 다르므로 예외 발생
        CustomException ex = assertThrows(CustomException.class, () -> {
            spyService.getRecipeDetail(existingId, userId);
        });
        assertEquals(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED, ex.getErrorCode());

        verify(recipeRepository, times(1)).findWithUserById(existingId);
        verifyNoMoreInteractions(
                recipeLikeRepository,
                recipeFavoriteRepository,
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

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        when(recipeRepository.findPopularRecipesSince(any(), any())).thenReturn(mockPage);

        spyService.getPopularRecipes(period, pageable, currentUserId);

        verify(recipeRepository, times(1)).findPopularRecipesSince(any(LocalDateTime.class), eq(pageable));

        verify(spyService, times(1)).addLikeInfoToPage(any(), eq(currentUserId));
    }

    @Test
    @DisplayName("getBudgetRecipes: 예산 레시피 조회 시, 원가 기준 오름차순 정렬")
    void getBudgetRecipes_CallsCorrectRepositoryMethodWithSorting() {
        Integer maxCost = 15000;
        Pageable pageable = Pageable.unpaged();
        Long currentUserId = 1L;

        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        when(recipeRepository.findBudgetRecipes(any(), any())).thenReturn(mockPage);

        doReturn(mockPage)
                .when(spyService)
                .addLikeInfoToPage(any(), eq(currentUserId));

        spyService.getBudgetRecipes(maxCost, pageable, currentUserId);

        verify(recipeRepository, times(1)).findBudgetRecipes(eq(maxCost), eq(pageable));
        verify(spyService, times(1)).addLikeInfoToPage(any(), eq(currentUserId));
    }

    @Test
    @DisplayName("searchRecipes: Querydsl 사용 시, maxCost 및 정렬 조건 전달")
    void searchRecipes_Querydsl_WithMaxCostAndSorting() {
        // Given
        // Querydsl 사용하도록 설정
        when(searchProperties.getEngine()).thenReturn("querydsl");

        String q = "파스타";
        Integer maxCost = 20000;
        Pageable pageable = PageRequest.of(0, 10, Sort.by("totalIngredientCost").ascending());
        Long userId = 1L;

        RecipeSearchCondition cond = new RecipeSearchCondition(q, null, null, null, maxCost);

        // Mocking: recipeRepository.search 메서드가 Page를 반환하도록 설정
        Page<RecipeSimpleDto> mockPage = new PageImpl<>(List.of());
        when(recipeRepository.search(any(), any(), any(), any(), any(), any(), any())).thenReturn(mockPage);

        // When
        recipeSearchService.searchRecipes(cond, pageable, userId);

        // Then
        // recipeRepository.search 메서드가 올바른 인자들로 호출되었는지 검증
        verify(recipeRepository, times(1)).search(
                eq(q),
                any(), // dishType
                any(), // tagTypes
                any(), // isAiGenerated
                eq(maxCost),
                eq(pageable),
                eq(userId)
        );
    }
}
