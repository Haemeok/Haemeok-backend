package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.exception.RecipeAccessDeniedException;
import com.jdc.recipe_service.mapper.*;
import lombok.RequiredArgsConstructor;
import org.apache.coyote.BadRequestException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
public class RecipeService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeTagRepository recipeTagRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final UserRepository userRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final CommentLikeRepository commentLikeRepository;

    private final CommentService commentService;
    private final RecipeIngredientService recipeIngredientService;
    private final RecipeStepService recipeStepService;
    private final RecipeTagService recipeTagService;
    private final RecipeRatingService recipeRatingService;

    @Transactional
    public Long createRecipe(RecipeCreateRequestDto dto, Long userId) {
        // 1. 작성자 조회
        User user = getUserOrThrow(userId);

        // 2. 레시피 저장
        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);


        // 3. 하위 도메인 저장 처리
        recipeIngredientService.saveAll(recipe, dto.getIngredients());
        recipeRepository.flush();
        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTagNames());
        return recipe.getId();
    }

    @Transactional
    public Long createUserRecipe(RecipeUserCreateRequestDto dto, Long userId) {
        // 1. 작성자 조회
        User user = getUserOrThrow(userId);

        // 2. 레시피 저장
        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);

        // 3. 하위 도메인 저장 처리
        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
        recipe.setTotalIngredientCost(totalCost); // ✅ 총 원가 저장
        recipeRepository.flush();

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTagNames());

        return recipe.getId();
    }

    @Transactional(readOnly = true)
    public List<RecipeSimpleDto> getAllRecipesSimple(Long currentUserId) {
        // 1. 좋아요 수 포함된 레시피 목록 조회 (JPQL에서 DTO로 Projection)
        List<RecipeSimpleDto> recipes = recipeRepository.findAllWithLikeCount();

        if (currentUserId == null) {
            return recipes; // 로그인 안 한 경우, likedByCurrentUser는 false로 유지
        }

        // 2. 레시피 ID 목록 추출
        List<Long> recipeIds = recipes.stream()
                .map(RecipeSimpleDto::getId)
                .toList();

        // 3. 로그인 유저가 좋아요 누른 레시피 조회
        Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(rl -> rl.getRecipe().getId())
                .collect(Collectors.toSet());

        // 4. 각 DTO에 좋아요 여부 설정
        recipes.forEach(dto -> {
            if (likedRecipeIds.contains(dto.getId())) {
                dto.setLikedByCurrentUser(true);
            }
        });

        return recipes;
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchRecipes(RecipeSearchCondition condition, Pageable pageable, Long userId) {
        Page<RecipeSimpleDto> page = recipeRepository.search(condition, pageable);

        if (userId != null) {
            List<Long> recipeIds = page.getContent().stream()
                    .map(RecipeSimpleDto::getId)
                    .toList();

            Set<Long> likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(userId, recipeIds)
                    .stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());

            page.getContent().forEach(dto -> dto.setLikedByCurrentUser(likedIds.contains(dto.getId())));
        }

        return page;
    }


    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getByTagWithLikeInfo(String tagName, Long currentUserId, Pageable pageable) throws BadRequestException {
        // 1. Enum name 기준으로 변환 (예: "HOME_PARTY" → TagType.HOME_PARTY)
        TagType tag = TagType.fromNameOrThrow(tagName);

        // 2. 태그 기준 좋아요 수 포함된 Projection 조회
        Page<RecipeSimpleDto> page = recipeRepository.findByTagWithLikeCount(tag, pageable);
        List<RecipeSimpleDto> recipes = page.getContent();

        if (currentUserId == null) {
            return page; // 비로그인 유저는 like 정보 없이 그대로 반환
        }

        // 3. recipeIds 추출
        List<Long> recipeIds = recipes.stream()
                .map(RecipeSimpleDto::getId)
                .toList();

        // 4. 유저가 좋아요한 레시피 ID 조회
        Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(rl -> rl.getRecipe().getId())
                .collect(Collectors.toSet());

        // 5. 각 DTO에 likedByCurrentUser 반영
        recipes.forEach(dto -> {
            if (likedRecipeIds.contains(dto.getId())) {
                dto.setLikedByCurrentUser(true);
            }
        });

        // 6. PageImpl로 다시 감싸서 반환
        return new PageImpl<>(recipes, pageable, page.getTotalElements());
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getByDishTypeWithLikeInfo(String dishTypeEnumName, Long currentUserId, Pageable pageable) {
        // enum name 기반으로 변환 ("FRY", "SOUP" 등)
        DishType dishType = DishType.valueOf(dishTypeEnumName);

        // DishType 조건에 따른 좋아요 수 포함된 Projection 조회
        Page<RecipeSimpleDto> page = recipeRepository.findByDishTypeWithLikeCount(dishType, pageable);
        List<RecipeSimpleDto> recipes = page.getContent();

        if (currentUserId == null) {
            return page;
        }

        // 1. 조회된 레시피 ID 목록 추출
        List<Long> recipeIds = recipes.stream()
                .map(RecipeSimpleDto::getId)
                .toList();

        // 2. 로그인 사용자가 좋아요 누른 레시피의 ID 목록 조회
        Set<Long> likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        // 3. 각 DTO에 좋아요 여부 적용
        recipes.forEach(dto -> {
            if (likedIds.contains(dto.getId())) {
                dto.setLikedByCurrentUser(true);
            }
        });

        // 4. PageImpl로 다시 감싸서 반환
        return new PageImpl<>(recipes, pageable, page.getTotalElements());
    }


    @Transactional(readOnly = true)
    public RecipeDetailDto getRecipeDetail(Long recipeId, Long currentUserId) {

        Recipe recipe = getRecipeWithUserOrThrow(recipeId);

        int likeCount = recipeLikeRepository.countByRecipeId(recipeId);
        boolean likedByUser = currentUserId != null &&
                recipeLikeRepository.existsByRecipeIdAndUserId(recipeId, currentUserId);
        boolean favoritedByUser = currentUserId != null &&
                recipeFavoriteRepository.existsByRecipeIdAndUserId(recipeId, currentUserId);

        RecipeRatingInfoDto ratingInfo = RecipeRatingInfoDto.builder()
                .avgRating(recipe.getAvgRating())
                .myRating(recipeRatingService.getMyRating(recipeId, currentUserId))
                .ratingCount(recipeRatingService.getRatingCount(recipeId))
                .build();

        UserDto authorDto = UserMapper.toSimpleDto(recipe.getUser());

        List<RecipeTag> recipeTags = recipeTagRepository.findByRecipeId(recipeId);
        List<String> tagNames = recipeTags.stream()
                .map(recipeTag -> recipeTag.getTag().getDisplayName())
                .toList();
        List<RecipeIngredientDto> ingredients = RecipeIngredientMapper.toDtoList(recipeIngredientRepository.findByRecipeId(recipeId));

        List<RecipeStep> steps = recipeStepRepository.findWithIngredientsByRecipeIdOrderByStepNumber(recipeId);
        List<RecipeStepDto> stepDtos = steps.stream().map(step -> {
            List<RecipeStepIngredientDto> usedIngredients = StepIngredientMapper.toDtoList(step.getStepIngredients());
            return RecipeStepMapper.toDto(step, usedIngredients);
        }).toList();

        List<CommentDto> commentDtos = commentService.getTop3CommentsWithLikes(recipeId, currentUserId);

        // ✅ 비용 계산 보정
        Integer totalCost = recipe.getTotalIngredientCost();
        Integer marketPrice = recipe.getMarketPrice();

//        if (marketPrice == null && totalCost != null) {
//            marketPrice = (int) Math.round(totalCost * 1.3); // 대충 30% 프리미엄 가정
//        }

        Integer savings = (marketPrice != null && totalCost != null)
                ? marketPrice - totalCost
                : null;

        return RecipeDetailDto.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .dishType(recipe.getDishType().getDisplayName())
                .description(recipe.getDescription())
                .cookingTime(recipe.getCookingTime())
                .ratingInfo(ratingInfo)
                .imageUrl(recipe.getImageUrl())
                .youtubeUrl(recipe.getYoutubeUrl())
                .cookingTools(recipe.getCookingTools())
                .servings(recipe.getServings())
                .isAiGenerated(recipe.isAiGenerated())
                .marketPrice(marketPrice)
                .totalIngredientCost(totalCost)
                .savings(savings)
                .author(authorDto)
                .likeCount(likeCount)
                .likedByCurrentUser(likedByUser)
                .favoriteByCurrentUser(favoritedByUser)
                .tags(tagNames)
                .ingredients(ingredients)
                .steps(stepDtos)
                .comments(commentDtos)
                .commentCount(recipeCommentRepository.countVisibleComments(recipeId))
                .createdAt(recipe.getCreatedAt())
                .updatedAt(recipe.getUpdatedAt())
                .build();
    }


    public Page<MyRecipeSummaryDto> getMyRecipes(Long userId, Pageable pageable) {
        return recipeRepository.findByUserId(userId, pageable)
                .map(recipe -> MyRecipeSummaryDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(recipe.getImageUrl())
                        .dishType(recipe.getDishType().getDisplayName())
                        .createdAt(recipe.getCreatedAt())
                        .isAiGenerated(recipe.isAiGenerated())
                        .build());
    }


    @Transactional
    public Long updateRecipe(Long recipeId, Long userId, RecipeCreateRequestDto dto) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageUrl(),
                dto.getYoutubeUrl(),
                dto.getCookingTools(),
                dto.getServings(),
                dto.getTotalIngredientCost(),
                dto.getMarketPrice()
        );

        recipeIngredientService.updateIngredients(recipe, dto.getIngredients());
        recipeStepService.updateSteps(recipe, dto.getSteps());
        recipeTagService.updateTags(recipe, dto.getTagNames());

        recipeRepository.flush();
        return recipe.getId();
    }

    @Transactional
    public Long updateUserRecipe(Long recipeId, Long userId, RecipeUserCreateRequestDto dto) {
        // 1. 레시피 조회 + 작성자 검증
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        // 2. 레시피 자체 필드 업데이트
        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageUrl(),
                null, // youtubeUrl은 유저 입력 안 함
                dto.getCookingTools(),
                dto.getServings(),
                null, // totalCost은 아래에서 계산
                null  // marketPrice도 유저가 입력 안 함
        );

        // 3. 재료/단계/태그 업데이트
        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
        recipe.setTotalIngredientCost(totalCost); // 총원가 갱신

        recipeStepService.updateSteps(recipe, dto.getSteps());
        recipeTagService.updateTags(recipe, dto.getTagNames());

        return recipe.getId();
    }



    @Transactional
    public Long deleteRecipe(Long recipeId, Long userId) {
        //a. 레시피 존재 및 삭제 권한 체크
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        // b.연관 엔티티 삭제
        // 좋아요 및 즐겨찾기 삭제
        recipeLikeRepository.deleteByRecipeId(recipeId);
        recipeFavoriteRepository.deleteByRecipeId(recipeId);

        // c.댓글 삭제
        // 1. 댓글 ID 목록 조회
        List<RecipeComment> comments = recipeCommentRepository.findByRecipeId(recipeId);
        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .toList();

        // 2. 댓글 좋아요 먼저 삭제
        if (!commentIds.isEmpty()) {
            commentLikeRepository.deleteByCommentIdIn(commentIds);
        }
       // commentLikeRepository.deleteByCommentIdIn(commentIds);

        // 3. 댓글 삭제
        recipeCommentRepository.deleteByRecipeId(recipeId);

        // d. 조리 단계에 연결된 재료 삭제
        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
        for (RecipeStep step : steps) {
            recipeStepIngredientRepository.deleteByStepId(step.getId());
        }

        // e. 조리 단계 삭제
        recipeStepRepository.deleteByRecipeId(recipeId);

        // f. 레시피 재료 삭제
        recipeIngredientRepository.deleteByRecipeId(recipeId);

        // g. 레시피 태그 삭제
        recipeTagRepository.deleteByRecipeId(recipeId);

        // h. 레시피 자체 삭제
        recipeRepository.delete(recipe);

        return recipeId;
    }

    private Recipe getRecipeOrThrow(Long recipeId) {
        return recipeRepository.findById(recipeId)
                .orElseThrow(() -> new RuntimeException("레시피가 존재하지 않습니다."));
    }

    private Recipe getRecipeWithUserOrThrow(Long recipeId) {
        return recipeRepository.findWithUserById(recipeId)
                .orElseThrow(() -> new RuntimeException("레시피를 찾을 수 없습니다."));
    }

    private User getUserOrThrow(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new RuntimeException("유저가 존재하지 않습니다."));
    }

    private void validateOwnership(Recipe recipe, Long userId) {
        if (!recipe.getUser().getId().equals(userId)) {
            throw new RecipeAccessDeniedException("권한이 없습니다.");
        }
    }

}
