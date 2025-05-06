package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingInfoDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeSearchService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final RecipeTagRepository recipeTagRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;

    private final RecipeRatingService recipeRatingService;
    private final CommentService commentService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }


    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getAllRecipesSimple(Long currentUserId, Pageable pageable) {
        // 1. QueryDSL 기반 projection + 페이징
        Page<RecipeSimpleDto> page = recipeRepository.findAllSimpleWithRatingAndCookingInfo(pageable);

        if (currentUserId == null) {
            return page; // 비회원은 likedByCurrentUser false 상태 그대로 반환
        }

        List<RecipeSimpleDto> content = page.getContent();

        // 2. 레시피 ID 목록 추출
        List<Long> recipeIds = content.stream()
                .map(RecipeSimpleDto::getId)
                .toList();

        // 3. 유저가 좋아요 누른 레시피 ID 조회
        Set<Long> likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        // 4. liked 상태 반영
        content.forEach(dto -> dto.setLikedByCurrentUser(likedIds.contains(dto.getId())));

        return new PageImpl<>(content, pageable, page.getTotalElements());
    }


    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchRecipes(RecipeSearchCondition condition, Pageable pageable, Long userId) {

        String title = condition.getTitle();
        DishType dishType = condition.getDishTypeEnum();
        List<TagType> tagTypes = condition.getTagEnums();

        Page<RecipeSimpleDto> page = recipeRepository.search(title, dishType, tagTypes, pageable, userId);


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
    public Page<RecipeSimpleDto> getByTagWithLikeInfo(
            String tagName, Long currentUserId, Pageable pageable) {

        TagType tag;
        try {
            tag = TagType.fromCode(tagName);
        } catch (IllegalArgumentException e) {
            throw new CustomException(ErrorCode.INVALID_TAG_NAME);
        }

        //정렬 없이 전체 조회
        Page<RecipeSimpleDto> page = recipeRepository.findByTagWithLikeCount(tag, Pageable.unpaged());
        List<RecipeSimpleDto> recipes = new ArrayList<>(page.getContent());

        if (currentUserId != null) {
            List<Long> recipeIds = recipes.stream().map(RecipeSimpleDto::getId).toList();
            Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                    .stream()
                    .map(rl -> rl.getRecipe().getId())
                    .collect(Collectors.toSet());

            recipes.forEach(dto -> {
                dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
                if (likedRecipeIds.contains(dto.getId())) {
                    dto.setLikedByCurrentUser(true);
                }
            });
        }

        // ✅ 정렬
        if (!pageable.getSort().isEmpty()) {
            Sort.Order order = pageable.getSort().iterator().next();
            if ("likeCount".equals(order.getProperty())) {
                recipes.sort(Comparator.comparing(RecipeSimpleDto::getLikeCount,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            } else {
                recipes.sort(Comparator.comparing(RecipeSimpleDto::getCreatedAt,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            }
        }

        // ✅ 페이징
        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), recipes.size());
        List<RecipeSimpleDto> pageContent = start > recipes.size() ? List.of() : recipes.subList(start, end);

        return new PageImpl<>(pageContent, pageable, recipes.size());
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getByDishTypeWithLikeInfo(
            String dishTypeCode, Long currentUserId, Pageable pageable) {

        DishType dishType = DishType.fromCode(dishTypeCode); // valueOf → fromCode

        Page<RecipeSimpleDto> page = recipeRepository.findByDishTypeWithLikeCount(dishType, Pageable.unpaged());
        List<RecipeSimpleDto> recipes = new ArrayList<>(page.getContent());

        if (currentUserId != null) {
            List<Long> recipeIds = recipes.stream().map(RecipeSimpleDto::getId).toList();
            Set<Long> likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                    .stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());

            recipes.forEach(dto -> {
                dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
                if (likedIds.contains(dto.getId())) {
                    dto.setLikedByCurrentUser(true);
                }
            });
        }

        if (!pageable.getSort().isEmpty()) {
            Sort.Order order = pageable.getSort().iterator().next();
            if ("likeCount".equals(order.getProperty())) {
                recipes.sort(Comparator.comparing(RecipeSimpleDto::getLikeCount,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            } else {
                recipes.sort(Comparator.comparing(RecipeSimpleDto::getCreatedAt,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            }
        }

        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), recipes.size());
        List<RecipeSimpleDto> pageContent = start > recipes.size() ? List.of() : recipes.subList(start, end);

        return new PageImpl<>(pageContent, pageable, recipes.size());
    }


    @Transactional(readOnly = true)
    public RecipeDetailDto getRecipeDetail(Long recipeId, Long currentUserId) {

        Recipe recipe = getRecipeWithUserOrThrow(recipeId);

        if (recipe.getIsPrivate() && (currentUserId == null || !recipe.getUser().getId().equals(currentUserId))) {
            throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
        }

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

            String stepImageUrl = generateImageUrl(step.getImageKey());
            return RecipeStepMapper.toDto(step, usedIngredients, stepImageUrl);
        }).toList();

        List<CommentDto> commentDtos = commentService.getTop3CommentsWithLikes(recipeId, currentUserId);

        // ✅ 비용 계산 보정
        int totalCost = recipe.getTotalIngredientCost() != null ? recipe.getTotalIngredientCost() : 0;
        int marketPrice = recipe.getMarketPrice() != null ? recipe.getMarketPrice() : 0;
        int savings = marketPrice - totalCost;

        return RecipeDetailDto.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .dishType(recipe.getDishType().getDisplayName())
                .description(recipe.getDescription())
                .cookingTime(recipe.getCookingTime())
                .ratingInfo(ratingInfo)
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .youtubeUrl(recipe.getYoutubeUrl())
                .cookingTools(recipe.getCookingTools())
                .servings(recipe.getServings())
                .isPrivate(recipe.getIsPrivate())
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

    private Recipe getRecipeWithUserOrThrow(Long recipeId) {
        return recipeRepository.findWithUserById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
    }

}
