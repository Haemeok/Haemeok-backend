package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class FridgeRecipeService {

    private final RefrigeratorItemRepository fridgeRepo;
    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Transactional(readOnly = true)
    public Slice<FridgeRecipeDto> searchByFridge(Long userId, Pageable pageable, List<RecipeType> types) {

        List<RefrigeratorItem> myItems = fridgeRepo.findAllByUserId(userId);

        if (myItems.isEmpty()) {
            return new SliceImpl<>(List.of(), pageable, false);
        }

        Set<Long> myIngredientIds = myItems.stream()
                .map(item -> item.getIngredient().getId())
                .collect(Collectors.toSet());

        Set<String> myIngredientNames = myItems.stream()
                .map(item -> item.getIngredient().getName())
                .filter(Objects::nonNull)
                .map(this::normalizeName)
                .collect(Collectors.toSet());

        List<Long> searchIds = new ArrayList<>(myIngredientIds);

        Slice<Recipe> recipeSlice = recipeRepository.searchRecipesByFridgeIngredients(
                searchIds, types, pageable
        );

        if (recipeSlice.isEmpty()) {
            return new SliceImpl<>(List.of(), pageable, false);
        }

        List<Long> recipeIds = recipeSlice.getContent().stream()
                .map(Recipe::getId)
                .toList();

        Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        List<FridgeRecipeDto> dtos = recipeSlice.getContent().stream()
                .map(recipe -> convertToDto(recipe, myIngredientIds, myIngredientNames, likedRecipeIds))
                .toList();

        return new SliceImpl<>(dtos, pageable, recipeSlice.hasNext());
    }

    /**
     * DTO 변환 로직 분리 (가독성 및 유지보수성 향상)
     */
    private FridgeRecipeDto convertToDto(Recipe recipe, Set<Long> myIngredientIds, Set<String> myIngredientNames, Set<Long> likedRecipeIds) {
        List<String> matchedIngredients = new ArrayList<>();
        List<FridgeRecipeDto.MissingIngredientDto> missingIngredients = new ArrayList<>();

        for (RecipeIngredient ri : recipe.getIngredients()) {
            Ingredient ing = ri.getIngredient();

            if (ing != null && ing.isPantry()) {
                continue;
            }

            boolean isMatched = false;
            String displayName;
            String displayLink = null;

            if (ing != null) {
                displayName = ing.getName();
                if (myIngredientIds.contains(ing.getId())) {
                    isMatched = true;
                }
                else {
                    if (myIngredientNames.contains(normalizeName(ing.getName()))) {
                        isMatched = true;
                    }
                }

                if (!isMatched) {
                    displayLink = (ri.getCustomLink() != null && !ri.getCustomLink().isEmpty())
                            ? ri.getCustomLink() : ing.getCoupangLink();
                }
            } else {
                displayName = ri.getCustomName();
                if (displayName == null) continue;

                if (myIngredientNames.contains(normalizeName(displayName))) {
                    isMatched = true;
                }

                if (!isMatched) {
                    displayLink = ri.getCustomLink();
                }
            }

            if (isMatched) {
                matchedIngredients.add(displayName);
            } else {
                missingIngredients.add(new FridgeRecipeDto.MissingIngredientDto(displayName, displayLink));
            }
        }

        boolean isYoutube = recipe.getYoutubeUrl() != null && !recipe.getYoutubeUrl().isEmpty();

        RecipeSimpleDto simpleDto = RecipeSimpleDto.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .authorId(recipe.getUser().getId())
                .authorName(recipe.getUser().getNickname())
                .profileImage(recipe.getUser().getProfileImage())
                .createdAt(recipe.getCreatedAt())
                .favoriteCount(recipe.getFavoriteCount())
                .likeCount(recipe.getLikeCount())
                .likedByCurrentUser(likedRecipeIds.contains(recipe.getId()))
                .cookingTime(recipe.getCookingTime())
                .youtubeChannelName(recipe.getYoutubeChannelName())
                .youtubeChannelId(recipe.getYoutubeChannelId())
                .youtubeVideoTitle(recipe.getYoutubeVideoTitle())
                .youtubeThumbnailUrl(recipe.getYoutubeThumbnailUrl())
                .youtubeChannelProfileUrl(recipe.getYoutubeChannelProfileUrl())
                .youtubeSubscriberCount(recipe.getYoutubeSubscriberCount())
                .youtubeVideoViewCount(recipe.getYoutubeVideoViewCount())
                .avgRating(recipe.getAvgRating())
                .ratingCount(recipe.getRatingCount())
                .isYoutube(isYoutube)
                .isAiGenerated(recipe.isAiGenerated())
                .build();

        return new FridgeRecipeDto(simpleDto, matchedIngredients, missingIngredients);
    }

    private String normalizeName(String name) {
        if (name == null) return "";
        return name.trim().replace(" ", "");
    }
}