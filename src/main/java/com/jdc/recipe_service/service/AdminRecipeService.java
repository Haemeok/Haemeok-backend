package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.ImageStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeMapper;
import com.jdc.recipe_service.util.PricingUtil;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
public class AdminRecipeService {

    private final RecipeIngredientService recipeIngredientService;
    private final RecipeStepService recipeStepService;
    private final RecipeTagService recipeTagService;
    private final RecipeImageService recipeImageService;
    private final RecipeLikeService recipeLikeService;
    private final RecipeFavoriteService recipeFavoriteService;
    private final CommentService commentService;
    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;
    private final S3Util s3Util;

    @Transactional
    public Long createRecipe(RecipeCreateRequestDto dto, Long userId) {
        User user = getUserOrThrow(userId);
        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);
        recipeRepository.flush();

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
        recipe.updateTotalIngredientCost(totalCost);

        int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
        recipe.updateMarketPrice(marketPrice);

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTagNames());

        return recipe.getId();
    }

    @Transactional
    public PresignedUrlResponse createRecipeAndPresignedUrls(RecipeWithImageUploadRequest req, Long userId) {
        User user = getUserOrThrow(userId);
        RecipeCreateRequestDto dto = req.getRecipe();

        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);
        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
        recipe.updateTotalIngredientCost(totalCost);

        int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
        recipe.updateMarketPrice(marketPrice);

        recipeRepository.flush();

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTagNames());

        List<PresignedUrlResponseItem> uploads = generatePresignedUrlsAndSaveImages(recipe, req.getFiles());
        return PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(uploads)
                .build();
    }

    @Transactional
    public List<Long> createRecipesInBulk(List<RecipeCreateRequestDto> recipeDtos, Long userId) {
        List<Long> result = new ArrayList<>();
        User user = getUserOrThrow(userId);

        for (RecipeCreateRequestDto dto : recipeDtos) {
            Recipe recipe = RecipeMapper.toEntity(dto, user);
            recipe.updateIsPrivate(dto.getIsPrivate() != null && dto.getIsPrivate());
            recipeRepository.save(recipe);

            int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
            recipe.updateTotalIngredientCost(totalCost);

            int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
            recipe.updateMarketPrice(marketPrice);

            recipeStepService.saveAll(recipe, dto.getSteps());
            recipeTagService.saveAll(recipe, dto.getTagNames());

            result.add(recipe.getId());
        }
        return result;
    }

    @Transactional
    public Long updateRecipe(Long recipeId, Long userId, RecipeCreateRequestDto dto) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        if (!recipe.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }

        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageKey(),
                dto.getYoutubeUrl(),
                dto.getCookingTools(),
                dto.getServings(),
                null,
                dto.getMarketPrice()
        );

        int prevTotalCost = Optional.ofNullable(recipe.getTotalIngredientCost()).orElse(0);
        int newTotalCost = recipeIngredientService.updateIngredients(recipe, dto.getIngredients());

        if (!Objects.equals(prevTotalCost, newTotalCost)) {
            recipe.updateTotalIngredientCost(newTotalCost);

            if (dto.getMarketPrice() != null && dto.getMarketPrice() > 0) {
                recipe.updateMarketPrice(dto.getMarketPrice());
            } else {
                int margin = PricingUtil.randomizeMarginPercent(30);
                int mp = PricingUtil.applyMargin(newTotalCost, margin);
                recipe.updateMarketPrice(mp);
            }
        }

        recipeStepService.updateSteps(recipe, dto.getSteps());
        recipeTagService.updateTags(recipe, dto.getTagNames());

        recipeRepository.flush();
        return recipe.getId();
    }

    @Transactional
    public Long deleteRecipe(Long recipeId, Long userId, Boolean isAdmin) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        if (!isAdmin) {
            validateOwnership(recipe, userId);
        }

        recipeImageService.deleteImagesByRecipeId(recipeId);
        recipeLikeService.deleteByRecipeId(recipeId);
        recipeFavoriteService.deleteByRecipeId(recipeId);
        commentService.deleteAllByRecipeId(recipeId);
        recipeStepService.deleteAllByRecipeId(recipeId);
        recipeIngredientService.deleteAllByRecipeId(recipeId);
        recipeTagService.deleteAllByRecipeId(recipeId);

        recipeRepository.delete(recipe);
        return recipeId;
    }

    private List<PresignedUrlResponseItem> generatePresignedUrlsAndSaveImages(Recipe recipe, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = new ArrayList<>();
        List<RecipeImage> images = new ArrayList<>();

        for (FileInfoRequest fileInfo : files) {
            String slot = fileInfo.getType().equals("main") ? "main" : "step_" + fileInfo.getStepIndex();
            String fileKey = "recipes/" + recipe.getId() + "/" + slot + ".jpg";
            String presignedUrl = s3Util.createPresignedUrl(fileKey);

            uploads.add(PresignedUrlResponseItem.builder()
                    .fileKey(fileKey)
                    .presignedUrl(presignedUrl)
                    .build());

            images.add(RecipeImage.builder()
                    .recipe(recipe)
                    .slot(slot)
                    .fileKey(fileKey)
                    .status(ImageStatus.PENDING)
                    .build());
        }

        recipeImageService.saveAll(images);
        return uploads;
    }

    private int calculateMarketPrice(Integer providedMp, int totalCost) {
        if (providedMp != null && providedMp > 0) {
            return providedMp;
        } else if (totalCost > 0) {
            int randomPercent = PricingUtil.randomizeMarginPercent(30);
            return PricingUtil.applyMargin(totalCost, randomPercent);
        } else {
            return 0;
        }
    }

    private Recipe getRecipeOrThrow(Long recipeId) {
        return recipeRepository.findWithUserById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
    }

    private User getUserOrThrow(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
    }

    private void validateOwnership(Recipe recipe, Long userId) {
        if (!recipe.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }
    }
}
