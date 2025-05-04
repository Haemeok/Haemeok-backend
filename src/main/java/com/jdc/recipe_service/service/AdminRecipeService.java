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

        Integer providedMp = dto.getMarketPrice();
        int marketPrice;
        if (providedMp != null && providedMp > 0) {
            marketPrice = providedMp;
        } else if (totalCost > 0) {
            int randomPercent = PricingUtil.randomizeMarginPercent(30);
            marketPrice = PricingUtil.applyMargin(totalCost, randomPercent);
        } else {
            marketPrice = 0;
        }
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
            recipe.updateIsPrivate(dto.getIsPrivate() != null && dto.getIsPrivate()); // 기본값 false

            recipeRepository.save(recipe);

            int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients());
            recipe.updateTotalIngredientCost(totalCost);

            Integer providedMp = dto.getMarketPrice();
            int marketPrice;
            if (providedMp != null && providedMp > 0) {
                marketPrice = providedMp;
            } else if (totalCost > 0) {
                int randomPercent = PricingUtil.randomizeMarginPercent(30);
                marketPrice = PricingUtil.applyMargin(totalCost, randomPercent);
            } else {
                marketPrice = 0;
            }
            recipe.updateMarketPrice(marketPrice);

            recipeStepService.saveAll(recipe, dto.getSteps());
            recipeTagService.saveAll(recipe, dto.getTagNames());

            result.add(recipe.getId());
        }

        return result;
    }

    private List<PresignedUrlResponseItem> generatePresignedUrlsAndSaveImages(Recipe recipe, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = new ArrayList<>();
        List<RecipeImage> images = new ArrayList<>();

        for (FileInfoRequest fileInfo : files) {
            String slot = fileInfo.getType().equals("main")
                    ? "main"
                    : "step_" + fileInfo.getStepIndex();

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
    public Long deleteRecipe(Long recipeId, Long userId, Boolean isAdmin) {
        //a. 레시피 존재 및 삭제 권한 체크
        Recipe recipe = getRecipeOrThrow(recipeId);
        if (!isAdmin) {
            validateOwnership(recipe, userId);
        }

        //S3 이미지 + DB 삭제 (서비스로 위임)
        recipeImageService.deleteImagesByRecipeId(recipeId);

        // 연관 엔티티 삭제
        // 좋아요 및 즐겨찾기 삭제
        recipeLikeService.deleteByRecipeId(recipeId);
        recipeFavoriteService.deleteByRecipeId(recipeId);

        // 댓글 + 댓글 좋아요 삭제 (서비스로 위임)
        commentService.deleteAllByRecipeId(recipeId);

        // 조리 단계 + 단계 재료 삭제
        recipeStepService.deleteAllByRecipeId(recipeId);

        // 레시피 재료 삭제
        recipeIngredientService.deleteAllByRecipeId(recipeId);

        // 레시피 태그 삭제
        recipeTagService.deleteAllByRecipeId(recipeId);

        // 레시피 자체 삭제
        recipeRepository.delete(recipe);

        return recipeId;
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
