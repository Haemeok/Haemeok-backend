package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.*;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.*;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.util.PricingUtil;
import com.jdc.recipe_service.util.PromptBuilder;
import com.jdc.recipe_service.util.S3Util;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


@Service
@RequiredArgsConstructor
public class RecipeService {

    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;

    private final RecipeIngredientService recipeIngredientService;
    private final RecipeStepService recipeStepService;
    private final RecipeTagService recipeTagService;
    private final RecipeFavoriteService recipeFavoriteService;
    private final CommentService commentService;
    private final RecipeImageService recipeImageService;
    private final RecipeLikeService recipeLikeService;
    private final RecipeIndexingService recipeIndexingService;
    private final S3Util s3Util;
    private final EntityManager em;
    private final ReplicateService replicateService;
    private final ObjectMapper objectMapper;

    @Transactional
    public PresignedUrlResponse createUserRecipeAndGenerateUrls(RecipeWithImageUploadRequest req, Long userId, RecipeSourceType sourceType) {
        User user = getUserOrThrow(userId);
        RecipeCreateRequestDto dto = req.getRecipe();

        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipe.updateAiGenerated(sourceType == RecipeSourceType.AI);

        if (sourceType != RecipeSourceType.AI) {
            boolean hasMainImage = req.getFiles() != null &&
                    req.getFiles().stream().anyMatch(f -> "main".equals(f.getType()));
            if (!hasMainImage) {
                throw new CustomException(ErrorCode.USER_RECIPE_IMAGE_REQUIRED);
            }
        }

        if (sourceType == RecipeSourceType.AI) {
            recipe.updateIsPrivate(true);
        } else {
            recipe.updateIsPrivate(dto.getIsPrivate() != null && dto.getIsPrivate());
        }
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

        recipeRepository.flush();

        if (sourceType == RecipeSourceType.AI) {
            recipeStepService.saveAll(recipe, dto.getSteps());
        } else {
            recipeStepService.saveAllFromUser(recipe, dto.getSteps());
        }        recipeTagService.saveAll(recipe, dto.getTagNames());

        em.flush();
        em.clear();

        Long id = recipe.getId();

        Recipe full = recipeRepository.findWithAllRelationsById(id)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        recipeIndexingService.indexRecipe(full);

        List<PresignedUrlResponseItem> uploads = recipeImageService.generateAndSavePresignedUrls(recipe, req.getFiles());
        return PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(uploads)
                .build();
    }


    @Transactional
    public PresignedUrlResponse updateUserRecipe(Long recipeId, Long userId, RecipeWithImageUploadRequest req) {
        RecipeCreateRequestDto dto = req.getRecipe();

        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageKey(),
                null,
                dto.getCookingTools(),
                dto.getServings(),
                null,
                dto.getMarketPrice()
        );

        int prevTotalCost = Optional.ofNullable(recipe.getTotalIngredientCost()).orElse(0);
        int newTotalCost = recipeIngredientService.updateIngredientsFromUser(recipe, dto.getIngredients());

        if (!Objects.equals(newTotalCost, prevTotalCost)) {
            recipe.updateTotalIngredientCost(newTotalCost);

            if (dto.getMarketPrice() != null && dto.getMarketPrice() > 0) {
                recipe.updateMarketPrice(dto.getMarketPrice());
            } else {
                int margin = PricingUtil.randomizeMarginPercent(30);
                int mp = PricingUtil.applyMargin(newTotalCost, margin);
                recipe.updateMarketPrice(mp);
            }
        }

        recipeStepService.updateStepsFromUser(recipe, dto.getSteps());
        recipeTagService.updateTags(recipe, dto.getTagNames());

        em.flush();
        em.clear();

        Recipe full = recipeRepository.findWithAllRelationsById(recipe.getId())
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        recipeIndexingService.updateRecipe(full);

        List<FileInfoRequest> files = req.getFiles();
        List<PresignedUrlResponseItem> uploads = Collections.emptyList();

        if (files != null && !files.isEmpty()) {
            uploads = recipeImageService.generateAndSavePresignedUrls(full, files);
        }

        return PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(uploads)
                .build();
    }

    @Transactional
    public Long deleteRecipe(Long recipeId, Long userId) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        recipeImageService.deleteImagesByRecipeId(recipeId);

        recipeLikeService.deleteByRecipeId(recipeId);
        recipeFavoriteService.deleteByRecipeId(recipeId);

        commentService.deleteAllByRecipeId(recipeId);

        recipeStepService.deleteAllByRecipeId(recipeId);

        recipeIngredientService.deleteAllByRecipeId(recipeId);

        recipeTagService.deleteAllByRecipeId(recipeId);

        recipeRepository.delete(recipe);

        recipeIndexingService.deleteRecipe(recipeId);

        return recipeId;
    }

    @Transactional
    public FinalizeResponse finalizeRecipeImages(Long recipeId, Long callerUserId, boolean isAdmin) {
        Recipe recipe = recipeRepository.findWithStepsById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (!isAdmin && !recipe.getUser().getId().equals(callerUserId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }

        List<RecipeImage> images = recipeImageService.getImagesByRecipeId(recipeId);
        Set<String> activeImages = new LinkedHashSet<>();
        List<String> missingFiles = new ArrayList<>();
        boolean hasMainImageUploaded = false;

        for (RecipeImage image : images) {
            boolean exists = s3Util.doesObjectExist(image.getFileKey());

            if (!exists) {
                recipeImageService.deleteByFileKey(image.getFileKey());
                missingFiles.add(image.getFileKey());
                continue;
            }

            String slot = image.getSlot();
            if ("main".equals(slot)) {
                recipe.updateImageKey(image.getFileKey());
                hasMainImageUploaded = true;
            } else if (slot != null && slot.startsWith("step_")) {
                int stepIndex;
                try {
                    stepIndex = Integer.parseInt(slot.split("_")[1]);
                } catch (NumberFormatException e) {
                    continue;
                }
                recipe.getSteps().stream()
                        .filter(step -> step.getStepNumber() == stepIndex)
                        .findFirst()
                        .ifPresent(step -> step.updateStepImageKey(image.getFileKey()));
            }

            image.updateStatusToActive();
            activeImages.add(image.getFileKey());
        }

        if (missingFiles.isEmpty() && !recipe.isAiGenerated() && hasMainImageUploaded) {
            recipe.updateIsPrivate(false);
        }

        em.flush();
        return new FinalizeResponse(recipeId, new ArrayList<>(activeImages), missingFiles);
    }



    @Transactional
    public void updateImageKeys(Long recipeId, Long userId, RecipeImageKeyUpdateRequest request) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        if (!recipe.isAiGenerated() && (request.getImageKey() == null || request.getImageKey().isBlank())) {
            throw new CustomException(ErrorCode.USER_RECIPE_IMAGE_REQUIRED);
        }

        recipe.updateImageKey(request.getImageKey());

        List<RecipeStep> steps = recipeStepService.getStepsByRecipeId(recipeId);
        Map<Integer, String> imageKeyMap = IntStream.range(0, request.getStepImageKeys().size())
                .boxed()
                .collect(Collectors.toMap(i -> i, i -> request.getStepImageKeys().get(i)));

        for (RecipeStep step : steps) {
            if (imageKeyMap.containsKey(step.getStepNumber())) {
                step.updateStepImageKey(imageKeyMap.get(step.getStepNumber()));
            }
        }
    }

    @Transactional
    public RecipeWithImageUploadRequest buildRecipeFromAiRequest(RecipeWithImageUploadRequest req) {
        AiRecipeRequestDto aiReq = req.getAiRequest();
        if (aiReq == null) throw new CustomException(ErrorCode.NULL_POINTER, "AI 요청 필드 누락");

        try {
            String prompt = PromptBuilder.buildPrompt(aiReq);
            String json = replicateService.generateRecipeJson(prompt);
            RecipeCreateRequestDto generated = objectMapper.readValue(json, RecipeCreateRequestDto.class);

            req.setRecipe(generated);
            req.setFiles(Collections.emptyList());
            return req;
        } catch (com.fasterxml.jackson.core.JsonProcessingException jsonEx) {
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI JSON 파싱 실패: " + jsonEx.getMessage());
        } catch (Exception e) {
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 레시피 생성 실패: " + e.getMessage());
        }
    }


    @Transactional
    public boolean togglePrivacy(Long recipeId, Long userId) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        boolean newValue = !recipe.getIsPrivate();

        if (recipe.isAiGenerated() && !newValue && recipe.getImageKey() == null) {
            throw new CustomException(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE);
        }

        recipe.updateIsPrivate(newValue);
        return newValue;
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
