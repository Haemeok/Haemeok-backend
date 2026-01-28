package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.*;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.event.AiRecipeCreatedEvent;
import com.jdc.recipe_service.event.UserRecipeCreatedEvent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.*;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.ai.RecipeAnalysisService;
import com.jdc.recipe_service.service.image.RecipeImageService;
import com.jdc.recipe_service.util.*;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeService {

    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeRatingRepository recipeRatingRepository;

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
    private final ApplicationEventPublisher publisher;
    private final RecipeAnalysisService recipeAnalysisService;
    private final RecipeActivityService recipeActivityService;

    private static final String MAIN_IMAGE_SLOT = "main";
    private static final String STEP_IMAGE_SLOT_PREFIX = "step_";
    private static final int DEFAULT_MARGIN_PERCENT = 30;

    @Transactional
    public PresignedUrlResponse createRecipeAndGenerateUrls(
            RecipeWithImageUploadRequest req,
            Long userId,
            RecipeSourceType sourceType,
            AiRecipeConcept aiConcept
    ) {
        User user = getUserOrThrow(userId);
        RecipeCreateRequestDto dto = Optional.ofNullable(req.getRecipe())
                .orElseThrow(() -> new CustomException(
                        ErrorCode.INVALID_INPUT_VALUE,
                        "레시피 생성 요청 데이터(dto)가 null입니다."
                ));

        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipe.updateAiGenerated(sourceType == RecipeSourceType.AI);

        if (sourceType == RecipeSourceType.YOUTUBE) {
            recipe.updateYoutubeInfo(
                    dto.getYoutubeChannelName(),
                    dto.getYoutubeChannelId(),
                    dto.getYoutubeVideoTitle(),
                    dto.getYoutubeThumbnailUrl(),
                    dto.getYoutubeChannelProfileUrl(),
                    dto.getYoutubeSubscriberCount(),
                    dto.getYoutubeVideoViewCount()
            );
        }

        if (sourceType == RecipeSourceType.AI || sourceType == RecipeSourceType.YOUTUBE) {
            recipe.updateIsPrivate(false);
            if (dto.getImageStatus() != null) {
                recipe.updateImageStatus(dto.getImageStatus());
            } else {
                recipe.updateImageStatus(RecipeImageStatus.PENDING);
            }
        } else {
            boolean hasMain = req.getFiles() != null &&
                    req.getFiles().stream().anyMatch(f -> MAIN_IMAGE_SLOT.equals(f.getType()));
            if (!hasMain) {
                throw new CustomException(ErrorCode.USER_RECIPE_IMAGE_REQUIRED);
            }
            recipe.updateIsPrivate(dto.getIsPrivate() != null && dto.getIsPrivate());
        }
        recipeRepository.save(recipe);

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), sourceType);
        recipe.updateTotalIngredientCost(totalCost);

        List<RecipeIngredient> savedIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        calculateAndSetTotalNutrition(recipe, savedIngredients);

        int marketPrice = calculateMarketPrice(dto, totalCost);
        recipe.updateMarketPrice(marketPrice);

        if ((sourceType == RecipeSourceType.AI || sourceType == RecipeSourceType.YOUTUBE)
                && marketPrice < totalCost) {

            marketPrice = PricingUtil.applyMargin(totalCost, 30);
            recipe.updateMarketPrice(marketPrice);

            log.info("⚠️ 시장가(MarketPrice)가 원가보다 낮아 강제 조정됨. ID={}, Cost={}, NewPrice={}",
                    recipe.getId(), totalCost, marketPrice);
        }

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTags());

        if (dto.getComponents() != null && !dto.getComponents().isEmpty()) {
            List<SimpleComponentDto> filteredComponents = dto.getComponents().stream()
                    .map(c -> new SimpleComponentDto(
                            c.getRole(),
                            c.getName(),
                            c.getDescription(),
                            c.getProcess()
                    ))
                    .toList();

            var plating = dto.getPlating();
            FineDiningDetails details = FineDiningDetails.builder()
                    .components(filteredComponents)
                    .platingVessel(plating != null ? plating.getVessel() : null)
                    .platingGuide(plating != null ? plating.getGuide() : null)
                    .visualKeys(plating != null ? plating.getVisualKeys() : null)
                    .viewpoint(plating != null ? plating.getViewpoint() : null)
                    .lighting(plating != null ? plating.getLighting() : null)
                    .build();

            details.setRecipe(recipe);
            recipe.setFineDiningDetails(details);
        }

        em.flush();
        em.clear();


        Recipe full = recipeRepository.findWithAllRelationsById(recipe.getId())
                .orElseThrow(() -> new CustomException(
                        ErrorCode.RECIPE_NOT_FOUND, "생성된 레시피를 조회할 수 없습니다.")
                );

        final List<PresignedUrlResponseItem> uploads =
                (req.getFiles() != null && !req.getFiles().isEmpty())
                        ? recipeImageService.generateAndSavePresignedUrls(recipe, req.getFiles())
                        : Collections.emptyList();

        final Long recipeId = recipe.getId();
        final Long targetUserId = recipe.getUser().getId();
        final String targetUserNickname = user.getNickname();

        if (sourceType == RecipeSourceType.AI && aiConcept != null) {
            recipeActivityService.saveLog(
                    targetUserId,
                    targetUserNickname,
                    ActivityLogType.fromConcept(aiConcept)
            );
        }

        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        log.info("레시피 생성 커밋 완료. OpenSearch 색인 요청: ID={}", recipeId);
                        recipeIndexingService.indexRecipeSafelyWithRetry(recipeId);
                        if (sourceType == RecipeSourceType.AI || sourceType == RecipeSourceType.YOUTUBE) {
                            publisher.publishEvent(new AiRecipeCreatedEvent(recipeId, targetUserId));
                        } else {
                            publisher.publishEvent(new UserRecipeCreatedEvent(recipeId));
                        }
                    }
                }
        );

        return PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(uploads)
                .build();
    }

    @Transactional
    public PresignedUrlResponse updateUserRecipe(Long recipeId, Long userId, RecipeUpdateWithImageRequest req) {
        RecipeUpdateRequestDto dto = req.getRecipe();

        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        Set<String> tools = new HashSet<>(Optional.ofNullable(dto.getCookingTools()).orElse(Collections.emptyList()));

        RecipeNutritionDto nutritionDto = dto.getNutrition();

        BigDecimal protein = resolve(nutritionDto, RecipeNutritionDto::getProtein, recipe.getProtein());
        BigDecimal carbohydrate = resolve(nutritionDto, RecipeNutritionDto::getCarbohydrate, recipe.getCarbohydrate());
        BigDecimal fat = resolve(nutritionDto, RecipeNutritionDto::getFat, recipe.getFat());
        BigDecimal sugar = resolve(nutritionDto, RecipeNutritionDto::getSugar, recipe.getSugar());
        BigDecimal sodium = resolve(nutritionDto, RecipeNutritionDto::getSodium, recipe.getSodium());

        Integer newTotalIngredientCost = (dto.getTotalIngredientCost() != null) ? dto.getTotalIngredientCost() : recipe.getTotalIngredientCost();

        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageKey(),
                dto.getYoutubeUrl(),
                tools,
                dto.getServings(),
                newTotalIngredientCost,
                (dto.getMarketPrice() != null && dto.getMarketPrice() > 0) ? dto.getMarketPrice() : recipe.getMarketPrice(),
                (dto.getCookingTips() != null && !dto.getCookingTips().isBlank()) ? dto.getCookingTips() : recipe.getCookingTips(),
                protein,
                carbohydrate,
                fat,
                sugar,
                sodium
        );

        int prevTotalCost = Optional.ofNullable(recipe.getTotalIngredientCost()).orElse(0);
        int newTotalCost = prevTotalCost;

        if (Boolean.TRUE.equals(dto.getIsIngredientsModified())) {
            newTotalCost = recipeIngredientService.updateIngredientsFromUser(recipe, dto.getIngredients());
            recipe.updateTotalIngredientCost(newTotalCost);

            List<RecipeIngredient> currentIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
            calculateAndSetTotalNutrition(recipe, currentIngredients);

            int newMarketPrice = PricingUtil.applyMargin(newTotalCost, DEFAULT_MARGIN_PERCENT);
            recipe.updateMarketPrice(newMarketPrice);
        }

        else if (!Objects.equals(newTotalCost, prevTotalCost)) {
            int marketPrice = calculateMarketPriceForUpdate(dto, newTotalCost);
            recipe.updateMarketPrice(marketPrice);
        }

        recipeStepService.updateStepsFromUser(recipe, dto.getSteps());
        recipeTagService.updateTags(recipe, dto.getTags());

        recipeRepository.save(recipe);

        em.flush();
        em.clear();

        if (Boolean.TRUE.equals(dto.getIsIngredientsModified())) {
            TransactionSynchronizationManager.registerSynchronization(
                    new TransactionSynchronization() {
                        @Override
                        public void afterCommit() {
                            log.info("DB 커밋 완료. 수정된 내용으로 AI 분석 시작. ID: {}", recipe.getId());
                            recipeAnalysisService.analyzeRecipeAsync(recipe.getId());
                        }
                    });
        }

        Recipe full = recipeRepository.findWithAllRelationsById(recipe.getId())
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        log.info("레시피 수정 커밋 완료. OpenSearch 색인 요청. ID: {}", recipe.getId());

                        recipeIndexingService.indexRecipeSafelyWithRetry(full.getId());

                        if (Boolean.TRUE.equals(dto.getIsIngredientsModified())) {
                            recipeAnalysisService.analyzeRecipeAsync(recipe.getId());
                        }
                    }
                });

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

        recipeRatingRepository.deleteByRecipeId(recipeId);

        recipeStepService.deleteAllByRecipeId(recipeId);

        recipeIngredientService.deleteAllByRecipeId(recipeId);

        recipeTagService.deleteAllByRecipeId(recipeId);

        recipeRepository.deleteByIdDirectly(recipeId);

        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        recipeIndexingService.deleteRecipeSafelyWithRetry(recipeId);
                    }
                });

        return recipeId;
    }

    @Transactional
    public void recalculateNutrition(Long recipeId) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        List<RecipeIngredient> ingredients = recipeIngredientRepository.findByRecipeId(recipeId);

        calculateAndSetTotalNutrition(recipe, ingredients);

        recipeRepository.save(recipe);

        log.info("레시피 영양소 재계산 완료. ID: {}", recipeId);
    }

    @Transactional
    public FinalizeResponse finalizeRecipeImages(Long recipeId, Long callerUserId, boolean isAdmin) {
        Recipe recipe = recipeRepository.findWithStepsById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (!isAdmin && !recipe.getUser().getId().equals(callerUserId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }

        List<RecipeImage> images = recipeImageService.getImagesByRecipeId(recipeId);
        String folderPrefix = "images/recipes/" + recipeId + "/";
        Set<String> existingKeysInS3 = s3Util.listKeysInFolder(folderPrefix);
        Set<String> activeImages = new LinkedHashSet<>();
        List<String> missingFiles = new ArrayList<>();
        boolean hasMainImageUploaded = false;

        for (RecipeImage image : images) {

            boolean exists = existingKeysInS3.contains(image.getFileKey());

            if (!exists) {
                recipeImageService.deleteByFileKey(image.getFileKey());
                missingFiles.add(image.getFileKey());
                continue;
            }

            String slot = image.getSlot();
            if (MAIN_IMAGE_SLOT.equals(slot)) {
                recipe.updateImageKey(image.getFileKey());
                hasMainImageUploaded = true;
            } else if (slot != null && slot.startsWith(STEP_IMAGE_SLOT_PREFIX)) {
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

        if (!recipe.isAiGenerated()) {
            if (!hasMainImageUploaded) {
                recipe.updateIsPrivate(true);
                log.warn("사용자 레시피 {} finalize 실패: 메인 이미지 누락. 강제 비공개 처리.", recipeId);
            } else {
                recipe.updateIsPrivate(false);
            }
        }

        em.flush();
        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        log.info("이미지 확정(Finalize) 완료. OpenSearch 색인 업데이트. ID: {}", recipeId);
                        recipeIndexingService.indexRecipeSafelyWithRetry(recipeId);
                    }
                }
        );
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
    public boolean togglePrivacy(Long recipeId, Long userId) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        boolean newValue = !recipe.getIsPrivate();

        if (recipe.isAiGenerated() && !newValue && recipe.getImageKey() == null) {
            throw new CustomException(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE);
        }

        recipe.updateIsPrivate(newValue);

        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        recipeIndexingService.updatePrivacyStatusSafely(recipeId, newValue);
                    }
                });
        return newValue;
    }

    private static int calculateMarketPrice(RecipeCreateRequestDto dto, int totalCost) {
        Integer providedMp = dto.getMarketPrice();
        int marketPrice = (providedMp != null && providedMp > 0)
                ? providedMp
                : (totalCost > 0
                ? PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT))
                : 0);
        return marketPrice;
    }

    private static int calculateMarketPriceForUpdate(RecipeUpdateRequestDto dto, int totalCost) {
        Integer providedMp = dto.getMarketPrice();
        int marketPrice = (providedMp != null && providedMp > 0)
                ? providedMp
                : (totalCost > 0
                ? PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT))
                : 0);
        return marketPrice;
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

    private void calculateAndSetTotalNutrition(Recipe recipe, List<RecipeIngredient> ingredients) {
        BigDecimal totalCalorie = BigDecimal.ZERO;
        BigDecimal totalCarb = BigDecimal.ZERO;
        BigDecimal totalProtein = BigDecimal.ZERO;
        BigDecimal totalFat = BigDecimal.ZERO;
        BigDecimal totalSugar = BigDecimal.ZERO;
        BigDecimal totalSodium = BigDecimal.ZERO;

        for (RecipeIngredient ri : ingredients) {
            BigDecimal quantity = parseQuantityToBigDecimal(ri.getQuantity());

            if (ri.getIngredient() != null) {
                Ingredient ing = ri.getIngredient();

                totalCalorie = totalCalorie.add(ing.getCalorie().multiply(quantity));
                totalCarb = totalCarb.add(ing.getCarbohydrate().multiply(quantity));
                totalProtein = totalProtein.add(ing.getProtein().multiply(quantity));
                totalFat = totalFat.add(ing.getFat().multiply(quantity));
                totalSugar = totalSugar.add(ing.getSugar().multiply(quantity));
                totalSodium = totalSodium.add(ing.getSodium().multiply(quantity));
            } else {
                totalCalorie = totalCalorie.add(ri.getCustomCalorie());
                totalCarb = totalCarb.add(ri.getCustomCarbohydrate());
                totalProtein = totalProtein.add(ri.getCustomProtein());
                totalFat = totalFat.add(ri.getCustomFat());
                totalSugar = totalSugar.add(ri.getCustomSugar());
                totalSodium = totalSodium.add(ri.getCustomSodium());
            }
        }

        recipe.updateNutrition(totalProtein, totalCarb, totalFat, totalSugar, totalSodium, totalCalorie);
    }

    private BigDecimal parseQuantityToBigDecimal(String quantityStr) {
        if (quantityStr == null || quantityStr.isBlank()) return BigDecimal.ZERO;

        String cleanStr = quantityStr.replaceAll("[^0-9./]", "");

        try {
            if (cleanStr.contains("/")) {
                String[] parts = cleanStr.split("/");
                if (parts.length == 2) {
                    double num = Double.parseDouble(parts[0]);
                    double den = Double.parseDouble(parts[1]);
                    if (den == 0) return BigDecimal.ZERO;
                    return BigDecimal.valueOf(num / den);
                }
            }
            return new BigDecimal(cleanStr);
        } catch (Exception e) {
            return BigDecimal.ZERO;
        }
    }

    private <T> T resolve(RecipeNutritionDto dto, Function<RecipeNutritionDto, T> getter, T currentValue) {
        if (dto == null) return currentValue;
        T newValue = getter.apply(dto);
        return (newValue != null) ? newValue : currentValue;
    }
}