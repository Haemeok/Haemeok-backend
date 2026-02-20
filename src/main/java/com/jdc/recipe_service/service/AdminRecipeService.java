package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.report.AdminIngredientUpdateDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.ImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeMapper;
import com.jdc.recipe_service.service.image.RecipeImageService;
import com.jdc.recipe_service.util.PricingUtil;
import com.jdc.recipe_service.util.S3Util;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
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

    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeIngredientReportRepository recipeIngredientReportRepository;
    private final IngredientRepository ingredientRepository;
    private final EntityManager em;

    @Transactional
    public Long createRecipe(RecipeCreateRequestDto dto, Long userId) {
        User user = getUserOrThrow(userId);
        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);
        recipeRepository.flush();

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), RecipeSourceType.USER);
        recipe.updateTotalIngredientCost(totalCost);

        int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
        recipe.updateMarketPrice(marketPrice);

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTags());

        return recipe.getId();
    }

    @Transactional
    public PresignedUrlResponse createRecipeAndPresignedUrls(RecipeWithImageUploadRequest req, Long userId) {
        User user = getUserOrThrow(userId);
        RecipeCreateRequestDto dto = req.getRecipe();

        Recipe recipe = RecipeMapper.toEntity(dto, user);
        recipeRepository.save(recipe);
        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), RecipeSourceType.USER);
        recipe.updateTotalIngredientCost(totalCost);

        int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
        recipe.updateMarketPrice(marketPrice);

        recipeRepository.flush();

        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTags());

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

            int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), RecipeSourceType.USER);
            recipe.updateTotalIngredientCost(totalCost);

            int marketPrice = calculateMarketPrice(dto.getMarketPrice(), totalCost);
            recipe.updateMarketPrice(marketPrice);

            recipeStepService.saveAll(recipe, dto.getSteps());
            recipeTagService.saveAll(recipe, dto.getTags());

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
                new HashSet<>(dto.getCookingTools()),
                dto.getServings(),
                null,
                dto.getMarketPrice(),
                dto.getCookingTips(),
                dto.getNutrition().getProtein(),
                dto.getNutrition().getCarbohydrate(),
                dto.getNutrition().getFat(),
                dto.getNutrition().getSugar(),
                dto.getNutrition().getSodium()
        );

        int prevTotalCost = Optional.ofNullable(recipe.getTotalIngredientCost()).orElse(0);
        int newTotalCost = recipeIngredientService.updateIngredients(recipe, dto.getIngredients(), RecipeSourceType.USER);

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
        recipeTagService.updateTags(recipe, dto.getTags());

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

    /**
     * [관리자 전용] 재료 일괄 수정 (Batch Update)
     * - 기능: 추가(CREATE), 수정(UPDATE), 삭제(DELETE)
     * - 특징: 표준/커스텀 자동 판별, 신고 자동 해결, 영양소/비용 재계산
     */
    @Transactional
    public void updateIngredientsBatch(Long recipeId, List<AdminIngredientUpdateDto> dtos) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        List<RecipeIngredient> currentIngredients = recipeIngredientRepository.findByRecipeId(recipeId);

        Map<String, Ingredient> masterIngredientMap = ingredientRepository.findAll().stream()
                .collect(Collectors.toMap(i -> i.getName().trim(), Function.identity(), (p1, p2) -> p1));

        for (AdminIngredientUpdateDto dto : dtos) {
            String action = dto.getAction() != null ? dto.getAction().toUpperCase() : "UPDATE";

            RecipeIngredient target = currentIngredients.stream()
                    .filter(i -> (i.getIngredient() != null && i.getIngredient().getId().equals(dto.getId())) // 표준 재료 매칭 (19번 등)
                            || (i.getId().equals(dto.getId())))
                    .findFirst()
                    .orElse(null);

            if ("DELETE".equals(action)) {
                if (target != null) {
                    Long reportIdKey = (target.getIngredient() != null) ? target.getIngredient().getId() : target.getId();
                    resolvePendingReports(reportIdKey);
                    recipeIngredientRepository.delete(target);
                }
                continue;
            }

            Ingredient master = masterIngredientMap.get(dto.getName().trim());
            int calculatedPrice = 0;

            if (master != null) {
                double qty = parseQuantity(dto.getQuantity());
                int masterPrice = master.getPrice() != null ? master.getPrice() : 0;
                calculatedPrice = (int) (masterPrice * qty);
            } else {
                calculatedPrice = (dto.getPrice() != null) ? dto.getPrice() : 0;
            }

            if ("CREATE".equals(action)) {
                RecipeIngredient.RecipeIngredientBuilder builder = RecipeIngredient.builder()
                        .recipe(recipe)
                        .quantity(dto.getQuantity())
                        .unit(dto.getUnit())
                        .price(calculatedPrice);

                if (master != null) {
                    builder.ingredient(master);
                } else {
                    builder.customName(dto.getName());
                    builder.customUnit(dto.getUnit());
                    builder.customPrice(calculatedPrice);
                    builder.customCalorie(valOrZero(dto.getCalorie()))
                            .customCarbohydrate(valOrZero(dto.getCarbohydrate()))
                            .customProtein(valOrZero(dto.getProtein()))
                            .customFat(valOrZero(dto.getFat()))
                            .customSugar(valOrZero(dto.getSugar()))
                            .customSodium(valOrZero(dto.getSodium()));
                }
                recipeIngredientRepository.save(builder.build());

                resolvePendingReportsByName(recipeId, dto.getName());
            }
            else if ("UPDATE".equals(action)) {
                if (target != null) {
                    target.updateWithMapping(
                            dto.getName(),
                            dto.getQuantity(),
                            dto.getUnit(),
                            master,
                            calculatedPrice,
                            dto
                    );
                    Long reportIdKey = (target.getIngredient() != null) ? target.getIngredient().getId() : target.getId();
                    resolvePendingReports(reportIdKey);
                } else {
                    log.warn("[WARN] 수정을 시도했으나 레시피 {}에서 재료 ID {}를 찾을 수 없습니다.", recipeId, dto.getId());
                }
            }
        }

        recipeIngredientRepository.flush();
        em.clear();

        List<RecipeIngredient> updatedIngredients = recipeIngredientRepository.findByRecipeId(recipeId);
        calculateAndSetTotalNutrition(recipe, updatedIngredients);

        int newTotalCost = updatedIngredients.stream().mapToInt(RecipeIngredient::getPrice).sum();
        recipe.updateTotalIngredientCost(newTotalCost);
        recipe.updateMarketPrice(PricingUtil.applyMargin(newTotalCost, 30));

        recipeRepository.save(recipe);
        log.info("관리자 재료 일괄 수정 완료. RecipeID={}", recipeId);
    }


    private RecipeIngredient findById(List<RecipeIngredient> list, Long id) {
        if (id == null) return null;
        return list.stream().filter(i -> i.getId().equals(id)).findFirst().orElse(null);
    }

    private void resolvePendingReports(Long ingredientId) {
        List<RecipeIngredientReport> reports =
                recipeIngredientReportRepository.findByIngredientIdAndIsResolvedFalse(ingredientId);
        for (RecipeIngredientReport report : reports) {
            report.markAsResolved();
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

                totalCalorie = totalCalorie.add(valOrZero(ing.getCalorie()).multiply(quantity));
                totalCarb = totalCarb.add(valOrZero(ing.getCarbohydrate()).multiply(quantity));
                totalProtein = totalProtein.add(valOrZero(ing.getProtein()).multiply(quantity));
                totalFat = totalFat.add(valOrZero(ing.getFat()).multiply(quantity));
                totalSugar = totalSugar.add(valOrZero(ing.getSugar()).multiply(quantity));
                totalSodium = totalSodium.add(valOrZero(ing.getSodium()).multiply(quantity));
            } else {
                totalCalorie = totalCalorie.add(valOrZero(ri.getCustomCalorie()));
                totalCarb = totalCarb.add(valOrZero(ri.getCustomCarbohydrate()));
                totalProtein = totalProtein.add(valOrZero(ri.getCustomProtein()));
                totalFat = totalFat.add(valOrZero(ri.getCustomFat()));
                totalSugar = totalSugar.add(valOrZero(ri.getCustomSugar()));
                totalSodium = totalSodium.add(valOrZero(ri.getCustomSodium()));
            }
        }

        recipe.updateNutrition(totalProtein, totalCarb, totalFat, totalSugar, totalSodium, totalCalorie);
    }

    private void resolvePendingReportsByName(Long recipeId, String ingredientName) {
        if (ingredientName == null) return;

        List<RecipeIngredientReport> reports =
                recipeIngredientReportRepository.findByRecipeIdAndProposedNameAndIsResolvedFalse(recipeId, ingredientName.trim());

        for (RecipeIngredientReport report : reports) {
            report.markAsResolved();
        }
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

    private double parseQuantity(String quantityStr) {
        if (quantityStr == null || quantityStr.trim().isEmpty()) return 0.0;
        quantityStr = quantityStr.trim();
        if (Set.of("약간", "적당량").contains(quantityStr)) return 0.0;
        try {
            if (quantityStr.contains("/")) {
                String[] parts = quantityStr.split("/");
                if (parts.length == 2) {
                    double num = Double.parseDouble(parts[0].trim());
                    double den = Double.parseDouble(parts[1].trim());
                    if (den == 0) return 0.0;
                    return num / den;
                }
            }
            return Double.parseDouble(quantityStr.replaceAll("[^0-9.]", ""));
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }

    private BigDecimal valOrZero(BigDecimal val) {
        return val != null ? val : BigDecimal.ZERO;
    }

    private List<PresignedUrlResponseItem> generatePresignedUrlsAndSaveImages(Recipe recipe, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = new ArrayList<>();
        List<RecipeImage> images = new ArrayList<>();

        for (FileInfoRequest fileInfo : files) {
            String slot = fileInfo.getType().equals("main") ? "main" : "step_" + fileInfo.getStepIndex();
            String contentType = fileInfo.getContentType();
            if (contentType == null || contentType.isBlank()) {
                contentType = "image/jpeg";
            }

            String extension = getFileExtension(contentType);
            String fileKey = "recipes/" + recipe.getId() + "/" + slot + extension;

            String presignedUrl = s3Util.createPresignedUrl(fileKey, contentType);

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

    private String getFileExtension(String contentType) {
        if (contentType == null) return ".jpg";
        return switch (contentType.toLowerCase()) {
            case "image/webp" -> ".webp";
            case "image/png" -> ".png";
            case "image/jpeg" -> ".jpg";
            default -> ".jpg";
        };
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
