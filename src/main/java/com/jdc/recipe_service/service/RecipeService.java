package com.jdc.recipe_service.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.*;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.*;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.util.PricingUtil;
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
    private final AsyncImageService asyncImageService;


    @Transactional
    public PresignedUrlResponse createUserRecipeAndGenerateUrls(RecipeWithImageUploadRequest req, Long userId, RecipeSourceType sourceType) {
        User user = getUserOrThrow(userId);
        RecipeCreateRequestDto dto = req.getRecipe();

        if (dto == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 생성 요청 데이터(dto)가 null입니다.");
        }

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
            recipe.updateImageStatus(RecipeImageStatus.PENDING);
        } else {
            if (dto.getIsPrivate() == null) {
                recipe.updateIsPrivate(false);
            } else {
                recipe.updateIsPrivate(dto.getIsPrivate());
            }
        }
        recipeRepository.save(recipe);

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), sourceType);
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
        }
        recipeTagService.saveAll(recipe, dto.getTagNames());

        em.flush();
        em.clear();

        Long id = recipe.getId();

        Recipe full = recipeRepository.findWithAllRelationsById(id)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        recipeIndexingService.indexRecipe(full);

        List<PresignedUrlResponseItem> uploads = Collections.emptyList();
        if (req.getFiles() != null && !req.getFiles().isEmpty()) {
            uploads = recipeImageService.generateAndSavePresignedUrls(recipe, req.getFiles());
        }

        if (sourceType == RecipeSourceType.AI) {
            asyncImageService.generateAndUploadAiImageAsync(recipe.getId());
        }

        return PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(uploads)
                .build();
    }

    @Transactional
    public RecipeWithImageUploadRequest buildRecipeFromAiRequest(String prompt, AiRecipeRequestDto aiReq, List<FileInfoRequest> originalFiles) {
        String jsonFromAI = null;
        try {
            jsonFromAI = replicateService.generateRecipeJsonWithRetry(prompt);

            System.out.println(">>>>>> JSON 문자열 수신 (deserialization 직전): [\n" + jsonFromAI + "\n]");
            if (jsonFromAI == null || jsonFromAI.trim().isEmpty()) {
                System.err.println(">>>>>> CRITICAL: ReplicateService가 null 또는 빈 JSON 문자열을 반환했습니다.");
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI로부터 유효한 JSON 응답을 받지 못했습니다 (결과가 비어있음).");
            }

            RecipeCreateRequestDto generatedDto = objectMapper.readValue(jsonFromAI, RecipeCreateRequestDto.class);

            String dtoToString = "!!! DTO IS NULL !!!";
            if (generatedDto != null) {
                dtoToString = generatedDto.toString();
                if (dtoToString.length() > 500) {
                    dtoToString = dtoToString.substring(0, 500) + "...";
                }
            }
            System.out.println(">>>>>> Deserialized RecipeCreateRequestDto: " + dtoToString);


            if (generatedDto == null) {
                System.err.println(">>>>>> CRITICAL: RecipeCreateRequestDto가 objectMapper.readValue 후 null입니다! 원본 JSON: [\n" + jsonFromAI + "\n]");
                String snippetOnError = (jsonFromAI.length() > 500 ? jsonFromAI.substring(0, 500) + "..." : jsonFromAI);
                throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 레시피 DTO 변환 결과가 null입니다. 원본 JSON: " + snippetOnError);
            }

            RecipeWithImageUploadRequest result = new RecipeWithImageUploadRequest();
            result.setAiRequest(aiReq);
            result.setRecipe(generatedDto);
            result.setFiles(originalFiles);
            return result;

        } catch (JsonProcessingException jsonEx) {
            System.err.println(">>>>>> AI JSON 파싱 실패! (JsonProcessingException). 시도된 JSON 문자열: [\n" + jsonFromAI + "\n]");
            jsonEx.printStackTrace();
            String snippet = (jsonFromAI != null && jsonFromAI.length() > 200) ? jsonFromAI.substring(0, 200) + "..." : jsonFromAI;
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI JSON 파싱 실패: " + jsonEx.getMessage() + ". 시도된 JSON (일부): " + snippet, jsonEx);
        } catch (CustomException ce) {
            System.err.println(">>>>>> AI 레시피 생성 중 CustomException 발생: " + ce.getMessage());
            throw ce;
        } catch (Exception e) {
            System.err.println(">>>>>> AI 레시피 생성 중 예상치 못한 오류 발생. 마지막으로 시도된 JSON 문자열: [\n" + jsonFromAI + "\n]");
            e.printStackTrace();
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 레시피 생성 실패 (일반 오류): " + e.getMessage(), e);
        }
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
