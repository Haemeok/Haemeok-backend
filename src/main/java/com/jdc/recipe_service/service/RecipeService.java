package com.jdc.recipe_service.service;

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

        // 중간에 강제 반영 & 캐시 초기화
        em.flush();  // 지금까지 변경된 INSERT/UPDATE SQL을 모두 DB에 전송
        em.clear();  // 1차 캐시(영속성 컨텍스트)를 비워서, 이후 find 호출 시 DB에서 다시 로드

        // 저장된 recipeId
        Long id = recipe.getId();

        // 페치조인으로 연관관계까지 모두 로드
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
    public Long updateUserRecipe(Long recipeId, Long userId, RecipeCreateRequestDto dto) {
        // 1. 레시피 조회 + 작성자 검증
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

        // 2. 레시피 자체 필드 업데이트
        recipe.update(
                dto.getTitle(),
                dto.getDescription(),
                DishType.fromDisplayName(dto.getDishType()),
                dto.getCookingTime(),
                dto.getImageKey(),
                null, // youtubeUrl은 유저 입력 안 함
                dto.getCookingTools(),
                dto.getServings(),
                null, // totalCost은 아래에서 계산
                dto.getMarketPrice()
        );

        // 3. 재료/단계/태그 업데이트
        int prevTotalCost = Optional.ofNullable(recipe.getTotalIngredientCost()).orElse(0);

        // 3. 재료/단계/태그 업데이트
        int newTotalCost = recipeIngredientService.updateIngredientsFromUser(recipe, dto.getIngredients());

        // 🔄 원가 바뀐 경우만 marketPrice도 갱신
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

        em.flush();  // 지금까지 변경된 INSERT/UPDATE SQL을 모두 DB에 전송
        em.clear();  // 1차 캐시(영속성 컨텍스트)를 비워서, 이후 find 호출 시 DB에서 다시 로드

        Recipe full = recipeRepository.findWithAllRelationsById(recipe.getId())
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        recipeIndexingService.updateRecipe(full);

        return recipe.getId();
    }

    @Transactional
    public Long deleteRecipe(Long recipeId, Long userId) {
        //a. 레시피 존재 및 삭제 권한 체크
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId);

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

        recipeIndexingService.deleteRecipe(recipeId);

        return recipeId;
    }

    @Transactional
    public FinalizeResponse finalizeRecipeImages(Long recipeId, Long callerUserId, boolean isAdmin) {
        Recipe recipe = recipeRepository.findWithStepsById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        // 1. 권한 체크
        if (!isAdmin && !recipe.getUser().getId().equals(callerUserId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }

        List<RecipeImage> images = recipeImageService.getImagesByRecipeId(recipeId);

        List<String> activeImages = new ArrayList<>();
        List<String> missingFiles = new ArrayList<>();

        boolean hasMainImageUploaded = false;

        // 2. S3 업로드 여부 확인 및 활성화 처리
        for (RecipeImage image : images) {
            boolean exists = s3Util.doesObjectExist(image.getFileKey());
            if (exists) {
                image.updateStatusToActive();
                activeImages.add(image.getFileKey());

                if ("main".equals(image.getSlot())) {
                    recipe.updateImageKey(image.getFileKey());
                    hasMainImageUploaded = true;
                }

                else if (image.getSlot().startsWith("step_")) {
                    int stepIndex = Integer.parseInt(image.getSlot().split("_")[1]);
                    recipe.getSteps().stream()
                            .filter(step -> step.getStepNumber() == stepIndex)
                            .findFirst()
                            .ifPresent(step -> {
                                step.updateStepImageKey(image.getFileKey());
                            });
                }
            } else {
                missingFiles.add(image.getFileKey());
            }
        }

        // 3. 공개 여부 처리
        if (missingFiles.isEmpty() && !recipe.isAiGenerated() && hasMainImageUploaded) {
            recipe.updateIsPrivate(false);
        }

        em.flush();

        // 4. 응답 반환
        return new FinalizeResponse(recipeId, activeImages, missingFiles);
    }


    @Transactional
    public void updateImageKeys(Long recipeId, Long userId, RecipeImageKeyUpdateRequest request) {
        Recipe recipe = getRecipeOrThrow(recipeId);
        validateOwnership(recipe, userId); // 작성자 권한 체크

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

        // AI 생성이고 이미지 없으면 공개 전환 불가
        boolean newValue = !recipe.getIsPrivate(); // 사용자가 공개로 바꾸려는 의도

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
