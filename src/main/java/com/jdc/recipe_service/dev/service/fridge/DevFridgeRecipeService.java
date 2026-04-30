package com.jdc.recipe_service.dev.service.fridge;

import com.jdc.recipe_service.dev.repository.fridge.DevFridgeRecipeQueryRepository;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.type.RecipeType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 fridge 추천 service.
 *
 * 운영 {@link com.jdc.recipe_service.service.FridgeRecipeService}와 거의 동일한 변환 로직이지만
 * recipe 조회를 dev repository({@link DevFridgeRecipeQueryRepository})로 위임 — RESTRICTED 누수 차단.
 *
 * convertToDto는 운영 private 메서드라 복제 (일시 중복). 응답 shape은 운영과 동일 ({@link FridgeRecipeDto}).
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DevFridgeRecipeService {

    private final RefrigeratorItemRepository fridgeRepo;
    private final RecipeLikeRepository recipeLikeRepository;
    private final DevFridgeRecipeQueryRepository devFridgeRecipeQueryRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Transactional(readOnly = true)
    public Slice<FridgeRecipeDto> searchByFridgeDev(Long userId, Pageable pageable, List<RecipeType> types) {
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

        // 운영의 RecipeRepository.searchRecipesByFridgeIngredients 대신 dev repository 호출
        Slice<Recipe> recipeSlice = devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(
                searchIds, types, pageable);
        if (recipeSlice.isEmpty()) {
            return new SliceImpl<>(List.of(), pageable, false);
        }

        List<Long> recipeIds = recipeSlice.getContent().stream().map(Recipe::getId).toList();
        Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        List<FridgeRecipeDto> dtos = recipeSlice.getContent().stream()
                .map(recipe -> convertToDto(recipe, myIngredientIds, myIngredientNames, likedRecipeIds))
                .toList();

        return new SliceImpl<>(dtos, pageable, recipeSlice.hasNext());
    }

    /**
     * 운영 FridgeRecipeService.convertToDto와 거의 동일하지만 dev raw 보존 정책에 맞게 확장:
     *
     * <ul>
     *   <li><b>displayName</b>: rawName > customName > ingredient.name 우선순위.
     *       상세 화면 표시(RecipeIngredientDisplayResolver)와 일치해야 사용자가 본 재료명과 일관.
     *       AI fallback row(rawName="청양고추" + ingredient.name="고추")의 경우 운영처럼 "고추"가 아니라
     *       "청양고추"가 표시된다.</li>
     *   <li><b>매칭</b>: ingredient.id 매칭이 가장 강함. 그 외에는 row의 모든 이름 candidates
     *       (rawName/customName/ingredient.name)를 normalizeName으로 비교 — 하나라도 myIngredientNames에
     *       hit하면 matched.</li>
     *   <li><b>displayLink</b>: matched 안 됐을 때만. customLink > ingredient.coupangLink.</li>
     * </ul>
     *
     * <p><b>조회와 분류의 범위 차이 (중요)</b>: dev fridge 조회({@link DevFridgeRecipeQueryRepository})는
     * {@code join(recipeIngredient.ingredient) + ingredient.id IN (...)}로 작동한다. 즉
     * <b>ingredient.id가 null인 row(C' bypass / UNRESOLVED)는 검색 후보 자체에 포함되지 않는다</b>. 따라서
     * "쿠팡에서 산 청양고추(rawName)이 fridge에 있고, 어떤 레시피의 customName='청양고추' bypass row
     * 하나로만 그 레시피가 후보에 들어가는" 시나리오는 현재 repository로는 불가능 — 그 레시피의 다른
     * ingredient.id 매칭 row가 있어야 후보가 된다.
     *
     * <p>이 메서드의 raw-aware 분류는 <b>이미 후보로 들어온 레시피</b>에 대해 missing/matched 분리를
     * 정확히 하는 것 — bypass row가 같은 레시피에 끼어 있을 때 raw 표시명으로 일관되게 다룬다. raw/custom
     * name까지 조회 입력으로 확장하려면 repository에 OR 조건 추가가 필요하고, 그건 별도 작업.
     */
    private FridgeRecipeDto convertToDto(Recipe recipe, Set<Long> myIngredientIds, Set<String> myIngredientNames, Set<Long> likedRecipeIds) {
        List<String> matchedIngredients = new ArrayList<>();
        List<FridgeRecipeDto.MissingIngredientDto> missingIngredients = new ArrayList<>();

        for (RecipeIngredient ri : recipe.getIngredients()) {
            Ingredient ing = ri.getIngredient();
            if (ing != null && ing.isPantry()) {
                continue;
            }

            String displayName = pickDisplayName(ri);
            if (displayName == null) continue;  // raw/custom/ingredient 셋 다 비어있는 corrupt row 방어

            boolean isMatched = false;
            if (ing != null && myIngredientIds.contains(ing.getId())) {
                isMatched = true;
            } else if (matchesByAnyName(ri, myIngredientNames)) {
                isMatched = true;
            }

            if (isMatched) {
                matchedIngredients.add(displayName);
            } else {
                missingIngredients.add(new FridgeRecipeDto.MissingIngredientDto(displayName, pickDisplayLink(ri)));
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

    /** raw-first 표시 이름: rawName > customName > ingredient.name. 셋 다 비면 null. */
    private static String pickDisplayName(RecipeIngredient ri) {
        if (ri.getRawName() != null && !ri.getRawName().isBlank()) return ri.getRawName();
        if (ri.getCustomName() != null && !ri.getCustomName().isBlank()) return ri.getCustomName();
        if (ri.getIngredient() != null && ri.getIngredient().getName() != null
                && !ri.getIngredient().getName().isBlank()) {
            return ri.getIngredient().getName();
        }
        return null;
    }

    /**
     * 모든 이름 candidates(raw/custom/ingredient.name)를 정규화 후 myNames에 hit 검사.
     * 하나라도 일치하면 matched — raw row + AI fallback row + 운영 호환 row 모두 잡힌다.
     */
    private boolean matchesByAnyName(RecipeIngredient ri, Set<String> myNames) {
        if (myNames == null || myNames.isEmpty()) return false;
        if (matchesNormalized(ri.getRawName(), myNames)) return true;
        if (matchesNormalized(ri.getCustomName(), myNames)) return true;
        if (ri.getIngredient() != null && matchesNormalized(ri.getIngredient().getName(), myNames)) return true;
        return false;
    }

    private boolean matchesNormalized(String name, Set<String> myNames) {
        if (name == null || name.isBlank()) return false;
        return myNames.contains(normalizeName(name));
    }

    /** missing 라인의 표시 링크: customLink 우선, 없으면 ingredient.coupangLink (master master 있을 때). */
    private static String pickDisplayLink(RecipeIngredient ri) {
        if (ri.getCustomLink() != null && !ri.getCustomLink().isEmpty()) {
            return ri.getCustomLink();
        }
        if (ri.getIngredient() != null) {
            return ri.getIngredient().getCoupangLink();
        }
        return null;
    }
}
