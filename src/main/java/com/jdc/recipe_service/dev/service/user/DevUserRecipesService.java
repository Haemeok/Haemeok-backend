package com.jdc.recipe_service.dev.service.user;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevMyRecipeSummaryDto;
import com.jdc.recipe_service.dev.repository.recipe.DevUserRecipesQueryRepository;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 user/me recipes service.
 *
 * 운영 {@code UserService.getUserRecipes}의 dev 미러:
 *  - 운영 isOwner 분기(owner면 isPrivate 무관 모두) 대신 통합 {@link DevUserRecipesQueryRepository}로 위임
 *  - dev 정책: viewer == owner면 ACTIVE PRIVATE/RESTRICTED 모두, viewer != owner면 ACTIVE+PUBLIC+LISTED만
 *  - non-ACTIVE는 owner도 차단 (dev V3 invariant — 운영의 owner-all-visible과 다름)
 *
 * 응답 DTO는 {@link DevMyRecipeSummaryDto} (운영 base + 4 enum 노출).
 */
@Service
@RequiredArgsConstructor
public class DevUserRecipesService {

    private final UserRepository userRepository;
    private final DevUserRecipesQueryRepository devUserRecipesQueryRepository;
    private final RecipeLikeRepository recipeLikeRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Transactional(readOnly = true)
    public Page<DevMyRecipeSummaryDto> getUserRecipesDev(
            Long targetUserId,
            @Nullable Long viewerId,
            @Nullable List<RecipeSourceType> sourceTypes,
            org.springframework.data.domain.Pageable pageable) {

        userRepository.findById(targetUserId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        Page<Recipe> recipesPage = devUserRecipesQueryRepository
                .findUserRecipesAccessible(targetUserId, viewerId, sourceTypes, pageable);

        Set<Long> likedIds;
        if (viewerId != null && recipesPage.hasContent()) {
            List<Long> ids = recipesPage.stream().map(Recipe::getId).toList();
            likedIds = recipeLikeRepository
                    .findByUserIdAndRecipeIdIn(viewerId, ids)
                    .stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());
        } else {
            likedIds = Collections.emptySet();
        }

        return recipesPage.map(recipe -> DevMyRecipeSummaryDto.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .dishType(recipe.getDishType().getDisplayName())
                .type(recipe.getSource().name())
                .createdAt(recipe.getCreatedAt())
                .isAiGenerated(recipe.isAiGenerated())
                .isPrivate(recipe.getIsPrivate())
                .likedByCurrentUser(likedIds.contains(recipe.getId()))
                // dev V3 4 enum 노출
                .visibility(recipe.getVisibility() != null ? recipe.getVisibility().name() : null)
                .listingStatus(recipe.getListingStatus() != null ? recipe.getListingStatus().name() : null)
                .lifecycleStatus(recipe.getLifecycleStatus() != null ? recipe.getLifecycleStatus().name() : null)
                .source(recipe.getSource() != null ? recipe.getSource().name() : null)
                .imageStatus(recipe.getImageStatus() != null ? recipe.getImageStatus().name() : null)
                .build());
    }
}
