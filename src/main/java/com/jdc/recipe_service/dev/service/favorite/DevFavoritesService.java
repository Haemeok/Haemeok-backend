package com.jdc.recipe_service.dev.service.favorite;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleDto;
import com.jdc.recipe_service.dev.repository.favorite.DevFavoritesQueryRepository;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 favorites service.
 *
 * 운영 {@code UserService.getFavoriteRecipesByUser}의 dev 미러:
 *  - dev repository로 정책 통과 레시피만 받아옴 (RESTRICTED 누수 차단)
 *  - 응답 DTO에 4 enum 노출 ({@link DevRecipeSimpleDto})
 *  - likedByCurrentUser는 favoriter 본인 기준 batch fetch
 *
 * favorite 자체는 본인 컬렉션이라 viewer == favoriter == userId.
 */
@Service
@RequiredArgsConstructor
public class DevFavoritesService {

    private final DevFavoritesQueryRepository devFavoritesQueryRepository;
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
    public Page<DevRecipeSimpleDto> getMyFavoritesDev(Long userId, Pageable pageable) {
        Page<Recipe> recipesPage = devFavoritesQueryRepository.findFavoritesAccessible(userId, pageable);

        Set<Long> likedIds;
        if (recipesPage.hasContent()) {
            List<Long> recipeIds = recipesPage.stream().map(Recipe::getId).toList();
            likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(userId, recipeIds).stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());
        } else {
            likedIds = Collections.emptySet();
        }

        return recipesPage.map(recipe -> {
            boolean isYoutube = recipe.getYoutubeUrl() != null && !recipe.getYoutubeUrl().isEmpty();
            return DevRecipeSimpleDto.builder()
                    .id(recipe.getId())
                    .title(recipe.getTitle())
                    .imageUrl(generateImageUrl(recipe.getImageKey()))
                    .authorId(recipe.getUser().getId())
                    .authorName(recipe.getUser().getNickname())
                    .profileImage(recipe.getUser().getProfileImage())
                    .createdAt(recipe.getCreatedAt())
                    .favoriteCount(recipe.getFavoriteCount())
                    .likeCount(recipe.getLikeCount())
                    .likedByCurrentUser(likedIds.contains(recipe.getId()))
                    .favoriteByCurrentUser(true)  // 즐겨찾기 목록이라 항상 true
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
                    .visibility(recipe.getVisibility() != null ? recipe.getVisibility().name() : null)
                    .lifecycleStatus(recipe.getLifecycleStatus() != null ? recipe.getLifecycleStatus().name() : null)
                    .source(recipe.getSource() != null ? recipe.getSource().name() : null)
                    // LAZY ManyToOne의 != null 체크는 FK 값만 보므로 select 미발생.
                    .remix(recipe.getOriginRecipe() != null)
                    .build();
        });
    }
}
