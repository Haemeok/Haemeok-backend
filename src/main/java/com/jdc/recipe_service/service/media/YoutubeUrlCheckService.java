package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
@Slf4j
public class YoutubeUrlCheckService {

    private final RecipeRepository recipeRepository;

    private static final Pattern YOUTUBE_URL_PATTERN = Pattern.compile(
            "(?i)^(https?://)?(www\\.)?(youtube\\.com|youtu\\.be)/.+$"
    );

    /**
     * 특정 유튜브 URL에 대해 이미 '이미지 레시피'가 존재하는지 확인합니다.
     * @param videoUrl 검사할 유튜브 URL
     * @return 존재 여부 및 기존 레시피 ID
     */
    @Transactional(readOnly = true)
    public Long checkUrlExistence(String videoUrl) {
        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }
        String videoId = extractVideoId(videoUrl);
        if (videoId == null) throw new CustomException(ErrorCode.INVALID_URL_FORMAT);

        String watchUrl = "https://www.youtube.com/watch?v=" + videoId;
        String shortsUrl = "https://www.youtube.com/shorts/" + videoId;

        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(watchUrl)
                .or(() -> recipeRepository.findFirstByYoutubeUrl(shortsUrl));

        return existingRecipe
                .filter(r -> r.getImageKey() != null)
                .filter(r -> r.getSource() == RecipeSourceType.YOUTUBE)
                .filter(r -> r.getLifecycleStatus() == RecipeLifecycleStatus.ACTIVE)
                .filter(r -> r.getVisibility() == RecipeVisibility.PUBLIC)
                .filter(r -> r.getListingStatus() == RecipeListingStatus.LISTED)
                .map(Recipe::getId)
                .orElse(null);
    }

    private String extractVideoId(String url) {
        Matcher matcher = Pattern.compile("(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%5C%2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*").matcher(url);
        return matcher.find() ? matcher.group().trim() : null;
    }
}