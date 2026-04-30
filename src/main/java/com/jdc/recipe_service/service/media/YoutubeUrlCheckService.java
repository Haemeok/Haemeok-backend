package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
@Slf4j
public class YoutubeUrlCheckService {

    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;

    private final RecipeRepository recipeRepository;

    private static final Pattern YOUTUBE_URL_PATTERN = Pattern.compile(
            "(?i)^(https?://)?(www\\.)?(youtube\\.com|youtu\\.be)/.+$"
    );

    /**
     * 특정 유튜브 URL에 대해 이미 공개 가능한 official 레시피가 존재하는지 확인합니다.
     *
     * strict 조건은 모두 SQL로 push down (source=YOUTUBE, ACTIVE, PUBLIC, LISTED, imageStatus=READY OR NULL).
     * 같은 URL에 older PRIVATE row + later PUBLIC row가 공존해도 PUBLIC을 정확히 매칭한다.
     * 매칭 실패 시 null 반환 — RESTRICTED/PRIVATE/non-ACTIVE/PENDING/FAILED 레시피의 존재가 누설되지 않음.
     *
     * {@link PageRequest#of(int, int) PageRequest.of(0, 1)}로 SQL LIMIT 1을 강제 — strict 통과 row가 2개 이상이어도
     * 가장 작은 id 하나만 반환 ({@code IncorrectResultSizeDataAccessException} 회피).
     *
     * @param videoUrl 검사할 유튜브 URL
     * @return strict 조건 통과한 레시피 ID 또는 null
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
        PageRequest limitOne = PageRequest.of(0, 1);

        Optional<Recipe> existingRecipe = recipeRepository
                .findStrictPublicYoutubeRecipes(watchUrl, OFFICIAL_RECIPE_USER_ID, limitOne)
                .stream()
                .findFirst()
                .or(() -> recipeRepository
                        .findStrictPublicYoutubeRecipes(shortsUrl, OFFICIAL_RECIPE_USER_ID, limitOne)
                        .stream()
                        .findFirst());

        return existingRecipe.map(Recipe::getId).orElse(null);
    }

    private String extractVideoId(String url) {
        Matcher matcher = Pattern.compile("(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%5C%2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*").matcher(url);
        return matcher.find() ? matcher.group().trim() : null;
    }
}
