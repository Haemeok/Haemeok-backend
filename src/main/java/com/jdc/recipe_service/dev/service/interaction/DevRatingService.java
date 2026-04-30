package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeRatingService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Dev V3 평점(rating) dispatcher.
 *
 * 운영 {@link RecipeRatingService#rateRecipe}는 rating upsert + 옵션으로 새 {@code RecipeComment} 추가 +
 * comment 알림(REQUIRES_NEW)을 한 번에 수행한다. 알림 REQUIRES_NEW가 outer transaction rollback에 묶이지 않아
 * race로 게이트 우회 add가 일어나면 알림이 새어나갈 수 있다 — like와 동일한 누수 벡터.
 *
 * 따라서 분기로 차단:
 *  - {@code existing rating + 새 comment 없음} → "pure update" 경로: 운영 service 호출 없이 rating 값만 직접 갱신 +
 *    avg/count 재계산. race로 이미 사라졌으면 no-op (add 절대 안 함).
 *  - {@code 신규 rating OR 새 comment 추가} → {@link DevRecipeAccessValidator} 통과 후 운영 service 위임
 *    (comment 알림은 게이트 통과한 add에만 발생).
 *
 * delete/getMy는 cleanup right / self-read이라 게이트 없음 — 단순 위임.
 *
 * <p><b>운영 코드 동기화 주의:</b> 향후 {@link RecipeRatingService#rateRecipe} update 경로에 추가 side effect가
 * 들어가면 {@code updateExistingRatingDirect}도 갱신해야 한다.
 */
@Service
@RequiredArgsConstructor
public class DevRatingService {

    private final DevRecipeAccessValidator accessValidator;
    private final RecipeRatingRepository ratingRepository;
    private final RecipeRepository recipeRepository;
    private final RecipeRatingService ratingService;

    @Transactional
    public void rateRecipe(Long userId, Long recipeId, RecipeRatingRequestDto dto) {
        boolean hasExistingRating = ratingRepository.findByUserIdAndRecipeId(userId, recipeId).isPresent();
        boolean hasNewComment = dto.getComment() != null && !dto.getComment().isBlank();

        if (hasExistingRating && !hasNewComment) {
            // pure update: 운영 service 미호출 → race로 add 발생 자체 불가, 알림 누수 차단
            updateExistingRatingDirect(userId, recipeId, dto.getRating());
            return;
        }

        // 신규 rating OR 새 comment → 게이트 통과 후 운영 service에 위임
        accessValidator.loadAndCheckInteractable(recipeId, userId);
        ratingService.rateRecipe(recipeId, userId, dto);
    }

    @Transactional(readOnly = true)
    public Double getMyRating(Long userId, Long recipeId) {
        // self-data read — 게이트 없음 (운영과 동일하게 0.0 default)
        return ratingService.getMyRating(recipeId, userId);
    }

    @Transactional
    public void deleteRating(Long userId, Long recipeId) {
        // cleanup right — 게이트 없음. 운영 deleteRating은 RATING_NOT_FOUND를 던지므로 그대로 propagate.
        // 알림 등 REQUIRES_NEW side effect 없음 → 안전.
        ratingService.deleteRating(recipeId, userId);
    }

    private void updateExistingRatingDirect(Long userId, Long recipeId, Double newRating) {
        RecipeRating rating = ratingRepository.findByUserIdAndRecipeId(userId, recipeId).orElse(null);
        if (rating == null) {
            // race로 이미 삭제됨 → no-op (add 절대 안 함, 핵심 invariant)
            return;
        }
        rating.updateRating(newRating);

        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        double avg = ratingRepository.calculateAverageByRecipeId(recipeId);
        long count = ratingRepository.countByRecipeId(recipeId);
        recipe.updateAvgRating(BigDecimal.valueOf(avg).setScale(2, RoundingMode.HALF_UP));
        recipe.updateRatingCount(count);
    }
}
