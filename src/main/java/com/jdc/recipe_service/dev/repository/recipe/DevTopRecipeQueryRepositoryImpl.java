package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.dto.recipe.QRecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeIngredient;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;

/**
 * 운영 {@link com.jdc.recipe_service.domain.repository.RecipeQueryRepositoryImpl#findTopByIngredientId}와 거의
 * 동일한 query 구조지만 {@code recipe.isPrivate.isFalse()} 자리에
 * {@link DevRecipeQueryPredicates#publicListedActive(QRecipe)} 적용 — RESTRICTED/PRIVATE/non-ACTIVE 누수 차단.
 *
 * Projection은 운영 {@link QRecipeSimpleDto} 재사용 (dev 전용 DTO 안 만듦 — 4 enum 노출 가치 낮은 영역).
 */
@Repository
@RequiredArgsConstructor
public class DevTopRecipeQueryRepositoryImpl implements DevTopRecipeQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Override
    public List<RecipeSimpleDto> findTopByIngredientIdDev(Long ingredientId, int limit) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeIngredient ri = QRecipeIngredient.recipeIngredient;

        // 운영 isPrivate.isFalse() 대신 dev 정책 (ACTIVE && PUBLIC && LISTED)
        BooleanExpression accessCondition = DevRecipeQueryPredicates.publicListedActive(recipe);
        BooleanExpression imageReadyCondition = recipe.imageStatus.eq(RecipeImageStatus.READY)
                .or(recipe.imageStatus.isNull());
        BooleanExpression ingredientFilter = ri.ingredient.id.eq(ingredientId);

        List<RecipeSimpleDto> content = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.favoriteCount.coalesce(0L),
                        recipe.likeCount,
                        Expressions.constant(false),
                        recipe.cookingTime.coalesce(0),
                        recipe.youtubeChannelName,
                        recipe.youtubeChannelId,
                        recipe.youtubeVideoTitle,
                        recipe.youtubeThumbnailUrl,
                        recipe.youtubeChannelProfileUrl,
                        recipe.youtubeSubscriberCount,
                        recipe.youtubeVideoViewCount,
                        recipe.avgRating.coalesce(BigDecimal.ZERO),
                        recipe.ratingCount.coalesce(0L),
                        recipe.youtubeUrl,
                        recipe.isAiGenerated
                ))
                .from(recipe)
                .join(recipe.ingredients, ri)
                .where(accessCondition, imageReadyCondition, ingredientFilter)
                .groupBy(recipe.id)
                .orderBy(recipe.popularityScore.desc(), recipe.id.desc())
                .limit(limit)
                .fetch();

        content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        return content;
    }
}
