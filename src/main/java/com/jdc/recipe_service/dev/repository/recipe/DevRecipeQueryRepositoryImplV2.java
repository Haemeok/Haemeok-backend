package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.QDevRecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.type.TagType;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.util.List;

/**
 * Dev V3 QueryDSL search 구현.
 *
 * <p>운영 {@link com.jdc.recipe_service.domain.repository.RecipeQueryRepositoryImplV2}와 거의 동일한 query 구조지만:
 * <ul>
 *   <li>{@code privacyCondition (recipe.isPrivate.eq(false))} 자리에 {@link DevRecipeQueryPredicates#publicListedActive} 사용
 *       → discovery 단일 정책 (검색은 viewer 무관하게 ACTIVE+PUBLIC+LISTED만, owner 자기 PRIVATE 글도 검색 결과에 안 섞임)</li>
 *   <li>SELECT projection을 {@link DevRecipeSimpleStaticDto}로 (4 enum 추가)</li>
 * </ul>
 *
 * <p>운영 코드는 zero touch (의존성 방향 깔끔, 영향 0). 헬퍼들은 일시적 중복.
 *
 * <p><b>viewerId 인자</b>: 시그니처 호환을 위해 유지하되 정책상 무시된다 (검색은 discovery라 viewer 분기 없음).
 * 향후 인터페이스 정리 시 제거 가능.
 */
@Repository
@RequiredArgsConstructor
public class DevRecipeQueryRepositoryImplV2 implements DevRecipeQueryRepositoryV2 {

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
    public Page<DevRecipeSimpleStaticDto> searchStatic(RecipeSearchCondition cond,
                                                        Pageable pageable,
                                                        @Nullable Long viewerId) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;

        // 검색은 discovery 단일 정책 — ACTIVE && PUBLIC && LISTED. viewerId는 무시 (link-only/PRIVATE/RESTRICTED 모두 검색에서 빠짐).
        BooleanExpression accessCondition = DevRecipeQueryPredicates.publicListedActive(recipe);

        BooleanExpression imageReadyCondition = recipe.imageStatus.eq(RecipeImageStatus.READY)
                .or(recipe.imageStatus.isNull());

        var contentQuery = queryFactory
                .select(new QDevRecipeSimpleStaticDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime.coalesce(0),
                        recipe.likeCount.coalesce(0L),
                        recipe.favoriteCount.coalesce(0L),
                        recipe.avgRating.coalesce(BigDecimal.ZERO),
                        recipe.ratingCount.coalesce(0L),
                        recipe.youtubeChannelName,
                        recipe.youtubeChannelId,
                        recipe.youtubeVideoTitle,
                        recipe.youtubeThumbnailUrl,
                        recipe.youtubeChannelProfileUrl,
                        recipe.youtubeSubscriberCount,
                        recipe.youtubeVideoViewCount,
                        recipe.youtubeUrl,
                        recipe.isAiGenerated,
                        recipe.visibility,
                        recipe.lifecycleStatus,
                        recipe.source
                ))
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .leftJoin(recipe.user, com.jdc.recipe_service.domain.entity.QUser.user)
                .where(
                        accessCondition,
                        imageReadyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums()),
                        filterByTypes(cond.getTypes()),
                        costBetween(cond.getMinCost(), cond.getMaxCost()),
                        caloriesBetween(cond.getMinCalories(), cond.getMaxCalories()),
                        proteinBetween(cond.getMinProtein(), cond.getMaxProtein()),
                        carbBetween(cond.getMinCarb(), cond.getMaxCarb()),
                        fatBetween(cond.getMinFat(), cond.getMaxFat()),
                        sugarBetween(cond.getMinSugar(), cond.getMaxSugar()),
                        sodiumBetween(cond.getMinSodium(), cond.getMaxSodium())
                )
                .distinct()
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<DevRecipeSimpleStaticDto> content = contentQuery.fetch();
        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        }

        // 운영 V2의 count query에는 imageReadyCondition이 빠져 있어 totalElements가 부풀려지는 버그가 있다.
        // dev에서는 content/count where 조건을 일치시켜 페이지네이션 계산이 정확하게 한다.
        Long total = queryFactory
                .select(recipe.countDistinct())
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .where(
                        accessCondition,
                        imageReadyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums()),
                        filterByTypes(cond.getTypes()),
                        costBetween(cond.getMinCost(), cond.getMaxCost()),
                        caloriesBetween(cond.getMinCalories(), cond.getMaxCalories()),
                        proteinBetween(cond.getMinProtein(), cond.getMaxProtein()),
                        carbBetween(cond.getMinCarb(), cond.getMaxCarb()),
                        fatBetween(cond.getMinFat(), cond.getMaxFat()),
                        sugarBetween(cond.getMinSugar(), cond.getMaxSugar()),
                        sodiumBetween(cond.getMinSodium(), cond.getMaxSodium())
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    @Override
    public List<DevRecipeSimpleStaticDto> findAllByIds(List<Long> ids, @Nullable Long viewerId) {
        if (ids == null || ids.isEmpty()) {
            return List.of();
        }
        QRecipe recipe = QRecipe.recipe;

        // OpenSearch dev index stale/lag 대비 — DB가 source of truth로 한 번 더 정책 검증.
        // OpenSearch 검색이 publicListedActiveFilter를 쓰므로 hydration도 동일 정책 — PUBLIC+UNLISTED/RESTRICTED/PRIVATE/non-ACTIVE 차단.
        BooleanExpression accessCondition = DevRecipeQueryPredicates.publicListedActive(recipe);
        BooleanExpression imageReadyCondition = recipe.imageStatus.eq(RecipeImageStatus.READY)
                .or(recipe.imageStatus.isNull());

        List<DevRecipeSimpleStaticDto> dtos = queryFactory
                .select(new QDevRecipeSimpleStaticDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime.coalesce(0),
                        recipe.likeCount.coalesce(0L),
                        recipe.favoriteCount.coalesce(0L),
                        recipe.avgRating.coalesce(BigDecimal.ZERO),
                        recipe.ratingCount.coalesce(0L),
                        recipe.youtubeChannelName,
                        recipe.youtubeChannelId,
                        recipe.youtubeVideoTitle,
                        recipe.youtubeThumbnailUrl,
                        recipe.youtubeChannelProfileUrl,
                        recipe.youtubeSubscriberCount,
                        recipe.youtubeVideoViewCount,
                        recipe.youtubeUrl,
                        recipe.isAiGenerated,
                        recipe.visibility,
                        recipe.lifecycleStatus,
                        recipe.source
                ))
                .from(recipe)
                .leftJoin(recipe.user, com.jdc.recipe_service.domain.entity.QUser.user)
                .where(
                        recipe.id.in(ids),
                        accessCondition,
                        imageReadyCondition
                )
                .fetch();

        dtos.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        return dtos;
    }

    // ---------- helpers (운영 V2 RecipeQueryRepositoryImplV2와 동일 시그니처) ----------

    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;
        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.getDirection().isAscending() ? Order.ASC : Order.DESC;
                    return switch (o.getProperty()) {
                        case "cookingTime" -> new OrderSpecifier<>(dir, recipe.cookingTime);
                        default -> new OrderSpecifier<>(dir, recipe.createdAt);
                    };
                })
                .toArray(OrderSpecifier[]::new);
    }

    private BooleanExpression dishTypeEq(DishType dishType) {
        return (dishType != null) ? QRecipe.recipe.dishType.eq(dishType) : null;
    }

    private BooleanExpression tagIn(List<TagType> tagTypes) {
        return (tagTypes != null && !tagTypes.isEmpty()) ? QRecipeTag.recipeTag.tag.in(tagTypes) : null;
    }

    private BooleanExpression titleContains(String keyword) {
        if (!StringUtils.hasText(keyword)) return null;
        return QRecipe.recipe.title.containsIgnoreCase(keyword)
                .or(QRecipe.recipe.youtubeChannelName.containsIgnoreCase(keyword));
    }

    private BooleanBuilder filterByTypes(List<RecipeType> types) {
        if (types == null || types.isEmpty()) return null;
        if (types.size() == RecipeType.values().length) return null;

        BooleanBuilder builder = new BooleanBuilder();
        QRecipe recipe = QRecipe.recipe;
        for (RecipeType type : types) {
            switch (type) {
                case AI -> builder.or(recipe.isAiGenerated.isTrue());
                case YOUTUBE -> builder.or(recipe.youtubeUrl.isNotNull().and(recipe.youtubeUrl.ne("")));
                case USER -> builder.or(recipe.isAiGenerated.isFalse()
                        .and(recipe.youtubeUrl.isNull().or(recipe.youtubeUrl.eq(""))));
            }
        }
        return builder;
    }

    private BooleanExpression costBetween(Integer min, Integer max) {
        if (min == null && max == null) return null;
        int minVal = (min != null) ? min : 0;
        if (max == null) return QRecipe.recipe.totalIngredientCost.goe(minVal);
        return QRecipe.recipe.totalIngredientCost.between(minVal, max);
    }

    private BooleanExpression caloriesBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.totalCalories, min, max);
    }
    private BooleanExpression proteinBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.protein, min, max);
    }
    private BooleanExpression carbBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.carbohydrate, min, max);
    }
    private BooleanExpression fatBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.fat, min, max);
    }
    private BooleanExpression sugarBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.sugar, min, max);
    }
    private BooleanExpression sodiumBetween(Integer min, Integer max) {
        return numberBetween(QRecipe.recipe.sodium, min, max);
    }

    private BooleanExpression numberBetween(NumberPath<BigDecimal> path, Integer min, Integer max) {
        if (min == null && max == null) return null;
        int minVal = (min != null) ? min : 0;
        if (max == null) return path.goe(BigDecimal.valueOf(minVal));
        return path.between(BigDecimal.valueOf(minVal), BigDecimal.valueOf(max));
    }
}
