package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.QRecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.type.TagType;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.CaseBuilder;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.NumberExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

@RequiredArgsConstructor
public class RecipeQueryRepositoryImpl implements RecipeQueryRepository {

    private final JPAQueryFactory queryFactory;
    private final RecipeLikeRepository recipeLikeRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Override
    public Page<RecipeSimpleDto> search(RecipeSearchCondition cond, Pageable pageable, Long currentUserId) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;

        BooleanExpression privacyCondition = recipe.isPrivate.isFalse();

        BooleanExpression imageReadyCondition = recipe.imageStatus.eq(RecipeImageStatus.READY)
                .or(recipe.imageStatus.isNull());

        var contentQuery = queryFactory
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
                        recipe.cookingTime,
                        recipe.youtubeChannelName,
                        recipe.youtubeChannelId,
                        recipe.youtubeVideoTitle,
                        recipe.youtubeThumbnailUrl,
                        recipe.youtubeChannelProfileUrl,
                        recipe.youtubeSubscriberCount,
                        recipe.youtubeVideoViewCount,
                        recipe.avgRating,
                        recipe.ratingCount.coalesce(0L),
                        recipe.youtubeUrl,
                        recipe.isAiGenerated
                ))
                .from(recipe);

        if (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) {
            contentQuery.leftJoin(recipe.tags, tag);
        }

        List<RecipeSimpleDto> content = contentQuery
                .where(
                        privacyCondition,
                        imageReadyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums(), tag),
                        filterByTypes(cond.getTypes()),
                        costBetween(cond.getMinCost(), cond.getMaxCost()),
                        caloriesBetween(cond.getMinCalories(), cond.getMaxCalories()),
                        proteinBetween(cond.getMinProtein(), cond.getMaxProtein()),
                        carbBetween(cond.getMinCarb(), cond.getMaxCarb()),
                        fatBetween(cond.getMinFat(), cond.getMaxFat()),
                        sugarBetween(cond.getMinSugar(), cond.getMaxSugar()),
                        sodiumBetween(cond.getMinSodium(), cond.getMaxSodium())
                )
                .groupBy(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount,
                        recipe.likeCount
                )
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));

            if (currentUserId != null) {
                List<Long> recipeIds = content.stream()
                        .map(RecipeSimpleDto::getId)
                        .toList();

                Set<Long> likedIds = recipeLikeRepository
                        .findRecipeIdsByUserIdAndRecipeIdIn(currentUserId, recipeIds);

                content.forEach(dto ->
                        dto.setLikedByCurrentUser(likedIds.contains(dto.getId()))
                );
            }
        }

        var countQuery = queryFactory
                .select(recipe.countDistinct())
                .from(recipe);

        if (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) {
            countQuery.leftJoin(recipe.tags, tag);
        }

        Long total = countQuery
                .where(
                        privacyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums(), tag),
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
    public Page<RecipeSimpleDto> findPopularRecipesSince(LocalDateTime startDate, Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeLike recipeLike = QRecipeLike.recipeLike;

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
                        recipe.cookingTime,
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
                .leftJoin(recipeLike)
                .on(recipeLike.recipe.eq(recipe)
                        .and(recipeLike.createdAt.goe(startDate)))
                .where(recipe.isPrivate.isFalse())
                .groupBy(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.likeCount,
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount
                )
                .orderBy(
                        recipeLike.count().desc(),
                        recipe.avgRating.desc(),
                        recipe.createdAt.desc()
                )
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));

        Long total = queryFactory
                .select(recipe.count())
                .from(recipe)
                .where(recipe.isPrivate.isFalse())
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    @Override
    public Page<RecipeSimpleDto> findBudgetRecipes(Integer maxCost, Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        BooleanExpression condition = recipe.isPrivate.isFalse();
        if (maxCost != null) {
            condition = condition.and(recipe.totalIngredientCost.loe(maxCost));
        }

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
                        recipe.cookingTime,
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
                .where(condition)
                .orderBy(recipe.totalIngredientCost.asc())
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));

        Long total = queryFactory
                .select(recipe.count())
                .from(recipe)
                .where(condition)
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    @Override
    public Page<RecipeSimpleDto> searchAndSortByDynamicField(
            RecipeSearchCondition cond,
            String property,
            Sort.Direction direction,
            Pageable pageable,
            Long userId) {

        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;

        BooleanExpression privacyCondition = recipe.isPrivate.isFalse();

        Order dir = direction.isAscending() ? Order.ASC : Order.DESC;
        OrderSpecifier<?> dynamicOrder;

        if ("likeCount".equals(property)) {
            dynamicOrder = new OrderSpecifier<>(dir, recipe.likeCount);
        } else if ("avgRating".equals(property)) {
            dynamicOrder = new OrderSpecifier<>(dir, recipe.avgRating);
        } else {
            dynamicOrder = recipe.createdAt.desc();
        }

        var contentQuery = queryFactory
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
                        recipe.cookingTime,
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
                .from(recipe);

        if (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) {
            contentQuery.leftJoin(recipe.tags, tag);
        }

        List<RecipeSimpleDto> content = contentQuery
                .where(
                        privacyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums(), tag),
                        filterByTypes(cond.getTypes()),
                        costBetween(cond.getMinCost(), cond.getMaxCost()),
                        caloriesBetween(cond.getMinCalories(), cond.getMaxCalories()),
                        proteinBetween(cond.getMinProtein(), cond.getMaxProtein()),
                        carbBetween(cond.getMinCarb(), cond.getMaxCarb()),
                        fatBetween(cond.getMinFat(), cond.getMaxFat()),
                        sugarBetween(cond.getMinSugar(), cond.getMaxSugar()),
                        sodiumBetween(cond.getMinSodium(), cond.getMaxSodium())
                )
                .groupBy(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount,
                        recipe.likeCount
                )
                .orderBy(dynamicOrder, recipe.createdAt.desc())
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        if (!content.isEmpty()) {
            content.forEach(dto ->
                    dto.setImageUrl(generateImageUrl(dto.getImageUrl()))
            );
        }

        var countQuery = queryFactory
                .select(recipe.countDistinct())
                .from(recipe);

        if (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) {
            countQuery.leftJoin(recipe.tags, tag);
        }

        Long total = countQuery
                .where(
                        privacyCondition,
                        titleContains(cond.getTitle()),
                        dishTypeEq(cond.getDishTypeEnum()),
                        tagIn(cond.getTagEnums(), tag),
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
    public Page<Recipe> findByFridgeFallback(
            List<Long> fridgeIds,
            List<RecipeType> types,
            Pageable pageable
    ) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeIngredient ri = QRecipeIngredient.recipeIngredient;
        QIngredient ing = QIngredient.ingredient;
        QUser user = QUser.user;

        BooleanBuilder typeCondition = filterByTypes(types);
        Set<Long> pantryIds = com.jdc.recipe_service.opensearch.service.RecipeIndexingService.PANTRY_IDS;

        NumberExpression<Long> totalEssentialCount = ri.count();
        NumberExpression<Long> myFridgeCount = new CaseBuilder()
                .when(ing.id.in(fridgeIds)).then(1L)
                .otherwise((Long) null)
                .count();

        var query = queryFactory
                .select(recipe)
                .from(recipe)
                .join(recipe.user, user).fetchJoin()
                .join(recipe.ingredients, ri)
                .join(ri.ingredient, ing)
                .where(
                        recipe.isPrivate.isFalse(),
                        typeCondition,
                        ing.id.notIn(pantryIds)
                )
                .groupBy(recipe.id)
                .having(totalEssentialCount.eq(myFridgeCount))
                .orderBy(getFallbackOrderSpecifiers(pageable));

        List<Recipe> content = query
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        long total = queryFactory
                .select(recipe.id)
                .from(recipe)
                .join(recipe.ingredients, ri)
                .join(ri.ingredient, ing)
                .where(
                        recipe.isPrivate.isFalse(),
                        typeCondition,
                        ing.id.notIn(pantryIds)
                )
                .groupBy(recipe.id)
                .having(totalEssentialCount.eq(myFridgeCount))
                .fetch().size();

        return new PageImpl<>(content, pageable, total);
    }

    private OrderSpecifier<?>[] getFallbackOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        if (pageable.getSort().isEmpty()) {
            return new OrderSpecifier[]{recipe.createdAt.desc()};
        }

        return pageable.getSort().stream().map(order -> {
            Order direction = order.isAscending() ? Order.ASC : Order.DESC;
            switch (order.getProperty()) {
                case "avgRating":
                    return new OrderSpecifier<>(direction, recipe.avgRating);
                case "likeCount":
                    return new OrderSpecifier<>(direction, recipe.likeCount);
                case "cookingTime":
                    return new OrderSpecifier<>(direction, recipe.cookingTime);
                case "createdAt":
                default:
                    return new OrderSpecifier<>(direction, recipe.createdAt);
            }
        }).toArray(OrderSpecifier[]::new);
    }


    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        if (pageable.getSort().isEmpty()) {
            return new OrderSpecifier[]{recipe.createdAt.desc()};
        }

        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.getDirection().isAscending() ? Order.ASC : Order.DESC;
                    switch (o.getProperty()) {
                        case "likeCount":
                            return new OrderSpecifier<>(dir, recipe.likeCount);
                        case "cookingTime":
                            return new OrderSpecifier<>(dir, recipe.cookingTime);
                        case "avgRating":
                            return new OrderSpecifier<>(dir, recipe.avgRating);
                        case "totalIngredientCost":
                            return new OrderSpecifier<>(dir, recipe.totalIngredientCost);
                        case "createdAt":
                        default:
                            return new OrderSpecifier<>(dir, recipe.createdAt);
                    }
                })
                .toArray(OrderSpecifier[]::new);
    }

    private BooleanExpression dishTypeEq(DishType dishType) {
        return (dishType != null) ? QRecipe.recipe.dishType.eq(dishType) : null;
    }

    private BooleanExpression tagIn(List<TagType> tagTypes, QRecipeTag tag) {
        return (tagTypes != null && !tagTypes.isEmpty())
                ? tag.tag.in(tagTypes)
                : null;
    }

    private BooleanExpression titleContains(String keyword) {
        if (!StringUtils.hasText(keyword)) {
            return null;
        }
        return QRecipe.recipe.title.containsIgnoreCase(keyword)
                .or(QRecipe.recipe.youtubeChannelName.containsIgnoreCase(keyword));
    }

    private BooleanBuilder filterByTypes(List<RecipeType> types) {
        if (types == null || types.isEmpty()) {
            return null;
        }
        if (types.size() == RecipeType.values().length) {
            return null;
        }

        BooleanBuilder builder = new BooleanBuilder();
        QRecipe recipe = QRecipe.recipe;

        for (RecipeType type : types) {
            switch (type) {
                case AI:
                    builder.or(recipe.isAiGenerated.isTrue());
                    break;
                case YOUTUBE:
                    builder.or(recipe.youtubeUrl.isNotNull().and(recipe.youtubeUrl.ne("")));
                    break;
                case USER:
                    builder.or(recipe.isAiGenerated.isFalse()
                            .and(recipe.youtubeUrl.isNull().or(recipe.youtubeUrl.eq(""))));
                    break;
            }
        }
        return builder;
    }


    private BooleanExpression costBetween(Integer min, Integer max) {
        if (min == null && max == null) return null;
        int minVal = (min != null) ? min : 0;
        if (max == null) {
            return QRecipe.recipe.totalIngredientCost.goe(minVal);
        }
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

    private BooleanExpression numberBetween(com.querydsl.core.types.dsl.NumberPath<BigDecimal> path, Integer min, Integer max) {
        if (min == null && max == null) return null;
        int minVal = (min != null) ? min : 0;

        if (max == null) {
            return path.goe(BigDecimal.valueOf(minVal));
        }
        return path.between(BigDecimal.valueOf(minVal), BigDecimal.valueOf(max));
    }
}