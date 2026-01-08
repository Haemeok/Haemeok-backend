package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.QRecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.domain.type.TagType;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.util.List;


@Repository
@RequiredArgsConstructor
public class RecipeQueryRepositoryImplV2 implements RecipeQueryRepositoryV2 {

    private final JPAQueryFactory queryFactory;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Override
    public Page<RecipeSimpleStaticDto> searchStatic(RecipeSearchCondition cond, Pageable pageable, Long currentUserId) {
        QRecipe recipe   = QRecipe.recipe;
        QRecipeTag tag    = QRecipeTag.recipeTag;

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);

        var contentQuery = queryFactory
                .select(new QRecipeSimpleStaticDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime,
                        recipe.likeCount.coalesce(0L),
                        recipe.avgRating.coalesce(BigDecimal.ZERO),
                        recipe.ratingCount.coalesce(0L),
                        recipe.youtubeUrl,
                        recipe.isAiGenerated
                ))
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .leftJoin(recipe.user, com.jdc.recipe_service.domain.entity.QUser.user)
                .where(
                        privacyCondition,
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

        List<RecipeSimpleStaticDto> content = contentQuery.fetch();

        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        }

        Long total = queryFactory
                .select(recipe.countDistinct())
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .where(
                        privacyCondition,
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

    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.getDirection().isAscending() ? Order.ASC : Order.DESC;
                    switch (o.getProperty()) {
                        case "cookingTime":
                            return new OrderSpecifier<>(dir, recipe.cookingTime);
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

    private BooleanExpression tagIn(List<TagType> tagTypes) {
        return (tagTypes != null && !tagTypes.isEmpty())
                ? QRecipeTag.recipeTag.tag.in(tagTypes)
                : null;
    }

    private BooleanExpression titleContains(String title) {
        return StringUtils.hasText(title) ? QRecipe.recipe.title.containsIgnoreCase(title) : null;
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