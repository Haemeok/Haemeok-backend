package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.QRecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeLike;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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
    public Page<RecipeSimpleDto> search(String title, DishType dishType, List<TagType> tagTypes, Boolean isAiGenerated, Integer maxCost, Pageable pageable, Long currentUserId) {
        QRecipe recipe   = QRecipe.recipe;
        QRecipeTag tag    = QRecipeTag.recipeTag;
        QRecipeLike rLike = QRecipeLike.recipeLike;

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);
        BooleanExpression aiCondition      = (isAiGenerated != null)
                ? recipe.isAiGenerated.eq(isAiGenerated)
                : null;
//        BooleanExpression adminExclude     = recipe.user.id.ne(7L);

        var contentQuery = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        rLike.id.countDistinct().castToNum(Long.class),
                        Expressions.constant(false),
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount.coalesce(0L)
                ))
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .leftJoin(recipe.likes, rLike)
                .where(
                        privacyCondition,
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tagTypes),
                        aiCondition,
                        maxCostLoe(maxCost)
//                        ,adminExclude
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
                        recipe.ratingCount
                )
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<RecipeSimpleDto> content = contentQuery.fetch();

        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
            if (currentUserId != null) {
                List<Long> recipeIds = content.stream()
                        .map(RecipeSimpleDto::getId)
                        .toList();
                Set<Long> likedIds = recipeLikeRepository
                        .findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                        .stream()
                        .map(liked -> liked.getRecipe().getId())
                        .collect(Collectors.toSet());
                content.forEach(dto -> dto.setLikedByCurrentUser(likedIds.contains(dto.getId())));
            }
        }

        Long total = queryFactory
                .select(recipe.countDistinct())
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .where(
                        privacyCondition,
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tagTypes),
                        aiCondition,
                        maxCostLoe(maxCost)
//                        ,adminExclude
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }


    @Override
    public Page<RecipeSimpleDto> findAllSimpleWithRatingAndCookingInfo(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);

        List<RecipeSimpleDto> content = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.likes.size().castToNum(Long.class),
                        com.querydsl.core.types.dsl.Expressions.constant(false),
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount.coalesce(0L)
                ))
                .from(recipe)
                .where(privacyCondition)
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));

        Long total = queryFactory
                .select(recipe.count())
                .from(recipe)
                .where(privacyCondition)
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }


    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.getDirection().isAscending() ? Order.ASC : Order.DESC;
                    switch (o.getProperty()) {
                        case "likeCount":
                            return new OrderSpecifier<>(dir, recipe.likes.size());
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

    private BooleanExpression tagIn(List<TagType> tagTypes) {
        return (tagTypes != null && !tagTypes.isEmpty())
                ? QRecipeTag.recipeTag.tag.in(tagTypes)
                : null;
    }

    private BooleanExpression titleContains(String title) {
        return StringUtils.hasText(title) ? QRecipe.recipe.title.containsIgnoreCase(title) : null;
    }

    private BooleanExpression maxCostLoe(Integer maxCost) {
        return (maxCost != null) ? QRecipe.recipe.totalIngredientCost.loe(maxCost) : null;
    }
}