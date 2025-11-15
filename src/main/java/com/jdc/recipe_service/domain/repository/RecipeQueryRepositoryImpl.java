package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.QRecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
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

import java.math.BigDecimal;
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

    @Override
    public Page<RecipeSimpleDto> searchAndSortByDynamicField(
            String title, DishType dishType, List<TagType> tags,
            Boolean isAiGenerated, Integer maxCost, String property,
            Sort.Direction direction, Pageable pageable, Long userId) {

        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;
        QRecipeLike rLike = QRecipeLike.recipeLike;

        // ⭐ 평점은 Recipe 엔티티에 직접 저장되어 있으므로 별도 Q-Type은 필요 없음

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);
        BooleanExpression aiCondition = (isAiGenerated != null)
                ? recipe.isAiGenerated.eq(isAiGenerated)
                : null;

        // 1. 동적 정렬 조건 생성
        Order dir = direction.isAscending() ? Order.ASC : Order.DESC;
        OrderSpecifier<?> dynamicOrder;

        if ("likeCount".equals(property)) {
            // 좋아요 수로 정렬: COUNT(rLike.id) 사용
            dynamicOrder = new OrderSpecifier<>(dir, rLike.id.countDistinct());
        } else if ("avgRating".equals(property)) {
            // 평균 평점으로 정렬: recipe.avgRating 필드 사용
            dynamicOrder = new OrderSpecifier<>(dir, recipe.avgRating);
        } else {
            // 안전 장치: 지원되지 않는 정렬이면 최신순으로 정렬
            dynamicOrder = new OrderSpecifier<>(Order.DESC, recipe.createdAt);
        }

        // 2. 검색 쿼리 실행
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
                        recipe.avgRating.coalesce(BigDecimal.valueOf(0.0d)),
                        recipe.ratingCount.coalesce(0L)
                ))
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .leftJoin(recipe.likes, rLike)
                .where(
                        privacyCondition,
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tags),
                        aiCondition,
                        maxCostLoe(maxCost)
                )
                .groupBy(
                        recipe.id, recipe.title, recipe.imageKey, recipe.user.id, recipe.user.nickname,
                        recipe.user.profileImage, recipe.createdAt, recipe.cookingTime,
                        recipe.avgRating, recipe.ratingCount
                )
                .orderBy(dynamicOrder, new OrderSpecifier<>(Order.DESC, recipe.createdAt))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<RecipeSimpleDto> content = contentQuery.fetch();

        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        }

        Long total = queryFactory
                .select(recipe.countDistinct())
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .where(
                        privacyCondition,
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tags),
                        aiCondition,
                        maxCostLoe(maxCost)
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    @Override
    public Page<Recipe> findByFridgeFallback(
            List<Long> fridgeIds,
            AiRecipeFilter aiFilter,
            Pageable pageable
    ) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeIngredient ri = QRecipeIngredient.recipeIngredient;
        QIngredient ing = QIngredient.ingredient;
        QUser user = QUser.user;

        BooleanExpression aiCond = buildAiFilter(aiFilter, recipe);

        var query = queryFactory
                .select(recipe)
                .from(recipe)
                .join(recipe.user, user).fetchJoin()
                .join(recipe.ingredients, ri)
                .join(ri.ingredient, ing)
                .where(
                        recipe.isPrivate.isFalse(),
                        ing.id.in(fridgeIds),
                        aiCond
                )
                .groupBy(recipe.id)
                .orderBy(
                        ri.count().desc(),
                        recipe.createdAt.desc()
                );

        List<Recipe> content = query
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        Long total = queryFactory
                .select(recipe.countDistinct())
                .from(recipe)
                .join(recipe.ingredients, ri)
                .join(ri.ingredient, ing)
                .where(
                        recipe.isPrivate.isFalse(),
                        ing.id.in(fridgeIds),
                        aiCond
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total == null ? 0 : total);
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

    private BooleanExpression buildAiFilter(AiRecipeFilter filter, QRecipe recipe) {
        if (filter == null || filter == AiRecipeFilter.ALL) return null;
        if (filter == AiRecipeFilter.USER_ONLY) return recipe.isAiGenerated.isFalse();
        if (filter == AiRecipeFilter.AI_ONLY) return recipe.isAiGenerated.isTrue();
        return null;
    }
}