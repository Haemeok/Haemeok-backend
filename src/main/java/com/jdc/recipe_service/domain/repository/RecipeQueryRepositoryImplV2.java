package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.QRecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
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
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

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
    public Page<RecipeSimpleStaticDto> searchStatic(String title, DishType dishType, List<TagType> tagTypes, AiRecipeFilter aiFilter, Pageable pageable, Long currentUserId) {
        QRecipe recipe   = QRecipe.recipe;
        QRecipeTag tag    = QRecipeTag.recipeTag;

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);
        BooleanExpression aiCondition = filterAiGenerated(aiFilter);

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
                        Expressions.constant(0L),
                        Expressions.constant(0.0),
                        Expressions.constant(0L)
                ))
                .from(recipe)
                .leftJoin(recipe.tags, tag)
                .where(
                        privacyCondition,
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tagTypes),
                        aiCondition
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
                        titleContains(title),
                        dishTypeEq(dishType),
                        tagIn(tagTypes),
                        aiCondition
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }


    @Override
    public Page<RecipeSimpleStaticDto> findAllSimpleStatic(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        BooleanExpression privacyCondition = recipe.isPrivate.eq(false);

        List<RecipeSimpleStaticDto> content = queryFactory
                .select(new QRecipeSimpleStaticDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.id,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.cookingTime,
                        Expressions.constant(0L),
                        Expressions.constant(0.0),
                        Expressions.constant(0L)
                ))
                .from(recipe)
                .where(privacyCondition)
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        if (!content.isEmpty()) {
            content.forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        }

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

    private BooleanExpression filterAiGenerated(AiRecipeFilter filter) {
        if (filter == AiRecipeFilter.AI_ONLY) {
            return QRecipe.recipe.isAiGenerated.isTrue();
        }
        if (filter == AiRecipeFilter.ALL) {
            return null;
        }
        return QRecipe.recipe.isAiGenerated.isFalse();
    }

}