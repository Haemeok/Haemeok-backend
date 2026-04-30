package com.jdc.recipe_service.dev.repository.fridge;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryPredicates;
import com.jdc.recipe_service.domain.entity.QFineDiningDetails;
import com.jdc.recipe_service.domain.entity.QIngredient;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeIngredient;
import com.jdc.recipe_service.domain.entity.QUser;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.NumberExpression;
import com.querydsl.jpa.impl.JPAQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 운영 {@link com.jdc.recipe_service.domain.repository.RecipeQueryRepositoryImpl#searchRecipesByFridgeIngredients}와
 * 거의 동일한 query 구조지만 {@code recipe.isPrivate.isFalse()} 자리에
 * {@link DevRecipeQueryPredicates#publicListedActive(QRecipe)} 적용.
 *
 * 운영의 {@code typeFilter}, {@code getSecondarySort} 헬퍼는 private이라 dev 복제 (일시 중복).
 */
@Repository
@RequiredArgsConstructor
public class DevFridgeRecipeQueryRepositoryImpl implements DevFridgeRecipeQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Override
    public Slice<Recipe> searchRecipesByFridgeIngredientsDev(
            List<Long> userIngredientIds,
            List<RecipeType> types,
            Pageable pageable) {

        QRecipe recipe = QRecipe.recipe;
        QRecipeIngredient recipeIngredient = QRecipeIngredient.recipeIngredient;
        QIngredient ingredient = QIngredient.ingredient;
        QUser user = QUser.user;
        QFineDiningDetails fineDiningDetails = QFineDiningDetails.fineDiningDetails;

        NumberExpression<Double> matchRate = recipeIngredient.count().doubleValue()
                .divide(recipe.totalIngredientCount.coalesce(1).doubleValue());

        JPAQuery<Recipe> query = queryFactory
                .selectFrom(recipe)
                .leftJoin(recipe.user, user).fetchJoin()
                .leftJoin(recipe.fineDiningDetails, fineDiningDetails).fetchJoin()
                .join(recipe.ingredients, recipeIngredient)
                .join(recipeIngredient.ingredient, ingredient)
                .where(
                        ingredient.id.in(userIngredientIds).and(ingredient.isPantry.isFalse()),
                        // 운영 isPrivate.isFalse() 대신 dev 정책 (ACTIVE && PUBLIC && LISTED)
                        DevRecipeQueryPredicates.publicListedActive(recipe),
                        // 운영 fridge query에는 빠져 있는 imageReady 필터를 dev에서는 일관되게 적용
                        // (A2 search, ingredient top recipe와 정합 — "실제 노출 가능한 레시피만" 추천)
                        recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()),
                        typeFilter(types)
                )
                .groupBy(recipe.id)
                .having(recipeIngredient.count().goe(1));

        OrderSpecifier<?> secondarySort = getSecondarySort(pageable);

        query.orderBy(matchRate.desc(), secondarySort);

        int pageSize = pageable.getPageSize();
        List<Recipe> content = query
                .offset(pageable.getOffset())
                .limit(pageSize + 1)
                .fetch();

        boolean hasNext = false;
        if (content.size() > pageSize) {
            content.remove(pageSize);
            hasNext = true;
        }

        return new SliceImpl<>(content, pageable, hasNext);
    }

    // ---------- helpers (운영 RecipeQueryRepositoryImpl와 동일 시그니처) ----------

    private BooleanBuilder typeFilter(List<RecipeType> types) {
        if (types == null || types.isEmpty()) {
            return null;
        }
        QRecipe recipe = QRecipe.recipe;
        BooleanBuilder builder = new BooleanBuilder();
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

    private OrderSpecifier<?> getSecondarySort(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;
        if (pageable.getSort().isEmpty()) {
            return recipe.createdAt.desc();
        }
        for (Sort.Order order : pageable.getSort()) {
            Order direction = order.isAscending() ? Order.ASC : Order.DESC;
            switch (order.getProperty()) {
                case "likeCount" -> { return new OrderSpecifier<>(direction, recipe.likeCount); }
                case "avgRating" -> { return new OrderSpecifier<>(direction, recipe.avgRating); }
                case "favoriteCount" -> { return new OrderSpecifier<>(direction, recipe.favoriteCount); }
                case "cookingTime" -> { return new OrderSpecifier<>(direction, recipe.cookingTime); }
            }
        }
        return recipe.createdAt.desc();
    }
}
