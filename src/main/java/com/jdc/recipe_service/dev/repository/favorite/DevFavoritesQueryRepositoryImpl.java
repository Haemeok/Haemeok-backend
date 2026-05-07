package com.jdc.recipe_service.dev.repository.favorite;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryPredicates;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeFavorite;
import com.jdc.recipe_service.domain.entity.QUser;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 운영 {@code findMyFavoritesWithPending}와 거의 동일 query 구조지만 dev 정책 적용:
 *  - {@code (r.isPrivate = false OR r.imageStatus = PENDING)} 자리에
 *    {@code viewableBy(recipe, userId)} + {@code imageReady} (link-only PUBLIC+UNLISTED 저장 항목도 노출)
 *  - userId가 favoriter이자 viewer (즐겨찾기는 본인 컬렉션)
 *
 * Fetch 전략: {@code select(recipe).from(favorite).join(favorite.recipe, recipe).join(recipe.user, user).fetchJoin()}.
 * recipe 자체는 select 대상이라 별도 fetchJoin 불필요. recipe.user만 fetchJoin으로 N+1 방지.
 *
 * 정렬: 운영이 {@code Page<RecipeFavorite>}를 반환해서 {@code createdAt}이 즐겨찾기 추가 시각이 되는 것과 같은
 * 시맨틱으로 default와 {@code createdAt}을 {@code favorite.createdAt}으로 매핑. recipe 생성일 기준은 별도 sort key
 * ({@code recipeCreatedAt})로 제공.
 */
@Repository
@RequiredArgsConstructor
public class DevFavoritesQueryRepositoryImpl implements DevFavoritesQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Override
    public Page<Recipe> findFavoritesAccessible(Long userId, Pageable pageable) {
        QRecipeFavorite favorite = QRecipeFavorite.recipeFavorite;
        QRecipe recipe = QRecipe.recipe;
        QUser user = QUser.user;

        // V1.x 정책: 즐겨찾기는 viewable — PUBLIC+UNLISTED(link-only) 저장된 글도 보여야 함. listingStatus 무시.
        BooleanExpression accessCondition = DevRecipeQueryPredicates.viewableBy(recipe, userId);
        BooleanExpression imageReadyCondition = recipe.imageStatus.eq(RecipeImageStatus.READY)
                .or(recipe.imageStatus.isNull());

        BooleanExpression whereClause = favorite.user.id.eq(userId)
                .and(accessCondition)
                .and(imageReadyCondition);

        // recipe + recipe.user fetch join (운영 JOIN FETCH 정합)
        List<Recipe> content = queryFactory
                .select(recipe)
                .from(favorite)
                .join(favorite.recipe, recipe)
                .join(recipe.user, user).fetchJoin()
                .where(whereClause)
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        Long total = queryFactory
                .select(favorite.count())
                .from(favorite)
                .join(favorite.recipe, recipe)
                .where(whereClause)
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipeFavorite favorite = QRecipeFavorite.recipeFavorite;
        QRecipe recipe = QRecipe.recipe;
        if (pageable.getSort().isEmpty()) {
            // default: 즐겨찾기 추가 시점 기준 최신 — 운영 Page<RecipeFavorite> 시맨틱 정합
            return new OrderSpecifier<?>[]{favorite.createdAt.desc()};
        }
        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.isAscending() ? Order.ASC : Order.DESC;
                    return switch (o.getProperty()) {
                        case "cookingTime" -> new OrderSpecifier<>(dir, recipe.cookingTime);
                        // recipe 생성일은 별도 key로 노출 (운영 시맨틱과 차별)
                        case "recipeCreatedAt" -> new OrderSpecifier<>(dir, recipe.createdAt);
                        // default("createdAt" 포함): favorite.createdAt — 운영 Page<RecipeFavorite>와 동일 의미
                        default -> new OrderSpecifier<>(dir, favorite.createdAt);
                    };
                })
                .toArray(OrderSpecifier[]::new);
    }
}
