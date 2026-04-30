package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 운영 {@code findByUserIdAndIsPrivateFalse(...)} / {@code findCompletedRecipesByUserId(...)}의 dev 통합 버전.
 *
 *  - {@code recipe.user.id = targetUserId} 기본 + viewer 시점 정책({@link DevRecipeQueryPredicates#accessibleBy})
 *  - {@code AI && imageKey IS NULL} 차단 (운영 "completed recipes" 조건 동일 — AI는 이미지 생성 후만 노출)
 *  - source filter 옵션 (sourceTypes 있으면 IN 절 추가)
 *
 * Projection은 entity Slice로 — service 레이어가 DTO 변환 (likedByCurrentUser 등 viewer-aware 필드 추가).
 */
@Repository
@RequiredArgsConstructor
public class DevUserRecipesQueryRepositoryImpl implements DevUserRecipesQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Override
    public Page<Recipe> findUserRecipesAccessible(Long targetUserId,
                                                   @Nullable Long viewerId,
                                                   @Nullable List<RecipeSourceType> sourceTypes,
                                                   Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        BooleanExpression whereClause = recipe.user.id.eq(targetUserId)
                .and(DevRecipeQueryPredicates.accessibleBy(recipe, viewerId))
                // 운영 "completed recipes" 조건: AI는 imageKey 없으면 noisy(생성 중) → 차단
                .and(recipe.isAiGenerated.isFalse().or(recipe.imageKey.isNotNull()))
                // imageReady — A2/A3 정합. PENDING/FAILED는 dev 노출 차단 (인터페이스 javadoc 정합).
                .and(recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()));

        if (sourceTypes != null && !sourceTypes.isEmpty()) {
            whereClause = whereClause.and(recipe.source.in(sourceTypes));
        }

        List<Recipe> content = queryFactory
                .selectFrom(recipe)
                .leftJoin(recipe.user, com.jdc.recipe_service.domain.entity.QUser.user).fetchJoin()
                .where(whereClause)
                .orderBy(getOrderSpecifiers(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        Long total = queryFactory
                .select(recipe.count())
                .from(recipe)
                .where(whereClause)
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }

    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;
        if (pageable.getSort().isEmpty()) {
            return new OrderSpecifier<?>[]{recipe.createdAt.desc()};
        }
        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.isAscending() ? Order.ASC : Order.DESC;
                    return switch (o.getProperty()) {
                        case "cookingTime" -> new OrderSpecifier<>(dir, recipe.cookingTime);
                        default -> new OrderSpecifier<>(dir, recipe.createdAt);
                    };
                })
                .toArray(OrderSpecifier[]::new);
    }
}
