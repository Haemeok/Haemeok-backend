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
 * <p>V1.x 정책 — viewer 분기:
 * <ul>
 *   <li>viewer == owner (내 프로필) → {@link DevRecipeQueryPredicates#ownerVisible}: ACTIVE 자기 거 모두</li>
 *   <li>viewer != owner (타인 프로필) → {@link DevRecipeQueryPredicates#publicListedActive}: discovery 정책 (link-only PUBLIC+UNLISTED도 타인 프로필에 노출되지 않음)</li>
 * </ul>
 * 추가 조건:
 * <ul>
 *   <li>{@code recipe.user.id = targetUserId} 기본</li>
 *   <li>{@code AI && imageKey IS NULL} 차단 (운영 "completed recipes" 조건 동일 — AI는 이미지 생성 후만 노출)</li>
 *   <li>source filter 옵션 (sourceTypes 있으면 IN 절 추가)</li>
 * </ul>
 *
 * <p>Projection은 entity Slice로 — service 레이어가 DTO 변환 (likedByCurrentUser 등 viewer-aware 필드 추가).
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
        boolean ownerView = viewerId != null && viewerId.equals(targetUserId);

        // 내 프로필이면 자기 모든 ACTIVE 글을 봐야 하고 (PRIVATE/UNLISTED 포함),
        // 타인 프로필이면 discovery 정책 — link-only(PUBLIC+UNLISTED)도 타인 프로필에 안 나오게 publicListedActive 사용.
        BooleanExpression accessClause = ownerView
                ? DevRecipeQueryPredicates.ownerVisible(recipe, viewerId)
                : DevRecipeQueryPredicates.publicListedActive(recipe);

        BooleanExpression whereClause = recipe.user.id.eq(targetUserId).and(accessClause);

        // Owner should see in-progress/failed image generation rows in "my recipes".
        // Public profile viewers only see displayable recipes.
        if (!ownerView) {
            whereClause = whereClause
                    .and(recipe.isAiGenerated.isFalse().or(recipe.imageKey.isNotNull()))
                    .and(recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()));
        }

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
