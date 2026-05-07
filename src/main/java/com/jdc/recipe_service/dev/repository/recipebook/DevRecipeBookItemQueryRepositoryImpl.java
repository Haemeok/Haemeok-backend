package com.jdc.recipe_service.dev.repository.recipebook;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryPredicates;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeBookItem;
import com.jdc.recipe_service.domain.entity.QUser;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.querydsl.core.Tuple;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 운영 {@code RecipeBookItemRepository}의 dev 미러 (QueryDSL).
 *
 * <p>V1.x 정책: 레시피북에 저장된 항목 조회는 viewable — PUBLIC+UNLISTED(link-only)도 보여야 함. listingStatus 무시.
 * isPrivate 단일 필터 대신 {@code viewableBy(recipe, userId)} + {@code imageReady} 적용.
 *
 * <p>Fetch 전략: {@code select(item).from(item).join(item.recipe, recipe).join(recipe.user, user).fetchJoin()}.
 * item이 select 대상, recipe와 recipe.user는 후속 변환에서 사용되므로 모두 fetch (N+1 차단).
 */
@Repository
@RequiredArgsConstructor
public class DevRecipeBookItemQueryRepositoryImpl implements DevRecipeBookItemQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Override
    public Slice<RecipeBookItem> findAccessibleDevByBookIdAndUserId(Long bookId, Long userId, Pageable pageable) {
        QRecipeBookItem item = QRecipeBookItem.recipeBookItem;
        QRecipe recipe = QRecipe.recipe;
        QUser user = QUser.user;

        // book ownership guard: 다른 사람 bookId가 들어와도 빈 결과 (defense in depth — 호출자 ownership check가 잊혀도 누수 없음)
        BooleanExpression whereClause = item.book.id.eq(bookId)
                .and(item.book.user.id.eq(userId))
                .and(DevRecipeQueryPredicates.viewableBy(recipe, userId))
                .and(recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull()));

        int pageSize = pageable.getPageSize();
        List<RecipeBookItem> content = queryFactory
                .select(item)
                .from(item)
                .join(item.recipe, recipe).fetchJoin()
                .join(recipe.user, user).fetchJoin()
                .where(whereClause)
                .orderBy(getOrderSpecifiers(pageable))
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

    @Override
    public int countAccessibleDevByBookIdAndUserId(Long bookId, Long userId) {
        QRecipeBookItem item = QRecipeBookItem.recipeBookItem;
        QRecipe recipe = QRecipe.recipe;

        Long count = queryFactory
                .select(item.count())
                .from(item)
                .join(item.recipe, recipe)
                .where(
                        item.book.id.eq(bookId),
                        // ownership guard (footgun 방지) — find와 일관
                        item.book.user.id.eq(userId),
                        DevRecipeQueryPredicates.viewableBy(recipe, userId),
                        recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull())
                )
                .fetchOne();
        return count != null ? count.intValue() : 0;
    }

    @Override
    public Map<Long, Integer> countAccessibleDevByUserIdGroupByBookId(Long userId) {
        QRecipeBookItem item = QRecipeBookItem.recipeBookItem;
        QRecipe recipe = QRecipe.recipe;

        List<Tuple> rows = queryFactory
                .select(item.book.id, item.count())
                .from(item)
                .join(item.recipe, recipe)
                .where(
                        item.book.user.id.eq(userId),
                        DevRecipeQueryPredicates.viewableBy(recipe, userId),
                        recipe.imageStatus.eq(RecipeImageStatus.READY).or(recipe.imageStatus.isNull())
                )
                .groupBy(item.book.id)
                .fetch();

        Map<Long, Integer> result = new HashMap<>();
        for (Tuple row : rows) {
            Long bookId = row.get(item.book.id);
            Long cnt = row.get(item.count());
            if (bookId != null && cnt != null) {
                result.put(bookId, cnt.intValue());
            }
        }
        return result;
    }

    private OrderSpecifier<?>[] getOrderSpecifiers(Pageable pageable) {
        QRecipeBookItem item = QRecipeBookItem.recipeBookItem;
        QRecipe recipe = QRecipe.recipe;
        if (pageable.getSort().isEmpty()) {
            // default: 책에 추가된 시각 기준 최신 (favorite 패턴 정합)
            return new OrderSpecifier<?>[]{item.createdAt.desc()};
        }
        return pageable.getSort().stream()
                .map(o -> {
                    Order dir = o.isAscending() ? Order.ASC : Order.DESC;
                    return switch (o.getProperty()) {
                        case "cookingTime" -> new OrderSpecifier<>(dir, recipe.cookingTime);
                        case "recipeCreatedAt" -> new OrderSpecifier<>(dir, recipe.createdAt);
                        // default("createdAt"): item.createdAt — 책 추가 시각
                        default -> new OrderSpecifier<>(dir, item.createdAt);
                    };
                })
                .toArray(OrderSpecifier[]::new);
    }
}
