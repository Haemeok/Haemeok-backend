package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.QRecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
import com.jdc.recipe_service.domain.type.TagType;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.StringUtils;

import java.util.List;

@RequiredArgsConstructor
public class RecipeQueryRepositoryImpl implements RecipeQueryRepository {

    private final JPAQueryFactory queryFactory;

    @Override
    public Page<RecipeSimpleDto> search(RecipeSearchCondition condition, Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag recipeTag = QRecipeTag.recipeTag;

        // ✅ 태그 조건이 있을 경우 join
        var query = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.nickname,
                        recipe.createdAt,
                        recipe.likes.size(),
                        Expressions.constant(false)
                ))
                .from(recipe)
                .leftJoin(recipe.tags, recipeTag)
                .where(
                        titleContains(condition.getTitle()),
                        dishTypeEq(condition.getDishType()),
                        tagNameIn(condition.getTagNames())
                )
                .distinct() // 중복 제거
                .orderBy(getOrderSpecifier(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<RecipeSimpleDto> content = query.fetch();

        Long total = queryFactory
                .select(recipe.countDistinct()) // 중복 방지
                .from(recipe)
                .leftJoin(recipe.tags, recipeTag)
                .where(
                        titleContains(condition.getTitle()),
                        dishTypeEq(condition.getDishType()),
                        tagNameIn(condition.getTagNames())
                )
                .fetchOne();

        return new PageImpl<>(content, pageable, total != null ? total : 0);
    }


    private OrderSpecifier<?> getOrderSpecifier(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        if (!pageable.getSort().isEmpty()) {
            Sort.Order order = pageable.getSort().iterator().next(); // 첫 번째만 사용
            Order direction = order.getDirection().isAscending() ? Order.ASC : Order.DESC;

            switch (order.getProperty()) {
                case "likeCount":
                    return new OrderSpecifier<>(direction, recipe.likes.size());
                case "createdAt":
                default:
                    return new OrderSpecifier<>(direction, recipe.createdAt);
            }
        }

        // 기본은 최신순
        return recipe.createdAt.desc();
    }

    private BooleanExpression tagNameIn(List<String> tagNames) {
        if (tagNames == null || tagNames.isEmpty()) return null;

        List<TagType> enums = tagNames.stream()
                .map(name -> {
                    try {
                        return TagType.valueOf(name); // 영어로 Enum 이름으로 매핑
                    } catch (IllegalArgumentException e) {
                        return null; // or throw new BadRequestException
                    }
                })
                .filter(t -> t != null)
                .toList();

        return enums.isEmpty() ? null : QRecipeTag.recipeTag.tag.in(enums);
    }


    private BooleanExpression titleContains(String title) {
        return StringUtils.hasText(title) ? QRecipe.recipe.title.containsIgnoreCase(title) : null;
    }

    private BooleanExpression dishTypeEq(String dishType) {
        return StringUtils.hasText(dishType) ? QRecipe.recipe.dishType.stringValue().eq(dishType) : null;
    }
}