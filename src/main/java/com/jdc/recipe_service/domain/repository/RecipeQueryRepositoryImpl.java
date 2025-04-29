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
import java.util.Set;
import java.util.stream.Collectors;

@RequiredArgsConstructor
public class RecipeQueryRepositoryImpl implements RecipeQueryRepository {

    private final JPAQueryFactory queryFactory;
    private final RecipeLikeRepository recipeLikeRepository;

    @Override
    public Page<RecipeSimpleDto> search(RecipeSearchCondition condition, Pageable pageable, Long currentUserId) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag recipeTag = QRecipeTag.recipeTag;

        var query = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
                        recipe.user.nickname,
                        recipe.user.profileImage,
                        recipe.createdAt,
                        recipe.likes.size().castToNum(Long.class),
                        Expressions.constant(false), // 초기값 false
                        recipe.cookingTime,
                        recipe.avgRating,
                        recipe.ratingCount.coalesce(0L)
                ))
                .from(recipe)
                .leftJoin(recipe.tags, recipeTag)
                .where(
                        titleContains(condition.getTitle()),
                        dishTypeEq(condition.getDishType()),
                        tagNameIn(condition.getTagNames())
                )
                .distinct()
                .orderBy(getOrderSpecifier(pageable))
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<RecipeSimpleDto> content = query.fetch();

        if (currentUserId != null && !content.isEmpty()) {
            List<Long> recipeIds = content.stream().map(RecipeSimpleDto::getId).toList();

            Set<Long> likedIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                    .stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());

            content.forEach(dto -> dto.setLikedByCurrentUser(likedIds.contains(dto.getId())));
        }

        Long total = queryFactory
                .select(recipe.countDistinct())
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


    @Override
    public Page<RecipeSimpleDto> findAllSimpleWithRatingAndCookingInfo(Pageable pageable) {
        QRecipe recipe = QRecipe.recipe;

        List<RecipeSimpleDto> content = queryFactory
                .select(new QRecipeSimpleDto(
                        recipe.id,
                        recipe.title,
                        recipe.imageKey,
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
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        Long total = queryFactory
                .select(recipe.count())
                .from(recipe)
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