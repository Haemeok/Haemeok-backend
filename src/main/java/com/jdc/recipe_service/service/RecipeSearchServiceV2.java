package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.entity.QRecipeTag;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.mapper.UserMapper;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import com.jdc.recipe_service.util.SearchProperties;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeSearchServiceV2 {

    private final OpenSearchService openSearchService;
    private final SearchProperties searchProperties;
    private final RestHighLevelClient client;
    private static final Logger log = LoggerFactory.getLogger(RecipeSearchServiceV2.class);

    private final RecipeRepository recipeRepository;
    private final RecipeQueryRepositoryV2 recipeQueryRepositoryV2;
    private final JPAQueryFactory queryFactory;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeTagRepository recipeTagRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;
    @Value("${cloud.aws.region.static}")
    private String region;
    private volatile boolean isOpenSearchHealthy = true;

    public String generateImageUrl(String key) {
        return key == null ? null : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Transactional(readOnly = true)
    public RecipeDetailStaticDto getRecipeDetail(Long recipeId, Long currentUserId) {
        Recipe basic = recipeRepository.findWithUserById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (basic.getIsPrivate() && (currentUserId == null || !basic.getUser().getId().equals(currentUserId))) {
            throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
        }

        UserDto authorDto = UserMapper.toSimpleDto(basic.getUser());
        List<String> tagNames = recipeTagRepository.findByRecipeId(recipeId)
                .stream()
                .map(rt -> rt.getTag().getDisplayName())
                .toList();

        List<RecipeIngredientDto> ingredients = RecipeIngredientMapper.toDtoList(
                recipeIngredientRepository.findByRecipeId(recipeId)
        );

        double rawTotal = ingredients.stream()
                .mapToDouble(i -> i.getCalories() != null ? i.getCalories() : 0.0)
                .sum();
        double totalCalories = BigDecimal
                .valueOf(rawTotal)
                .setScale(2, RoundingMode.DOWN)
                .doubleValue();

        List<RecipeStepDto> steps = recipeStepRepository
                .findWithIngredientsByRecipeIdOrderByStepNumber(recipeId)
                .stream()
                .map(step -> {
                    var used = StepIngredientMapper.toDtoList(step.getStepIngredients());
                    var url  = generateImageUrl(step.getImageKey());
                    return RecipeStepMapper.toDto(step, used, url, step.getImageKey());
                })
                .toList();

        int totalCost   = basic.getTotalIngredientCost() != null ? basic.getTotalIngredientCost() : 0;
        int marketPrice = basic.getMarketPrice() != null ? basic.getMarketPrice() : 0;
        int savings     = marketPrice - totalCost;

        return RecipeDetailStaticDto.builder()
                .id(recipeId)
                .title(basic.getTitle())
                .dishType(basic.getDishType().getDisplayName())
                .description(basic.getDescription())
                .cookingTime(basic.getCookingTime())
                .imageUrl(generateImageUrl(basic.getImageKey()))
                .imageKey(basic.getImageKey())
                .imageStatus(basic.getImageStatus() != null ? basic.getImageStatus().name() : null)
                .youtubeUrl(basic.getYoutubeUrl())
                .cookingTools(new ArrayList<>(basic.getCookingTools()))
                .servings(basic.getServings())
                .isPrivate(basic.getIsPrivate())
                .isAiGenerated(basic.isAiGenerated())
                .author(authorDto)
                .tags(tagNames)
                .ingredients(ingredients)
                .steps(steps)
                .totalIngredientCost(totalCost)
                .totalCalories(totalCalories)
                .marketPrice(marketPrice)
                .savings(savings)
                .createdAt(basic.getCreatedAt())
                .updatedAt(basic.getUpdatedAt())
                .build();
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleStaticDto> searchRecipes(RecipeSearchCondition condition, Pageable pageable, Long userId) {
        if (shouldUseOpenSearch(condition)) {
            log.info("V2 API: Using OpenSearch");
            return openSearchService.searchRecipesV2(condition, pageable, userId);
        } else {
            log.info("V2 API: Using QueryDSL");
            return searchWithQuerydslV2(condition, pageable, userId);
        }
    }

    private Page<RecipeSimpleStaticDto> searchWithQuerydslV2(RecipeSearchCondition condition, Pageable pageable, Long userId) {
        Sort.Order likeSort = pageable.getSort().getOrderFor("likeCount");
        Sort.Order ratingSort = pageable.getSort().getOrderFor("avgRating");

        if (likeSort != null) {
            return searchRecipesSortedByDynamicField(condition, pageable, "likeCount", likeSort.getDirection());
        }
        if (ratingSort != null) {
            return searchRecipesSortedByDynamicField(condition, pageable, "avgRating", ratingSort.getDirection());
        }

        return recipeQueryRepositoryV2.searchStatic(
                condition.getTitle(),
                condition.getDishTypeEnum(),
                condition.getTagEnums(),
                condition.getIsAiGenerated(),
                pageable,
                userId
        );
    }

    private Page<RecipeSimpleStaticDto> searchRecipesSortedByDynamicField(RecipeSearchCondition cond, Pageable pageable, String property, Sort.Direction direction) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;
        BooleanExpression whereClause = createWhereClauseForSearch(cond);
        OrderSpecifier<?> orderSpecifier = "likeCount".equals(property)
                ? (direction == Sort.Direction.ASC ? recipe.likes.size().asc() : recipe.likes.size().desc())
                : (direction == Sort.Direction.ASC ? recipe.avgRating.asc() : recipe.avgRating.desc());

        List<Long> sortedIds = queryFactory.select(recipe.id).from(recipe)
                .leftJoin(recipe.tags, tag).where(whereClause)
                .groupBy(recipe.id).orderBy(orderSpecifier, recipe.createdAt.desc())
                .offset(pageable.getOffset()).limit(pageable.getPageSize()).fetch();

        if (sortedIds.isEmpty()) {
            return Page.empty(pageable);
        }
        List<RecipeSimpleStaticDto> content = recipeRepository.findAllSimpleStaticByIds(sortedIds);
        Map<Long, RecipeSimpleStaticDto> contentMap = content.stream().collect(Collectors.toMap(RecipeSimpleStaticDto::getId, dto -> dto));
        List<RecipeSimpleStaticDto> sortedContent = sortedIds.stream().map(contentMap::get).filter(Objects::nonNull).collect(Collectors.toList());

        Long total = queryFactory.select(recipe.id.countDistinct()).from(recipe)
                .leftJoin(recipe.tags, tag).where(whereClause).fetchOne();

        return new PageImpl<>(sortedContent, pageable, total != null ? total : 0);
    }

    private BooleanExpression createWhereClauseForSearch(RecipeSearchCondition cond) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;
        BooleanExpression privacy = recipe.isPrivate.eq(false);
        BooleanExpression ai = (cond.getIsAiGenerated() != null) ? QRecipe.recipe.isAiGenerated.eq(cond.getIsAiGenerated()) : null;
        BooleanExpression title = StringUtils.hasText(cond.getTitle()) ? recipe.title.containsIgnoreCase(cond.getTitle()) : null;
        BooleanExpression dishType = (cond.getDishTypeEnum() != null) ? recipe.dishType.eq(cond.getDishTypeEnum()) : null;
        BooleanExpression tags = (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) ? tag.tag.in(cond.getTagEnums()) : null;
        return privacy.and(ai).and(title).and(dishType).and(tags);
    }

    @Scheduled(initialDelay = 5000, fixedRate = 10000)
    public void checkOpenSearchHealth() {
        boolean currentHealth;
        try {
            currentHealth = client.ping(RequestOptions.DEFAULT);
        } catch (Exception e) {
            currentHealth = false;
        }
        if (this.isOpenSearchHealthy != currentHealth) {
            log.info("OpenSearch Health Status Changed: {} -> {}", this.isOpenSearchHealthy, currentHealth);
            this.isOpenSearchHealthy = currentHealth;
        }
    }

    private boolean shouldUseOpenSearch(RecipeSearchCondition cond) {
        String engine = searchProperties.getEngine();
        if ("opensearch".equalsIgnoreCase(engine)) return true;
        if ("querydsl".equalsIgnoreCase(engine)) return false;
        if (this.isOpenSearchHealthy) {
            return cond.getTitle() != null && !cond.getTitle().isBlank();
        }
        return false;
    }
}