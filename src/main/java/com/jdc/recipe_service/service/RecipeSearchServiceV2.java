package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
import com.jdc.recipe_service.domain.dto.v2.comment.CommentStaticDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.dto.v2.rating.RecipeRatingInfoStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.mapper.UserMapper;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import com.jdc.recipe_service.util.SearchProperties;
import com.querydsl.core.BooleanBuilder;
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
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
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final CommentService commentService;

    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;

    @Value("${app.s3.bucket-name}")
    private String bucketName;
    @Value("${cloud.aws.region.static}")
    private String region;
    private volatile boolean isOpenSearchHealthy = true;

    public String generateImageUrl(String key) {
        if (key == null) {
            return null;
        }
        if (key.startsWith("http")) {
            return key;
        }

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    /**
     * V2 레시피 상세 조회 (정적 정보 + 공통 동적 정보만 제공)
     * 사용자 특정 동적 정보(좋아요/즐겨찾기 여부, 나의 평점 등)는 RecipeStatusService를 통해 별도로 조회해야 합니다.
     *
     * @param recipeId 레시피 ID
     * @param currentUserId 현재 로그인한 사용자 ID (비공개 레시피 접근 제어용)
     * @return RecipeDetailStaticDto (정적 DTO)
     */
    @Transactional
    public RecipeDetailStaticDto getRecipeDetail(Long recipeId, Long currentUserId) {

        log.info("Detail access for RecipeId: {}, CurrentUserId: {}", recipeId, currentUserId);

        Recipe basic = recipeRepository.findDetailWithFineDiningById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (Boolean.TRUE.equals(basic.getIsPrivate())) {
            if (currentUserId == null || !basic.getUser().getId().equals(currentUserId)) {
                throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
            }
        }
        UserDto authorDto = UserMapper.toSimpleDto(basic.getUser());
        List<String> tags = recipeTagRepository.findByRecipeId(recipeId)
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

        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);

        List<Long> stepIds = steps.stream().map(RecipeStep::getId).toList();
        Map<Long, List<RecipeStepIngredient>> ingredientsMap = recipeStepIngredientRepository
                .findByStepIdIn(stepIds)
                .stream()
                .collect(Collectors.groupingBy(rsi -> rsi.getStep().getId()));

        List<RecipeStepDto> stepDtos = steps.stream()
                .map(step -> {
                    List<RecipeStepIngredient> stepIngs = ingredientsMap.getOrDefault(step.getId(), List.of());
                    var used = StepIngredientMapper.toDtoList(stepIngs);
                    var url = generateImageUrl(step.getImageKey());
                    return RecipeStepMapper.toDto(step, used, url, step.getImageKey());
                })
                .toList();

        int totalCost   = basic.getTotalIngredientCost() != null ? basic.getTotalIngredientCost() : 0;
        int marketPrice = basic.getMarketPrice() != null ? basic.getMarketPrice() : 0;
        int savings     = marketPrice - totalCost;

        RecipeNutritionDto nutrition = RecipeNutritionDto.builder()
                .protein(basic.getProtein())
                .carbohydrate(basic.getCarbohydrate())
                .fat(basic.getFat())
                .sugar(basic.getSugar())
                .sodium(basic.getSodium())
                .build();

        List<Long> idList = Collections.singletonList(recipeId);
        Map<Long, Long> ratingCounts = recipeRepository.findRatingCountsMapByIds(idList);
        Map<Long, Double> avgRatings = recipeRepository.findAvgRatingsMapByIds(idList);
        Map<Long, Long> commentCounts = recipeRepository.findCommentCountsMapByIds(idList);

        BigDecimal avgRating = BigDecimal.valueOf(avgRatings.getOrDefault(recipeId, 0.0))
                .setScale(2, RoundingMode.HALF_UP);

        RecipeRatingInfoStaticDto ratingInfo = RecipeRatingInfoStaticDto.builder()
                .avgRating(avgRating)
                .ratingCount(ratingCounts.getOrDefault(recipeId, 0L))
                .build();

        List<CommentStaticDto> comments = commentService.getCommentsStaticForRecipeDetail(recipeId);

        RecipeDetailStaticDto.FineDiningInfo fineDiningInfo = null;

        if (basic.getFineDiningDetails() != null) {
            var details = basic.getFineDiningDetails();

            fineDiningInfo = RecipeDetailStaticDto.FineDiningInfo.builder()
                    .components(details.getComponents().stream()
                            .map(c -> RecipeDetailStaticDto.FineDiningComponentDto.builder()
                                    .role(c.getRole())
                                    .name(c.getName())
                                    .description(c.getDescription())
                                    .build())
                            .toList())
                    .plating(RecipeDetailStaticDto.FineDiningPlatingDto.builder()
                            .vessel(details.getPlatingVessel())
                            .guide(details.getPlatingGuide())
                            .build())
                    .build();
        }

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
                .youtubeChannelName(basic.getYoutubeChannelName())
                .youtubeVideoTitle(basic.getYoutubeVideoTitle())
                .youtubeThumbnailUrl(basic.getYoutubeThumbnailUrl())
                .cookingTools(new ArrayList<>(basic.getCookingTools()))
                .servings(basic.getServings())
                .isPrivate(basic.getIsPrivate())
                .isAiGenerated(basic.isAiGenerated())
                .author(authorDto)
                .ratingInfo(ratingInfo)
                .tags(tags)
                .ingredients(ingredients)
                .steps(stepDtos)
                .comments(comments)
                .commentCount(commentCounts.getOrDefault(recipeId, 0L))
                .totalIngredientCost(totalCost)
                .totalCalories(totalCalories)
                .marketPrice(marketPrice)
                .savings(savings)
                .cookingTips(basic.getCookingTips())
                .nutrition(nutrition)
                .fineDiningInfo(fineDiningInfo)
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
                condition,
                pageable,
                userId
        );
    }

    private Page<RecipeSimpleStaticDto> searchRecipesSortedByDynamicField(RecipeSearchCondition cond, Pageable pageable, String property, Sort.Direction direction) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;
        BooleanExpression whereClause = createWhereClauseForSearch(cond);
        OrderSpecifier<?> orderSpecifier = "likeCount".equals(property)
                ? (direction == Sort.Direction.ASC ? recipe.likeCount.asc() : recipe.likeCount.desc())
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

        sortedContent.forEach(dto -> {
            dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
        });

        Long total = queryFactory.select(recipe.id.countDistinct()).from(recipe)
                .leftJoin(recipe.tags, tag).where(whereClause).fetchOne();

        return new PageImpl<>(sortedContent, pageable, total != null ? total : 0);
    }

    private BooleanExpression createWhereClauseForSearch(RecipeSearchCondition cond) {
        QRecipe recipe = QRecipe.recipe;
        QRecipeTag tag = QRecipeTag.recipeTag;
        BooleanExpression privacy = recipe.isPrivate.eq(false);
        BooleanBuilder typeFilter = filterByTypes(cond.getTypes());
        BooleanExpression title = StringUtils.hasText(cond.getTitle()) ? recipe.title.containsIgnoreCase(cond.getTitle()) : null;
        BooleanExpression dishType = (cond.getDishTypeEnum() != null) ? recipe.dishType.eq(cond.getDishTypeEnum()) : null;
        BooleanExpression tags = (cond.getTagEnums() != null && !cond.getTagEnums().isEmpty()) ? tag.tag.in(cond.getTagEnums()) : null;

        BooleanExpression cost = (cond.getMaxCost() != null) ? recipe.totalIngredientCost.loe(cond.getMaxCost()) : null;
        BooleanExpression calories = (cond.getMaxCalories() != null) ? recipe.totalCalories.loe(BigDecimal.valueOf(cond.getMaxCalories())) : null;
        BooleanExpression protein = (cond.getMaxProtein() != null) ? recipe.protein.loe(BigDecimal.valueOf(cond.getMaxProtein())) : null;
        BooleanExpression carb = (cond.getMaxCarb() != null) ? recipe.carbohydrate.loe(BigDecimal.valueOf(cond.getMaxCarb())) : null;
        BooleanExpression fat = (cond.getMaxFat() != null) ? recipe.fat.loe(BigDecimal.valueOf(cond.getMaxFat())) : null;
        BooleanExpression sugar = (cond.getMaxSugar() != null) ? recipe.sugar.loe(BigDecimal.valueOf(cond.getMaxSugar())) : null;
        BooleanExpression sodium = (cond.getMaxSodium() != null) ? recipe.sodium.loe(BigDecimal.valueOf(cond.getMaxSodium())) : null;

        return privacy
                .and(typeFilter)
                .and(title)
                .and(dishType)
                .and(tags)
                .and(cost)
                .and(calories)
                .and(protein)
                .and(carb)
                .and(fat)
                .and(sugar)
                .and(sodium);
    }

    /**
     * 기간별 인기 레시피 목록 조회 (정적 정보)
     * V2 원칙: 동적 정보(좋아요 수, 좋아요 여부)를 DTO에 포함하지 않고,
     * 오직 정적 정보인 RecipeSimpleStaticDto만 반환합니다.
     */
    @Transactional(readOnly = true)
    public Page<RecipeSimpleStaticDto> getPopularRecipesStaticV2(
            String periodCode,
            Pageable pageable) {

        java.time.LocalDateTime startDate = java.time.LocalDateTime.now().minusDays(7);

        Page<RecipeSimpleStaticDto> page = recipeRepository.findPopularRecipesStaticV2(startDate, pageable);

        page.getContent().forEach(dto -> {
            dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
        });

        return page;
    }

    /**
     * 원가 기준 예산 레시피 목록 조회 (정적 정보)
     * V2 원칙: 동적 정보(좋아요 수, 좋아요 여부)를 DTO에 포함하지 않고,
     * 오직 정적 정보인 RecipeSimpleStaticDtoV2만 반환합니다.
     */
    @Transactional(readOnly = true)
    public Page<RecipeSimpleStaticDtoV2> getBudgetRecipesStaticV2(
            Integer maxCost,
            Pageable pageable) {

        if (maxCost == null || maxCost < 0) {
            maxCost = Integer.MAX_VALUE;
        }

        Page<RecipeSimpleStaticDtoV2> page = recipeRepository.findBudgetRecipesStaticV2(maxCost, pageable);

        page.getContent().forEach(dto -> {
            dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
        });

        return page;
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

    private BooleanBuilder filterByTypes(List<RecipeType> types) {
        if (types == null || types.isEmpty()) {
            return null;
        }
        if (types.size() == RecipeType.values().length) {
            return null;
        }

        BooleanBuilder builder = new BooleanBuilder();
        QRecipe recipe = QRecipe.recipe;

        for (RecipeType type : types) {
            switch (type) {
                case AI:
                    builder.or(recipe.isAiGenerated.isTrue());
                    break;
                case YOUTUBE:
                    builder.or(recipe.youtubeUrl.isNotNull().and(recipe.youtubeUrl.ne("")));
                    break;
                case USER:
                    builder.or(recipe.isAiGenerated.isFalse()
                            .and(recipe.youtubeUrl.isNull().or(recipe.youtubeUrl.eq(""))));
                    break;
            }
        }
        return builder;
    }
}