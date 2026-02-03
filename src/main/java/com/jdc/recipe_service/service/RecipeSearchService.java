package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingInfoDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.PopularityPeriod;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.mapper.UserMapper;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import com.jdc.recipe_service.util.SearchProperties;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeSearchService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeTagRepository recipeTagRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final RecipeIngredientReportRepository recipeIngredientReportRepository;
    private final Hashids hashids;

    private final RecipeRatingService recipeRatingService;
    private final CommentService commentService;
    private final OpenSearchService openSearchService;

    private final SearchProperties searchProperties;
    private static final Logger log = LoggerFactory.getLogger(RecipeSearchService.class);
    private final RestHighLevelClient client;
    private volatile boolean isOpenSearchHealthy = true;

    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        if (key == null) {
            return null;
        }
        if (key.startsWith("http")) {
            return key;
        }

        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    public String generateDetailImageUrl(String key) {
        String listUrl = generateImageUrl(key);
        if (listUrl == null) return null;

        return listUrl.replace(".webp", "_detail.webp");
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchRecipes(RecipeSearchCondition condition, Pageable pageable, Long userId) {

        Sort.Order likeSort = pageable.getSort().getOrderFor("likeCount");
        Sort.Order ratingSort = pageable.getSort().getOrderFor("avgRating");

        if (likeSort != null) {
            return searchWithQuerydslSortedByDynamicField(condition, pageable, "likeCount", likeSort.getDirection(), userId);
        }
        if (ratingSort != null) {
            return searchWithQuerydslSortedByDynamicField(condition, pageable, "avgRating", ratingSort.getDirection(), userId);
        }

        boolean useOpenSearch = shouldUseOpenSearch(condition);

        if (useOpenSearch) {
            Page<RecipeSimpleDto> page = openSearchService.searchRecipes(condition, pageable, userId);
            return addLikeInfoToPage(page, userId);
        } else {
            if (condition.getIngredientIds() != null && !condition.getIngredientIds().isEmpty()) {
                return searchByIncludedIngredients(condition, pageable, userId);
            } else {
                return searchWithQuerydsl(condition, pageable, userId);
            }
        }
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchWithQuerydslSortedByDynamicField(
            RecipeSearchCondition cond,
            Pageable pageable,
            String property,
            Sort.Direction direction,
            Long userId) {


        Page<RecipeSimpleDto> page = recipeRepository.searchAndSortByDynamicField(
                cond,
                property,
                direction,
                pageable,
                userId
        );

        return addLikeInfoToPage(page, userId);
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchByIncludedIngredients(
            RecipeSearchCondition cond,
            Pageable pageable,
            Long userId
    ) {
        List<Long> decodedIds = cond.getDecodedIngredientIds(hashids);

        if (decodedIds.isEmpty()) {
            return Page.empty(pageable);
        }

        Page<RecipeSimpleDto> page = recipeRepository.searchByIncludedIngredients(decodedIds, cond, pageable);

        return addLikeInfoToPage(page, userId);
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> searchWithQuerydsl(RecipeSearchCondition condition, Pageable pageable, Long userId) {

        return recipeRepository.search(condition, pageable, userId);
    }

    @Transactional
    public RecipeDetailDto getRecipeDetail(Long recipeId, Long currentUserId) {

        log.info("V1 Detail access for RecipeId: {}, CurrentUserId: {}", recipeId, currentUserId);

        Recipe basic = recipeRepository.findDetailWithFineDiningById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (Boolean.TRUE.equals(basic.getIsPrivate())) {
            if (currentUserId == null) {
                throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
            }
            if (!basic.getUser().getId().equals(currentUserId)) {
                throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
            }
        }

        long likeCount = basic.getLikeCount() != null ? basic.getLikeCount() : 0L;
        boolean likedByUser = currentUserId != null &&
                recipeLikeRepository.existsByRecipeIdAndUserId(recipeId, currentUserId);
        boolean favoritedByUser = currentUserId != null &&
                recipeFavoriteRepository.existsByRecipeIdAndUserId(recipeId, currentUserId);

        RecipeRatingInfoDto ratingInfo = RecipeRatingInfoDto.builder()
                .avgRating(basic.getAvgRating())
                .myRating(recipeRatingService.getMyRating(recipeId, currentUserId))
                .ratingCount(recipeRatingService.getRatingCount(recipeId))
                .build();

        UserDto authorDto = UserMapper.toSimpleDto(basic.getUser());

        List<String> tags = recipeTagRepository.findByRecipeId(recipeId)
                .stream()
                .map(rt -> rt.getTag().getDisplayName())
                .toList();

        List<RecipeIngredientDto> ingredients = RecipeIngredientMapper.toDtoList(
                recipeIngredientRepository.findByRecipeId(recipeId)
        );

        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);

        List<Long> stepIds = steps.stream().map(RecipeStep::getId).toList();
        Map<Long, List<RecipeStepIngredient>> ingredientsMap =
                stepIds.isEmpty()
                        ? Map.of()
                        : recipeStepIngredientRepository.findByStepIdIn(stepIds)
                        .stream()
                        .collect(Collectors.groupingBy(rsi -> rsi.getStep().getId()));

        List<RecipeStepDto> stepsDto = steps.stream()
                .map(step -> {
                    List<RecipeStepIngredient> stepIngs = ingredientsMap.getOrDefault(step.getId(), List.of());
                    var used = StepIngredientMapper.toDtoList(stepIngs);
                    var url = generateImageUrl(step.getImageKey());
                    return RecipeStepMapper.toDto(step, used, url, step.getImageKey());
                })
                .toList();

        List<CommentDto> comments = commentService.getTop3CommentsWithLikes(recipeId, currentUserId);
        long commentCount = recipeCommentRepository.countByRecipeId(recipeId);

        int totalCost   = basic.getTotalIngredientCost() != null ? basic.getTotalIngredientCost() : 0;
        int marketPrice = basic.getMarketPrice()               != null ? basic.getMarketPrice()   : 0;
        int savings     = marketPrice - totalCost;

        RecipeNutritionDto nutrition = RecipeNutritionDto.builder()
                .protein(basic.getProtein())
                .carbohydrate(basic.getCarbohydrate())
                .fat(basic.getFat())
                .sugar(basic.getSugar())
                .sodium(basic.getSodium())
                .build();

        List<String> tools = basic.getCookingTools() != null
                ? new ArrayList<>(basic.getCookingTools())
                : List.of();

        RecipeDetailDto.FineDiningInfo fineDiningInfo = null;

        if (basic.getFineDiningDetails() != null) {
            var details = basic.getFineDiningDetails();

            fineDiningInfo = RecipeDetailDto.FineDiningInfo.builder()
                    .components(details.getComponents().stream()
                            .map(c -> RecipeDetailDto.FineDiningComponentDto.builder()
                                    .role(c.getRole())
                                    .name(c.getName())
                                    .description(c.getDescription())
                                    .build())
                            .toList())
                    .plating(RecipeDetailDto.FineDiningPlatingDto.builder()
                            .vessel(details.getPlatingVessel())
                            .guide(details.getPlatingGuide())
                            .build())
                    .build();
        }

        List<Long> contributors = recipeIngredientReportRepository.findVerifiedMemberIds(recipeId);

        return RecipeDetailDto.builder()
                .id(recipeId)
                .title(basic.getTitle())
                .dishType(basic.getDishType() != null
                        ? basic.getDishType().getDisplayName()
                        : null)
                .description(basic.getDescription())
                .cookingTime(basic.getCookingTime())
                .ratingInfo(ratingInfo)
                .imageUrl(generateDetailImageUrl(basic.getImageKey()))
                .imageKey(basic.getImageKey())
                .imageStatus(basic.getImageStatus() != null
                        ? basic.getImageStatus().name() : null)
                .youtubeUrl(basic.getYoutubeUrl())
                .youtubeChannelName(basic.getYoutubeChannelName())
                .youtubeChannelId(basic.getYoutubeChannelId())
                .youtubeVideoTitle(basic.getYoutubeVideoTitle())
                .youtubeThumbnailUrl(basic.getYoutubeThumbnailUrl())
                .youtubeChannelProfileUrl(basic.getYoutubeChannelProfileUrl())
                .youtubeSubscriberCount(basic.getYoutubeSubscriberCount())
                .youtubeVideoViewCount(basic.getYoutubeVideoViewCount())
                .extractorId(basic.getExtractorId())
                .contributors(contributors)
                .cookingTools(tools)
                .servings(basic.getServings())
                .isPrivate(basic.getIsPrivate())
                .isAiGenerated(basic.isAiGenerated())
                .author(authorDto)
                .likeCount(likeCount)
                .likedByCurrentUser(likedByUser)
                .favoriteCount(basic.getFavoriteCount())
                .favoriteByCurrentUser(favoritedByUser)
                .tags(tags)
                .ingredients(ingredients)
                .totalCalories(basic.getTotalCalories() != null ? basic.getTotalCalories().doubleValue() : null)
                .steps(stepsDto)
                .comments(comments)
                .commentCount(commentCount)
                .totalIngredientCost(totalCost)
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
    public Page<RecipeSimpleDto> getPopularRecipes(
            String periodCode,
            Pageable pageable,
            Long currentUserId) {

        PopularityPeriod period = PopularityPeriod.fromCode(periodCode);
        LocalDateTime startDate = period.getStartDate().atStartOfDay();

        Page<RecipeSimpleDto> page = recipeRepository.findPopularRecipesSince(startDate, pageable);

        if (page.isEmpty()) {
            RecipeSearchCondition cond = new RecipeSearchCondition();
            page = recipeRepository.search(cond, pageable, currentUserId);
        }

        return addLikeInfoToPage(page, currentUserId);
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getBudgetRecipes(
            Integer maxCost,
            Pageable pageable,
            Long currentUserId) {

        if (maxCost == null || maxCost < 0) {
            maxCost = Integer.MAX_VALUE;
        }

        Page<RecipeSimpleDto> page = recipeRepository.findBudgetRecipes(maxCost, pageable);

        return addLikeInfoToPage(page, currentUserId);
    }

    protected Page<RecipeSimpleDto> addLikeInfoToPage(Page<RecipeSimpleDto> page, Long currentUserId) {
        if (page.isEmpty()) {
            return page;
        }

        page.getContent().forEach(dto -> {
            dto.setImageUrl(generateImageUrl(dto.getImageUrl()));
        });

        if (currentUserId == null) {
            return page;
        }

        List<Long> recipeIds = page.getContent().stream()
                .map(RecipeSimpleDto::getId)
                .toList();

        Set<Long> likedIds = recipeLikeRepository.findRecipeIdsByUserIdAndRecipeIdIn(currentUserId, recipeIds);

        page.getContent().forEach(dto -> {
            dto.setLikedByCurrentUser(likedIds.contains(dto.getId()));
        });

        return page;
    }

    @Scheduled(initialDelay = 5000, fixedRate = 10000)
    public void checkOpenSearchHealth() {
        boolean currentHealth;
        try {
            currentHealth = client.ping(RequestOptions.DEFAULT);

            if (!currentHealth && this.isOpenSearchHealthy) {
                log.warn("⚠️ OpenSearch PING 응답이 false입니다. 비정상 상태로 간주합니다.");
            }
        } catch (IOException e) {
            currentHealth = false;
            if (this.isOpenSearchHealthy) {
                log.warn("❌ OpenSearch PING 실패 (IOException), 서버 다운 또는 네트워크 문제일 수 있습니다: {}", e.getMessage());
            } else {
                log.debug("❌ OpenSearch PING이 계속 실패 중입니다 (IOException): {}", e.getMessage());
            }
        } catch (Exception e) {
            currentHealth = false;
            log.error("🚨 OpenSearch PING 중 예상치 못한 오류 발생: {}", e.getMessage(), e);
        }

        if (this.isOpenSearchHealthy != currentHealth) {
            log.info("🔄 OpenSearch 건강 상태 변경: {} -> {}", this.isOpenSearchHealthy ? "정상" : "비정상", currentHealth ? "정상" : "비정상");
            this.isOpenSearchHealthy = currentHealth;
        } else if (currentHealth) {
            log.debug("💚 OpenSearch 서버 상태 양호합니다 (변경 없음).");
        }
    }

    private boolean shouldUseOpenSearch(RecipeSearchCondition cond) {
        String engine = searchProperties.getEngine();

        if ("opensearch".equalsIgnoreCase(engine)) {
            log.info("🔍 OpenSearch 사용 (설정 파일에 명시됨)");
            return true;
        }
        if ("querydsl".equalsIgnoreCase(engine)) {
            log.info("🔍 QueryDSL 사용 (설정 파일에 명시됨)");
            return false;
        }

        if (this.isOpenSearchHealthy) {
            boolean hasKeyword = cond.getTitle() != null && !cond.getTitle().isBlank();
            boolean hasDishType = cond.getDishType() != null && !cond.getDishType().isBlank();
            boolean hasTags = cond.getTags() != null && !cond.getTags().isEmpty();

            if (hasKeyword || hasDishType || hasTags) {
                log.info("🔍 OpenSearch 사용 (서버 상태 양호, 검색 조건 존재 - 자동 모드)");
                return true;
            } else {
                log.info("🔍 QueryDSL 사용 (OpenSearch 서버 상태는 양호하나 검색 조건 없음 - 자동 모드)");
                return false;
            }
        } else {
            log.warn("⚠️ QueryDSL 사용 (OpenSearch 서버 상태 비정상 - 자동 모드)");
            return false;
        }
    }

    private void sortRecipes(List<RecipeSimpleDto> recipes, Pageable pageable) {
        if (pageable.getSort().isEmpty()) return;

        Sort.Order order = pageable.getSort().iterator().next();
        Comparator<RecipeSimpleDto> comparator = switch (order.getProperty()) {
            case "likeCount" -> Comparator.comparing(RecipeSimpleDto::getLikeCount);
            default -> Comparator.comparing(RecipeSimpleDto::getCreatedAt);
        };
        recipes.sort(order.isAscending() ? comparator : comparator.reversed());
    }

}
