package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.projection.RecipeSitemapProjection;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import jakarta.persistence.QueryHint;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository
public interface RecipeRepository extends JpaRepository<Recipe, Long>, RecipeQueryRepository {
    List<Recipe> findAllByUserId(Long userId);

    @Query("""
                SELECT r FROM Recipe r
                JOIN FETCH r.user
                WHERE r.id = :recipeId
            """)
    Optional<Recipe> findWithUserById(@Param("recipeId") Long recipeId);

    @EntityGraph(attributePaths = {
            "user",
            "fineDiningDetails",
            "ingredients",
            "ingredients.ingredient",
            "tags.tag"
    })
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.id = :recipeId
        """)
    Optional<Recipe> findDetailWithFineDiningById(@Param("recipeId") Long recipeId);

    @Query("""
                SELECT r FROM Recipe r
                LEFT JOIN FETCH r.steps
                WHERE r.id = :recipeId
            """)
    Optional<Recipe> findWithStepsById(@Param("recipeId") Long recipeId);

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    Page<Recipe> findByUserId(Long userId, Pageable pageable);

    /**
     * 타인 프로필의 사용자 레시피 목록 — V1.x discovery 정책 (ACTIVE + PUBLIC + LISTED).
     *
     * <p>Spring Data derived query({@code findByUserIdAndIsPrivateFalse})는 isPrivate=false만 보므로
     * PUBLIC+UNLISTED(link-only) 리믹스가 타인 프로필에 노출되는 누수가 있다. discovery 정책에 맞게
     * 명시적 {@code @Query}로 4-enum 필터 추가.
     */
    @EntityGraph(attributePaths = {"fineDiningDetails"})
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.user.id = :userId
              AND r.isPrivate = false
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
            """)
    Page<Recipe> findByUserIdAndIsPrivateFalse(@Param("userId") Long userId, Pageable pageable);

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.user.id = :userId
              AND r.isPrivate = false
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
              AND r.source IN :sources
            """)
    Page<Recipe> findByUserIdAndIsPrivateFalseAndSourceIn(
            @Param("userId") Long userId,
            @Param("sources") List<RecipeSourceType> sources,
            Pageable pageable);

    Optional<Recipe> findById(Long id);

    @EntityGraph(attributePaths = {
            "user",
            "tags.tag"
    })
    Optional<Recipe> findWithAllRelationsById(Long id);


    @Query("SELECT r.id, r.likeCount FROM Recipe r WHERE r.id IN :ids")
    List<Object[]> findLikeCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findLikeCountsMapByIds(List<Long> ids) {
        return findLikeCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Query("SELECT r.id, r.favoriteCount FROM Recipe r WHERE r.id IN :ids")
    List<Object[]> findFavoriteCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findFavoriteCountsMapByIds(List<Long> ids) {
        return findFavoriteCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Query("""
            SELECT r.id, COUNT(rc.id)
            FROM Recipe r
            LEFT JOIN RecipeComment rc ON rc.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findCommentCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findCommentCountsMapByIds(List<Long> ids) {
        return findCommentCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Query("""
            SELECT r.id, COALESCE(AVG(rr.rating), 0.0)
            FROM Recipe r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findAvgRatingsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Double> findAvgRatingsMapByIds(List<Long> ids) {
        return findAvgRatingsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Double) a[1] : 0.0
                ));
    }

    @Query("""
            SELECT r.id, COUNT(rr.id)
            FROM Recipe r
            LEFT JOIN RecipeRating rr ON rr.recipe = r
            WHERE r.id IN :ids
            GROUP BY r.id
            """)
    List<Object[]> findRatingCountsByIdsRaw(@Param("ids") List<Long> ids);

    default Map<Long, Long> findRatingCountsMapByIds(List<Long> ids) {
        return findRatingCountsByIdsRaw(ids).stream()
                .collect(Collectors.toMap(
                        a -> (Long) a[0],
                        a -> a[1] != null ? (Long) a[1] : 0L
                ));
    }

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.likeCount = r.likeCount + 1, r.popularityScore = r.popularityScore + 1 WHERE r.id = :id")
    void incrementLikeCount(@Param("id") Long id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.likeCount = GREATEST(r.likeCount - 1, 0), r.popularityScore = GREATEST(r.popularityScore - 1, 0) WHERE r.id = :id")
    void decrementLikeCount(@Param("id") Long id);

    @Modifying(clearAutomatically = true)
    @Query("""
        UPDATE Recipe r
        SET r.weeklyLikeCount = (
            SELECT COUNT(rl)
            FROM RecipeLike rl
            WHERE rl.recipe.id = r.id
              AND rl.createdAt >= :startDate
        )
    """)
    void updateAllWeeklyLikeCounts(@Param("startDate") java.time.LocalDateTime startDate);

    @Modifying(clearAutomatically = true)
    @Query("""
        UPDATE Recipe r
        SET r.weeklyFavoriteCount = (
            SELECT COUNT(rf)
            FROM RecipeFavorite rf    
            WHERE rf.recipe.id = r.id
              AND rf.createdAt >= :startDate
        )
    """)
    void updateAllWeeklyFavoriteCounts(@Param("startDate") java.time.LocalDateTime startDate);

    @SuppressWarnings("JpaQlInspection")
    @Query("""
        SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
            r.id,
            r.title,
            r.imageKey,
            r.user.id,
            r.user.nickname,
            r.user.profileImage,
            r.createdAt,
            r.cookingTime,
           COALESCE(r.likeCount, 0L),
            COALESCE(r.favoriteCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.youtubeChannelName,
            r.youtubeChannelId,
            r.youtubeVideoTitle,
            r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl,
            r.youtubeSubscriberCount,
            r.youtubeVideoViewCount,
            r.youtubeUrl,
            r.isAiGenerated
        )
        FROM Recipe r
        WHERE r.isPrivate = false
          AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
        ORDER BY (COALESCE(r.weeklyLikeCount, 0) + COALESCE(r.weeklyFavoriteCount, 0)) DESC, r.createdAt DESC
    """)
    Page<RecipeSimpleStaticDto> findPopularRecipesStaticV2(Pageable pageable);

    @SuppressWarnings("JpaQlInspection")
    @QueryHints({
            @QueryHint(name = "jakarta.persistence.cache.retrieveMode", value = "BYPASS")
    })
    @Query("""
                SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                    r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                    COALESCE(r.likeCount, 0L),
                    COALESCE(r.favoriteCount, 0L),
                    COALESCE(r.avgRating, 0.0),
                    COALESCE(r.ratingCount, 0L),
                    r.youtubeChannelName,
                    r.youtubeChannelId,
                    r.youtubeVideoTitle,
                    r.youtubeThumbnailUrl,
                    r.youtubeChannelProfileUrl,
                    r.youtubeSubscriberCount,
                    r.youtubeVideoViewCount,
                    r.youtubeUrl,
                    r.isAiGenerated
                )
                FROM Recipe r
                JOIN r.user u
                LEFT JOIN RecipeLike rl ON rl.recipe = r AND rl.createdAt >= :startDate
                WHERE r.isPrivate = false
                  AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
                  AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
                  AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
                  AND r.isAiGenerated = false
                GROUP BY
                   r.id, r.title, r.imageKey, u.id, u.nickname, u.profileImage, r.createdAt, r.cookingTime,
                   r.likeCount, r.favoriteCount, r.avgRating, r.ratingCount,
                   r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl,
                   r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount, r.youtubeUrl, r.isAiGenerated
                ORDER BY COUNT(DISTINCT rl.id) DESC, r.createdAt DESC
            """)
    Page<RecipeSimpleStaticDto> findPopularRecipesByRealtimeCount(
            @Param("startDate") LocalDateTime startDate,
            Pageable pageable);

    @Query("""
        SELECT r.id
        FROM Recipe r
        WHERE r.isPrivate = false
          AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
        ORDER BY (COALESCE(r.weeklyLikeCount, 0) + COALESCE(r.weeklyFavoriteCount, 0)) DESC, r.createdAt DESC
    """)
    List<Long> findTop10PopularRecipeIds(Pageable pageable);

    @SuppressWarnings("JpaQlInspection")
    @Query("""
        SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2(
            r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
            COALESCE(r.likeCount, 0L),
            COALESCE(r.favoriteCount, 0L),
            COALESCE(r.avgRating, 0.0),
            COALESCE(r.ratingCount, 0L),
            r.totalIngredientCost,
            r.marketPrice,
            r.youtubeChannelName,
            r.youtubeChannelId,
            r.youtubeVideoTitle,
            r.youtubeThumbnailUrl,
            r.youtubeChannelProfileUrl,
            r.youtubeSubscriberCount,
            r.youtubeVideoViewCount,
            r.youtubeUrl,
            r.isAiGenerated
        )
        FROM Recipe r
        JOIN r.user u
        WHERE r.isPrivate = false
          AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
          AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
          AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
          AND r.isAiGenerated = false
          AND r.totalIngredientCost <= :maxCost
          AND r.totalIngredientCost >= 1000
          AND r.id NOT IN :excludedIds
        ORDER BY 
            ( (COALESCE(r.favoriteCount, 0) * 0.3) + (COALESCE(r.weeklyFavoriteCount, 0) * 0.7) ) DESC,
            r.createdAt DESC
        """)
    Page<RecipeSimpleStaticDtoV2> findBudgetRecipesStaticV2(
            @Param("maxCost") Integer maxCost,
            @Param("excludedIds") List<Long> excludedIds,
            Pageable pageable);


    @SuppressWarnings("JpaQlInspection")
    @Query("""
            SELECT new com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto(
                r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.cookingTime,
                COALESCE(r.likeCount, 0L),
                COALESCE(r.favoriteCount, 0L),
                COALESCE(r.avgRating, 0.0),
                COALESCE(r.ratingCount, 0L),
                r.youtubeChannelName,
                r.youtubeChannelId,
                r.youtubeVideoTitle,
                r.youtubeThumbnailUrl,
                r.youtubeChannelProfileUrl,
                r.youtubeSubscriberCount,
                r.youtubeVideoViewCount,
                r.youtubeUrl,
                r.isAiGenerated
            )
            FROM Recipe r
            JOIN r.user u
            WHERE r.id IN :ids
            """)
    List<RecipeSimpleStaticDto> findAllSimpleStaticByIds(@Param("ids") List<Long> ids);

    @Query("""
            SELECT r
            FROM Recipe r
            JOIN FETCH r.user
            WHERE r.id IN :ids
              AND r.isPrivate = false
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
            """)
    List<Recipe> findAllByIdInAndIsPrivateFalseFetchUser(@Param("ids") List<Long> ids);

    @Query("SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(" +
            "r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.favoriteCount, " +
            "r.likeCount, FALSE, r.cookingTime, r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl, r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount, " +
            "COALESCE(ROUND(r.avgRating, 2), 0.0d), r.ratingCount, r.youtubeUrl, r.isAiGenerated)" +
            "FROM Recipe r " +
            "WHERE r.id IN :ids")
    List<RecipeSimpleDto> findAllSimpleDtoWithCountsByIdIn(@Param("ids") List<Long> ids);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.cookingTips = :tips, r.marketPrice = :price, r.aiAnalysisStatus = :status WHERE r.id = :id")
    void updateAiAnalysisResult(
            @Param("id") Long id,
            @Param("tips") String tips,
            @Param("price") Integer price,
            @Param("status") String status
    );

    @EntityGraph(attributePaths = {
            "user",
            "fineDiningDetails"
    })
    @Query("""
            SELECT r
            FROM Recipe r
            WHERE r.id IN :ids
            """)
    List<Recipe> findCandidatesForRecommendationByIds(@Param("ids") List<Long> ids);

    @EntityGraph(attributePaths = {
            "user",
            "tags",
            "tags.tag",
            "ingredients",
            "ingredients.ingredient",
            "fineDiningDetails"
    })
    @Query("""
            SELECT r
            FROM Recipe r
            WHERE r.id = :recipeId
            """)
    Optional<Recipe> findForRecommendationById(@Param("recipeId") Long recipeId);

    @Query(value = """
            SELECT r.id
            FROM recipes r
            WHERE r.is_private = false
              AND r.lifecycle_status = 'ACTIVE'
              AND r.visibility = 'PUBLIC'
              AND r.listing_status = 'LISTED'
              AND r.is_ai_generated = false
            ORDER BY RAND()
            LIMIT 100
            """, nativeQuery = true)
    List<Long> findRandomPublicRecipeIds();

    @Query("""
            SELECT r.id
            FROM Recipe r
            WHERE r.dishType IN :dishTypes
              AND r.isPrivate = false
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
              AND r.isAiGenerated = false
            """)
    List<Long> findIdsByDishTypeIn(@Param("dishTypes") List<DishType> dishTypes);

    @org.springframework.data.jpa.repository.Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM Recipe r WHERE r.id = :id")
    void deleteByIdDirectly(@Param("id") Long id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Recipe r SET r.imageKey = :imageKey, r.imageStatus = :status, r.isPrivate = :isPrivate WHERE r.id = :id")
    void updateImageInfo(@Param("id") Long id, @Param("imageKey") String imageKey,
                         @Param("status") RecipeImageStatus status, @Param("isPrivate") Boolean isPrivate);

    @Query("""
            SELECT r FROM Recipe r
            WHERE r.youtubeUrl = :youtubeUrl
              AND r.user.id = :officialUserId
              AND r.originRecipe IS NULL
            ORDER BY r.id ASC
            """)
    Optional<Recipe> findFirstOfficialByYoutubeUrl(
            @Param("youtubeUrl") String youtubeUrl,
            @Param("officialUserId") Long officialUserId);

    /**
     * URL probe로 official YOUTUBE 레시피의 존재 여부를 확인할 때 가시성 누설을 막기 위한 strict 변형.
     *
     * 같은 URL에 older PRIVATE/HIDDEN row + later PUBLIC ACTIVE row가 공존할 수 있으므로 Java 필터로 첫 row만 보면
     * 안 되고, SQL에서 4-enum + source + originRecipe IS NULL + imageStatus 조건을 모두 걸어야 한다.
     *
     * imageStatus는 dev V3 컨벤션을 따라 {@code READY OR NULL} (legacy null도 imageReady로 취급) — PENDING/FAILED만 차단.
     *
     * 호출자는 반드시 {@code PageRequest.of(0, 1)}을 넘기고 {@code stream().findFirst()}로 사용한다. {@code Pageable}
     * 없이 단순 {@code Optional} 반환을 쓰면 strict 통과 row가 2개 이상일 때 {@code IncorrectResultSizeDataAccessException}
     * 으로 500이 발생하므로 명시적 LIMIT이 필요하다.
     */
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.youtubeUrl = :youtubeUrl
              AND r.user.id = :officialUserId
              AND r.originRecipe IS NULL
              AND r.source = com.jdc.recipe_service.domain.type.recipe.RecipeSourceType.YOUTUBE
              AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE
              AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC
              AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED
              AND (r.imageStatus = com.jdc.recipe_service.domain.type.RecipeImageStatus.READY
                   OR r.imageStatus IS NULL)
            ORDER BY r.id ASC
            """)
    List<Recipe> findStrictPublicYoutubeRecipes(
            @Param("youtubeUrl") String youtubeUrl,
            @Param("officialUserId") Long officialUserId,
            org.springframework.data.domain.Pageable pageable);

    List<Recipe> findAllByYoutubeUrlIsNotNull();

    @Query("""
            SELECT r FROM Recipe r
            WHERE r.youtubeUrl IS NOT NULL
              AND (
                r.youtubeChannelName IS NULL OR r.youtubeChannelName = ''
                OR r.youtubeChannelProfileUrl IS NULL OR r.youtubeChannelProfileUrl = ''
                OR r.youtubeChannelId IS NULL OR r.youtubeChannelId = ''
              )
            """)
    List<Recipe> findAllWithNullYoutubeChannelInfo();

    @EntityGraph(attributePaths = {"fineDiningDetails"})
    @Query("""
            SELECT r FROM Recipe r
            WHERE r.user.id = :userId
              AND (r.isAiGenerated = false OR r.imageKey IS NOT NULL)
            """)
    Page<Recipe> findCompletedRecipesByUserId(@Param("userId") Long userId, Pageable pageable);

    @Query("""
            SELECT r FROM Recipe r
            WHERE r.user.id = :userId
              AND (r.isAiGenerated = false OR r.imageKey IS NOT NULL)
              AND r.source IN :sources
            """)
    Page<Recipe> findCompletedRecipesByUserIdAndSourceIn(
            @Param("userId") Long userId,
            @Param("sources") List<RecipeSourceType> sources,
            Pageable pageable);

    @Query("SELECT COUNT(r) FROM Recipe r " +
            "WHERE REPLACE(r.title, ' ', '') LIKE %:keyword% " +
            "AND r.dishType = :dishType " +
            "AND r.imageKey IS NOT NULL " +
            "AND r.isPrivate = false " +
            "AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE " +
            "AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC " +
            "AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED")
    long countCandidateRecipes(
            @Param("keyword") String keyword,
            @Param("dishType") DishType dishType);

    @Query("SELECT r FROM Recipe r " +
            "WHERE REPLACE(r.title, ' ', '') LIKE %:keyword% " +
            "AND r.dishType = :dishType " +
            "AND r.imageKey IS NOT NULL " +
            "AND r.isPrivate = false " +
            "AND r.lifecycleStatus = com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus.ACTIVE " +
            "AND r.visibility = com.jdc.recipe_service.domain.type.recipe.RecipeVisibility.PUBLIC " +
            "AND r.listingStatus = com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus.LISTED")
    Page<Recipe> findCandidateRecipesByKeywordAndDishType(
            @Param("keyword") String keyword,
            @Param("dishType") DishType dishType,
            Pageable pageable);

    @Query("SELECT r.id as id, r.updatedAt as updatedAt " +
            "FROM Recipe r " +
            "WHERE r.lifecycleStatus = 'ACTIVE' " +
            "  AND r.visibility = 'PUBLIC' " +
            "  AND r.listingStatus = 'LISTED'")
    List<RecipeSitemapProjection> findAllForSitemap();

    boolean existsByOriginRecipeIdAndUserId(Long originRecipeId, Long userId);

    // V1.x 리믹스 정책: 리믹스는 항상 PUBLIC+UNLISTED로 생성되므로 listingStatus=LISTED 조건이 있으면 모든 리믹스가 누락된다.
    // ACTIVE + visibility=PUBLIC만 적용 — PRIVATE 전환된 리믹스만 제외, link-only 리믹스는 모두 노출.
    @Query("SELECT new com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto(" +
            "r.id, r.title, r.imageKey, r.user.id, r.user.nickname, r.user.profileImage, r.createdAt, r.favoriteCount, " +
            "r.likeCount, FALSE, r.cookingTime, r.youtubeChannelName, r.youtubeChannelId, r.youtubeVideoTitle, r.youtubeThumbnailUrl, r.youtubeChannelProfileUrl, r.youtubeSubscriberCount, r.youtubeVideoViewCount, " +
            "COALESCE(ROUND(r.avgRating, 2), 0.0d), r.ratingCount, r.youtubeUrl, r.isAiGenerated) " +
            "FROM Recipe r " +
            "WHERE r.originRecipe.id = :originRecipeId " +
            "  AND r.isPrivate = false " +
            "  AND r.lifecycleStatus = 'ACTIVE' " +
            "  AND r.visibility = 'PUBLIC'")
    Page<RecipeSimpleDto> findRemixesByOriginRecipeId(@Param("originRecipeId") Long originRecipeId, Pageable pageable);

    // H6: WHERE 절은 findRemixesByOriginRecipeId와 동일해야 한다. 하나라도 어긋나면 count != list.size 불일치.
    @Query("SELECT COUNT(r) FROM Recipe r " +
            "WHERE r.originRecipe.id = :originRecipeId " +
            "  AND r.isPrivate = false " +
            "  AND r.lifecycleStatus = 'ACTIVE' " +
            "  AND r.visibility = 'PUBLIC'")
    long countRemixesByOriginRecipeId(@Param("originRecipeId") Long originRecipeId);
}