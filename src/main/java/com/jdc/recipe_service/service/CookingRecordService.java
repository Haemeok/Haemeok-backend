package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.domain.dto.calendar.*;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.PricingUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CookingRecordService {

    private final CookingRecordRepository repo;
    private final UserRepository userRepo;
    private final RecipeRepository recipeRepo;

    @Transactional
    public CookingRecord createCookingRecord(Long userId, Long recipeId) {
        var user = userRepo.getReferenceById(userId);
        var recipe = recipeRepo.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        int cost = recipe.getTotalIngredientCost() != null
                ? recipe.getTotalIngredientCost()
                : 0;

        Integer rp = recipe.getMarketPrice();
        int market;
        if (rp != null && rp > 0) {
            market = rp;
        } else if (cost > 0) {
            int percent = PricingUtil.randomizeMarginPercent(30);
            market = PricingUtil.applyMargin(cost, percent);
        } else {
            market = 0;
        }

        int savings = market - cost;

        CookingRecord rec = CookingRecord.builder()
                .user(user)
                .recipe(recipe)
                .ingredientCost(cost)
                .marketPrice(market)
                .savings(savings)
                .protein(recipe.getProtein())
                .carbohydrate(recipe.getCarbohydrate())
                .fat(recipe.getFat())
                .sugar(recipe.getSugar())
                .sodium(recipe.getSodium())
                .totalCalories(recipe.getTotalCalories())
                    .build();

        repo.save(rec);

        if (!user.isHasFirstRecord()) {
            user.markFirstRecord();
        }

        return rec;
    }

    @Transactional
    public void deleteCookingRecord(Long userId, Long recordId) {
        var rec = repo.findById(recordId)
                .orElseThrow(() -> new CustomException(ErrorCode.COOKING_RECORD_NOT_FOUND));
        if (!rec.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.USER_ACCESS_DENIED);
        }
        repo.delete(rec);
    }

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s",
                bucketName, region, key);
    }


    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    /**
     * 전체 요리 기록 피드 (날짜별 그룹, 무한스크롤).
     * 날짜 단위로 페이지네이션하여 하루치 기록이 페이지 경계에서 쪼개지지 않는다.
     * size 파라미터는 "날짜 수"를 의미한다.
     */
    @Transactional(readOnly = true)
    public CookingRecordFeedResponse getRecordFeed(Long userId, Pageable pageable) {
        // 날짜 DISTINCT 쿼리에 클라이언트 sort가 섞이면 안 되므로 unsorted로 정규화
        // 정렬은 JPQL의 ORDER BY cast(createdAt as date) DESC가 담당
        Pageable unsorted = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());

        // 1단계: 기록이 있는 날짜를 Slice로 페이지네이션
        Slice<LocalDate> dateSlice = repo.findDistinctDatesByUserId(userId, unsorted);

        if (dateSlice.isEmpty()) {
            return CookingRecordFeedResponse.builder()
                    .groups(List.of())
                    .hasNext(false)
                    .build();
        }

        // 2단계: 해당 날짜들의 레코드를 한 번에 조회 (N+1 방지)
        List<LocalDate> dates = dateSlice.getContent();
        List<CookingRecord> records = repo.findByUserIdAndDatesIn(userId, dates);

        // 날짜별 그룹핑 (LinkedHashMap + dates 순서로 최신순 유지)
        LinkedHashMap<LocalDate, List<CookingRecord>> grouped = records.stream()
                .collect(Collectors.groupingBy(
                        r -> r.getCreatedAt().atZone(KST).toLocalDate(),
                        LinkedHashMap::new,
                        Collectors.toList()
                ));

        List<CookingRecordFeedResponse.DailyGroup> groups = dates.stream()
                .filter(grouped::containsKey)
                .map(date -> CookingRecordFeedResponse.DailyGroup.builder()
                        .date(date)
                        .records(grouped.get(date).stream()
                                .map(r -> CookingRecordSummaryDto.from(
                                        r, generateImageUrl(r.getRecipe().getImageKey())))
                                .toList())
                        .build())
                .toList();

        return CookingRecordFeedResponse.builder()
                .groups(groups)
                .hasNext(dateSlice.hasNext())
                .build();
    }

    /** 월별 달력 요약(일별 절약액 + 월합계) */
    @Transactional(readOnly = true)
    public CalendarMonthSummaryDto getMonthlySummary(Long userId, int year, int month) {
        List<Object[]> raw = repo.findMonthlySummaryRaw(userId, year, month);

        List<CalendarDaySummaryDto> daily = raw.stream()
                .map(arr -> {
                    LocalDate date = ((java.sql.Date) arr[0]).toLocalDate();
                    long totalSavings    = ((Number) arr[1]).longValue();
                    LocalDateTime start = date.atStartOfDay();
                    LocalDateTime end = start.plusDays(1);
                    List<CookingRecord> records = repo
                            .findByUserIdAndCreatedAtBetweenOrderByCreatedAtDesc(userId, start, end);
                    long totalCount = records.size();

                    String imageKey = records.stream()
                            .map(r -> r.getRecipe().getImageKey())
                            .filter(key -> key != null && !key.isBlank())
                            .findFirst()
                            .orElse(null);

                    String firstImageUrl = generateImageUrl(imageKey);
                    return new CalendarDaySummaryDto(date, totalSavings, totalCount, firstImageUrl);
                })
                .toList();

        long monthlyTotal = daily.stream()
                .mapToLong(CalendarDaySummaryDto::getTotalSavings)
                .sum();

        return new CalendarMonthSummaryDto(daily, monthlyTotal);
    }

    /** 특정 일자의 생 엔티티 리스트 */
    @Transactional(readOnly = true)
    public List<com.jdc.recipe_service.domain.entity.CookingRecord> getDailyRecordEntities(
            Long userId, LocalDate date) {

        var start = date.atStartOfDay();
        var end   = start.plusDays(1);

        return repo.findByUserIdAndCreatedAtBetweenOrderByCreatedAtDesc(
                userId, start, end
        );
    }

    /** 개별 기록 상세 */
    @Transactional(readOnly = true)
    public CookingRecordDto getRecordDetail(Long userId, Long recordId) {
        var rec = repo.findById(recordId)
                .filter(r -> r.getUser().getId().equals(userId))
                .orElseThrow(() -> new CustomException(ErrorCode.COOKING_RECORD_NOT_FOUND));
        return CookingRecordDto.from(rec);
    }

    /** 불꽃(연속 요리 일수와 오늘 요리 여부) */
    @Transactional(readOnly = true)
    public CookingStreakDto getCookingStreakInfo(Long userId) {

        var rows = repo.findStreakAndTodayFlag(userId, LocalDate.now());
        if (rows.isEmpty()) {
            return new CookingStreakDto(0, false);
        }

        Object[] row = rows.get(0);   //
        int streak = ((Number) row[0]).intValue();
        boolean cookedToday = ((Number) row[1]).intValue() == 1;
        return new CookingStreakDto(streak, cookedToday);
    }
}