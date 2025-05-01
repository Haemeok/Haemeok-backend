package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.domain.dto.calendar.*;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.util.PricingUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.time.LocalDate;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CookingRecordService {

    private final CookingRecordRepository repo;
    private final UserRepository userRepo;
    private final RecipeRepository recipeRepo;

    /** 별점 작성 시점에 호출하여 기록 생성 */
    @Transactional
    public void createRecordFromRating(Long userId, Long recipeId, Long ratingId) {
        var user = userRepo.getReferenceById(userId);
        var recipe = recipeRepo.findById(recipeId)
                .orElseThrow(() -> new IllegalArgumentException("Invalid recipeId"));

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

        // 3) 절약액
        int savings = market - cost;

        CookingRecord rec = CookingRecord.builder()
                .user(user)
                .recipe(recipe)
                .ingredientCost(cost)
                .marketPrice(market)
                .savings(savings)
                .ratingId(ratingId)
                .build();

        repo.save(rec);
    }

    /** 별점 삭제 시점에 호출하여 기록 삭제 */
    @Transactional
    public void deleteByRatingId(Long ratingId) {
        repo.deleteByRatingId(ratingId);
    }

    /** 레시피 삭제 시점에 호출하여 연관 기록 모두 삭제 */
    @Transactional
    public void deleteByRecipeId(Long recipeId) {
        repo.deleteByRecipeId(recipeId);
    }

    /** 월별 달력 요약(일별 절약액 + 월합계) */
    @Transactional(readOnly = true)
    public CalendarMonthSummaryDto getMonthlySummary(Long userId, int year, int month) {
        List<Object[]> raw = repo.findMonthlySummaryRaw(userId, year, month);

        List<CalendarDaySummaryDto> daily = raw.stream()
                .map(arr -> {
                    java.sql.Date sqlDate = (java.sql.Date) arr[0];
                    long totalSavings    = ((Number) arr[1]).longValue();
                    return new CalendarDaySummaryDto(
                            sqlDate.toLocalDate(),
                            totalSavings
                    );
                })
                .toList();

        long monthlyTotal = daily.stream()
                .mapToLong(CalendarDaySummaryDto::getTotalSavings)
                .sum();

        return new CalendarMonthSummaryDto(daily, monthlyTotal);
    }

    /** 특정 일자의 기록 리스트 */
    @Transactional(readOnly = true)
    public List<CookingRecordDto> getDailyRecords(Long userId, LocalDate date) {
        var start = date.atStartOfDay();
        var end   = start.plusDays(1);
        return repo.findByUserIdAndCreatedAtBetweenOrderByCreatedAtDesc(userId, start, end).stream()
                .map(CookingRecordDto::from)
                .toList();
    }

    /** 개별 기록 상세 */
    @Transactional(readOnly = true)
    public CookingRecordDto getRecordDetail(Long userId, Long recordId) {
        var rec = repo.findById(recordId)
                .filter(r -> r.getUser().getId().equals(userId))
                .orElseThrow(() -> new IllegalArgumentException("Record not found"));
        return CookingRecordDto.from(rec);
    }
}