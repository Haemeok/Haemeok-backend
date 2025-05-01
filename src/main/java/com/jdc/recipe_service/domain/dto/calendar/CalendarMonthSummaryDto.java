package com.jdc.recipe_service.domain.dto.calendar;

import lombok.AllArgsConstructor;
import lombok.Getter;
import java.util.List;

@Getter @AllArgsConstructor
public class CalendarMonthSummaryDto {
    private List<CalendarDaySummaryDto> dailySummaries;
    private Long monthlyTotalSavings;
}