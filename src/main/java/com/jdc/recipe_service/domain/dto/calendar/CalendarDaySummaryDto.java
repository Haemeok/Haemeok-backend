package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;

import java.time.LocalDate;

@Getter
public class CalendarDaySummaryDto {

    private LocalDate date;
    private Long totalSavings;

    public CalendarDaySummaryDto(LocalDate date, Long totalSavings) {
        this.date = date;
        this.totalSavings = totalSavings;
    }
}