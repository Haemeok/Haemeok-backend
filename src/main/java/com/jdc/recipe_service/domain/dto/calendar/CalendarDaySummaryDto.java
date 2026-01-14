package com.jdc.recipe_service.domain.dto.calendar;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;

import java.time.LocalDate;

@Getter
public class CalendarDaySummaryDto {
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    private LocalDate date;

    private Long totalSavings;
    private Long totalCount;
    private String firstImageUrl;

    public CalendarDaySummaryDto(LocalDate date, Long totalSavings, Long totalCount, String firstImageUrl) {
        this.date = date;
        this.totalSavings = totalSavings;
        this.totalCount = totalCount;
        this.firstImageUrl = firstImageUrl;
    }
}