package com.jdc.recipe_service.domain.dto.log;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class StatsResponseDto {
    private long todayVisitors;
    private long todayClicks;
    private long totalVisitors;
    private long totalClicks;
}