package com.jdc.recipe_service.domain.dto.calendar;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class CookingStreakDto {
    private final int streak;
    private final boolean cookedToday;
}