package com.jdc.recipe_service.domain.type;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;

public enum PopularityPeriod {
    WEEKLY("weekly"),
    MONTHLY("monthly");

    private final String code;

    PopularityPeriod(String code) {
        this.code = code;
    }

    public static PopularityPeriod fromCode(String code) {
        for (PopularityPeriod period : PopularityPeriod.values()) {
            if (period.code.equalsIgnoreCase(code)) {
                return period;
            }
        }
        throw new IllegalArgumentException("유효하지 않은 기간 코드입니다: " + code);
    }

    /**
     * 현재 날짜를 기준으로 해당 기간의 시작일을 계산합니다.
     */
    public LocalDate getStartDate() {
        LocalDate today = LocalDate.now();
        return switch (this) {
            case WEEKLY -> today.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
            case MONTHLY -> today.with(TemporalAdjusters.firstDayOfMonth());
        };
    }
}
