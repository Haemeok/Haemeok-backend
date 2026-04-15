package com.jdc.recipe_service.domain.dto.calendar;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDate;
import java.util.List;

@Getter
@Builder
@Schema(description = "전체 요리 기록 피드 응답 (날짜별 그룹, 무한스크롤)")
public class CookingRecordFeedResponse {

    @Schema(description = "날짜별 그룹 리스트 (최신순)")
    private List<DailyGroup> groups;

    @Schema(description = "다음 페이지 존재 여부")
    private boolean hasNext;

    @Getter
    @Builder
    @Schema(description = "하루치 요리 기록 그룹")
    public static class DailyGroup {

        @Schema(description = "날짜", example = "2026-04-16")
        private LocalDate date;

        @Schema(description = "해당 날짜의 요리 기록 리스트")
        private List<CookingRecordSummaryDto> records;
    }
}
