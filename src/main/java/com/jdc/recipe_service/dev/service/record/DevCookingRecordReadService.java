package com.jdc.recipe_service.dev.service.record;

import com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.domain.dto.calendar.CalendarDaySummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordFeedResponse;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CookingRecordService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Dev V3 cooking-record read dispatcher.
 *
 * <h3>🚨 운영 leak</h3>
 * 사용자 A가 PUBLIC일 때 cooked한 레시피 X가 나중에 RESTRICTED/non-ACTIVE/PENDING으로 바뀌어도, 운영 record 조회는
 * X의 title/imageUrl을 그대로 노출 → A가 더 이상 접근권 없는 X의 현재 정보가 record list로 누설됨.
 *
 * <h3>dev V3 정책: silent filter</h3>
 * batch projection({@link DevRecipeAccessProjection})으로 record 안 recipeId들을 사전에 필터:
 *  - 가시성: {@link DevRecipeAccessPolicy#isAccessibleBy} 통과 필요
 *  - 표시 가능: {@link DevRecipeAccessProjection#isImageReady}
 * 둘 다 통과한 record만 응답에 포함. 나머지는 silently 제외 (record 자체는 보존되지만 응답에서 안 보임).
 *
 * <p>Trade-off: 사용자 통계가 일부 손실될 수 있음. dev V3 다른 user-collection 경로 (favorites, saved-books, status batch)와
 * 정합 — silent filter가 일관 패턴.
 *
 * <p>detail은 single-record라 silent skip 대신 차단 → COOKING_RECORD_NOT_FOUND (record는 존재하지만 표시할 수 없음).
 */
@Service
@RequiredArgsConstructor
public class DevCookingRecordReadService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    private final CookingRecordService cookingRecordService;
    private final CookingRecordRepository cookingRecordRepository;
    private final DevRecipeAccessProjectionRepository accessProjectionRepository;

    @Transactional(readOnly = true)
    public CookingRecordFeedResponse getRecordFeed(Long userId, Pageable pageable) {
        CookingRecordFeedResponse raw = cookingRecordService.getRecordFeed(userId, pageable);
        if (raw.getGroups() == null || raw.getGroups().isEmpty()) {
            return raw;
        }
        Set<Long> displayableIds = collectDisplayableRecipeIds(extractRecipeIds(raw), userId);

        List<CookingRecordFeedResponse.DailyGroup> filteredGroups = raw.getGroups().stream()
                .map(g -> CookingRecordFeedResponse.DailyGroup.builder()
                        .date(g.getDate())
                        .records(filterRecordSummaries(g.getRecords(), displayableIds))
                        .build())
                .filter(g -> !g.getRecords().isEmpty())  // 그룹의 모든 record가 차단되면 해당 날짜 그룹도 제거
                .toList();

        return CookingRecordFeedResponse.builder()
                .groups(filteredGroups)
                .hasNext(raw.isHasNext())
                .build();
    }

    /**
     * 월간 캘린더 요약 — 운영은 각 날짜별 records의 첫 imageKey를 firstImageUrl로 노출 → RESTRICTED 레시피 이미지 누수.
     * dev V3는 displayable 기준으로 totalCount/totalSavings/firstImageUrl 모두 재계산. 모두 차단된 날짜는 응답 제외.
     *
     * <p>월 전체 records를 단일 쿼리로 로드 (운영의 N개 per-day query 대신) → batch projection 한 번 → 날짜별 그룹화 + 재집계.
     */
    @Transactional(readOnly = true)
    public CalendarMonthSummaryDto getMonthlySummary(Long userId, int year, int month,
                                                       Function<String, String> imageUrlGenerator) {
        LocalDateTime monthStart = LocalDate.of(year, month, 1).atStartOfDay();
        LocalDateTime monthEnd = LocalDate.of(year, month, 1).plusMonths(1).atStartOfDay();

        // half-open [monthStart, monthEnd) — Spring Between은 양끝 inclusive라 월 경계가 새므로 dev 전용 range method 사용.
        // @EntityGraph(recipe)로 recipe LAZY 풀어서 N+1도 동시 차단.
        List<CookingRecord> all = cookingRecordRepository
                .findByUserIdAndCreatedAtRange(userId, monthStart, monthEnd);
        if (all.isEmpty()) {
            return new CalendarMonthSummaryDto(List.of(), 0L);
        }

        List<Long> recipeIds = all.stream().map(r -> r.getRecipe().getId()).distinct().toList();
        Set<Long> displayableIds = collectDisplayableRecipeIds(recipeIds, userId);
        List<CookingRecord> displayable = all.stream()
                .filter(r -> displayableIds.contains(r.getRecipe().getId()))
                .toList();

        // 날짜별 그룹화 — KST 기준 (운영과 동일)
        Map<LocalDate, List<CookingRecord>> grouped = displayable.stream()
                .collect(Collectors.groupingBy(
                        r -> r.getCreatedAt().atZone(KST).toLocalDate(),
                        LinkedHashMap::new,
                        Collectors.toList()
                ));

        List<CalendarDaySummaryDto> daily = grouped.entrySet().stream()
                .map(e -> {
                    LocalDate date = e.getKey();
                    List<CookingRecord> dayRecords = e.getValue();
                    long totalSavings = dayRecords.stream()
                            .mapToLong(r -> r.getSavings() != null ? r.getSavings().longValue() : 0L)
                            .sum();
                    long totalCount = dayRecords.size();
                    String firstImageUrl = dayRecords.stream()
                            .map(r -> r.getRecipe().getImageKey())
                            .filter(key -> key != null && !key.isBlank())
                            .findFirst()
                            .map(imageUrlGenerator)
                            .orElse(null);
                    return new CalendarDaySummaryDto(date, totalSavings, totalCount, firstImageUrl);
                })
                .toList();

        long monthlyTotal = daily.stream().mapToLong(CalendarDaySummaryDto::getTotalSavings).sum();
        return new CalendarMonthSummaryDto(daily, monthlyTotal);
    }

    @Transactional(readOnly = true)
    public List<CookingRecordSummaryDto> getDayRecords(Long userId, LocalDate date,
                                                         java.util.function.Function<String, String> imageUrlGenerator) {
        var entities = cookingRecordService.getDailyRecordEntities(userId, date);
        if (entities.isEmpty()) {
            return List.of();
        }

        List<Long> recipeIds = entities.stream()
                .map(e -> e.getRecipe().getId())
                .toList();
        Set<Long> displayableIds = collectDisplayableRecipeIds(recipeIds, userId);

        return entities.stream()
                .filter(e -> displayableIds.contains(e.getRecipe().getId()))
                .map(e -> CookingRecordSummaryDto.from(e, imageUrlGenerator.apply(e.getRecipe().getImageKey())))
                .toList();
    }

    @Transactional(readOnly = true)
    public CookingRecordDto getRecordDetail(Long userId, Long recordId) {
        // 운영이 ownership check + 단건 조회. 가시성 검사는 운영에 없음.
        CookingRecordDto dto = cookingRecordService.getRecordDetail(userId, recordId);

        // record가 가리키는 recipe가 현재 inaccessible/PENDING이면 detail도 차단 (single-record는 silent skip 대신 명시 응답)
        Set<Long> displayableIds = collectDisplayableRecipeIds(List.of(dto.getRecipeId()), userId);
        if (!displayableIds.contains(dto.getRecipeId())) {
            // record는 존재하지만 표시 불가 — 사용자에게는 "없는 것처럼" (favorite list 정책과 동일 매핑)
            throw new CustomException(ErrorCode.COOKING_RECORD_NOT_FOUND);
        }
        return dto;
    }

    private List<Long> extractRecipeIds(CookingRecordFeedResponse raw) {
        return raw.getGroups().stream()
                .flatMap(g -> g.getRecords().stream())
                .map(CookingRecordSummaryDto::getRecipeId)
                .distinct()
                .toList();
    }

    private List<CookingRecordSummaryDto> filterRecordSummaries(
            List<CookingRecordSummaryDto> records, Set<Long> displayableIds) {
        if (records == null) return List.of();
        return records.stream()
                .filter(r -> displayableIds.contains(r.getRecipeId()))
                .toList();
    }

    /**
     * 가시성(accessibleBy) AND 표시가능(imageReady) 둘 다 통과한 ID만 반환.
     * recommendation post-filter와 동일 패턴.
     */
    private Set<Long> collectDisplayableRecipeIds(List<Long> recipeIds, Long viewerId) {
        if (recipeIds.isEmpty()) return Collections.emptySet();
        List<DevRecipeAccessProjection> projections =
                accessProjectionRepository.findAccessProjectionsByIds(recipeIds);
        return projections.stream()
                .filter(p -> DevRecipeAccessPolicy.isAccessibleBy(
                        p.lifecycleStatus(), p.visibility(), p.listingStatus(), viewerId, p.ownerId()))
                .filter(DevRecipeAccessProjection::isImageReady)
                .map(DevRecipeAccessProjection::recipeId)
                .collect(Collectors.toSet());
    }
}
