package com.jdc.recipe_service.dev.service.record;

import com.jdc.recipe_service.dev.domain.dto.record.DevCookingRecordFeedResponse;
import com.jdc.recipe_service.dev.domain.dto.record.DevCookingRecordSummaryDto;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.domain.dto.calendar.CalendarDaySummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CalendarMonthSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordDto;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordFeedResponse;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CookingRecordService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevCookingRecordReadService 분기 매트릭스.
 *
 * 핵심 invariants:
 *  - timeline / dayRecords: 가시성 + imageReady silent filter (record 자체는 보존, 응답에서만 제외)
 *  - timeline: 그룹 모두 차단되면 그룹 자체 제거
 *  - detail: single-record라 silent skip 대신 차단 → COOKING_RECORD_NOT_FOUND
 */
@ExtendWith(MockitoExtension.class)
class DevCookingRecordReadServiceTest {

    @Mock CookingRecordService cookingRecordService;
    @Mock CookingRecordRepository cookingRecordRepository;
    @Mock DevRecipeAccessProjectionRepository accessProjectionRepository;

    @InjectMocks DevCookingRecordReadService devCookingRecordReadService;

    private static final Long USER_ID = 7L;
    private static final Long OWNER_ID = 99L;
    private static final Pageable PAGE = PageRequest.of(0, 20);
    private static final LocalDate TODAY = LocalDate.now();

    // ---------- timeline ----------

    @Test
    @DisplayName("[timeline] 운영 빈 응답 → projection 조회 없이 빈 dev DTO 반환")
    void timeline_emptyOperational_skipsProjection() {
        CookingRecordFeedResponse empty = CookingRecordFeedResponse.builder()
                .groups(List.of()).hasNext(false).build();
        given(cookingRecordService.getRecordFeed(USER_ID, PAGE)).willReturn(empty);

        DevCookingRecordFeedResponse result = devCookingRecordReadService.getRecordFeed(USER_ID, PAGE);

        // dev DTO 변환되어도 groups 비어있고 hasNext false 유지
        assertThat(result.getGroups()).isEmpty();
        assertThat(result.isHasNext()).isFalse();
        verify(accessProjectionRepository, never()).findAccessProjectionsByIds(anyCollection());
    }

    @Test
    @DisplayName("[timeline] 모든 record가 accessible+imageReady → 모두 dev DTO로 변환되어 반환")
    void timeline_allDisplayable_returnsAll() {
        CookingRecordFeedResponse raw = feedWith(group(TODAY, summary(1L, 100L), summary(2L, 200L)));
        given(cookingRecordService.getRecordFeed(USER_ID, PAGE)).willReturn(raw);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(100L, 200L)))
                .willReturn(List.of(publicReady(100L), publicReady(200L)));

        DevCookingRecordFeedResponse result = devCookingRecordReadService.getRecordFeed(USER_ID, PAGE);

        assertThat(result.getGroups()).hasSize(1);
        assertThat(result.getGroups().get(0).getRecords()).hasSize(2);
    }

    @Test
    @DisplayName("[timeline] **🚨 회귀 차단**: RESTRICTED 레시피 record → silent filter, 응답에서 제외")
    void timeline_restrictedRecipe_silentlyFiltered() {
        // record 1=PUBLIC, 2=RESTRICTED (다른 사람 추후 변경), 3=PUBLIC
        CookingRecordFeedResponse raw = feedWith(group(TODAY, summary(11L, 1L), summary(22L, 2L), summary(33L, 3L)));
        given(cookingRecordService.getRecordFeed(USER_ID, PAGE)).willReturn(raw);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(publicReady(1L), restricted(2L), publicReady(3L)));

        DevCookingRecordFeedResponse result = devCookingRecordReadService.getRecordFeed(USER_ID, PAGE);

        assertThat(result.getGroups()).hasSize(1);
        assertThat(result.getGroups().get(0).getRecords())
                .extracting(DevCookingRecordSummaryDto::getRecipeId)
                .containsExactly(1L, 3L);  // 2L 제외됨
    }

    @Test
    @DisplayName("[timeline] PENDING/FAILED imageStatus → silent filter (다른 dev 경로 imageReady 컨벤션 정합)")
    void timeline_pendingOrFailedImage_silentlyFiltered() {
        CookingRecordFeedResponse raw = feedWith(group(TODAY, summary(11L, 1L), summary(22L, 2L)));
        given(cookingRecordService.getRecordFeed(USER_ID, PAGE)).willReturn(raw);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicReady(1L), publicPending(2L)));

        DevCookingRecordFeedResponse result = devCookingRecordReadService.getRecordFeed(USER_ID, PAGE);

        assertThat(result.getGroups().get(0).getRecords())
                .extracting(DevCookingRecordSummaryDto::getRecipeId)
                .containsExactly(1L);
    }

    @Test
    @DisplayName("[timeline] 그룹의 모든 record가 차단됨 → 그룹 자체 제거")
    void timeline_allRecordsInGroupBlocked_groupRemoved() {
        CookingRecordFeedResponse raw = feedWith(
                group(TODAY, summary(11L, 1L), summary(22L, 2L)),
                group(TODAY.minusDays(1), summary(33L, 3L)));
        given(cookingRecordService.getRecordFeed(USER_ID, PAGE)).willReturn(raw);
        // 1, 2 차단 → 첫 그룹 통째로 제거. 3은 통과.
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(restricted(1L), restricted(2L), publicReady(3L)));

        DevCookingRecordFeedResponse result = devCookingRecordReadService.getRecordFeed(USER_ID, PAGE);

        assertThat(result.getGroups()).hasSize(1);
        assertThat(result.getGroups().get(0).getDate()).isEqualTo(TODAY.minusDays(1));
        assertThat(result.getGroups().get(0).getRecords())
                .extracting(DevCookingRecordSummaryDto::getRecipeId)
                .containsExactly(3L);
    }

    // ---------- dayRecords ----------

    @Test
    @DisplayName("[dayRecords] entities 빈 list → 빈 응답, projection 조회 없음")
    void dayRecords_emptyEntities_returnsEmpty() {
        given(cookingRecordService.getDailyRecordEntities(USER_ID, TODAY)).willReturn(List.of());

        List<DevCookingRecordSummaryDto> result =
                devCookingRecordReadService.getDayRecords(USER_ID, TODAY, key -> "url:" + key);

        assertThat(result).isEmpty();
        verify(accessProjectionRepository, never()).findAccessProjectionsByIds(anyCollection());
    }

    @Test
    @DisplayName("[dayRecords] 일부 RESTRICTED → silent filter")
    void dayRecords_partialRestricted_filters() {
        CookingRecord r1 = mockRecord(11L, 1L);
        CookingRecord r2 = mockRecord(22L, 2L);
        given(cookingRecordService.getDailyRecordEntities(USER_ID, TODAY)).willReturn(List.of(r1, r2));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicReady(1L), restricted(2L)));

        List<DevCookingRecordSummaryDto> result =
                devCookingRecordReadService.getDayRecords(USER_ID, TODAY, key -> "url:" + key);

        assertThat(result).extracting(DevCookingRecordSummaryDto::getRecipeId).containsExactly(1L);
    }

    @Test
    @DisplayName("[timeline + dayRecords] dev DTO에 visibility/listingStatus/isRemix 매핑 — projection meta로 채워짐")
    void timelineAndDayRecords_mapVisibilityListingIsRemixFromProjection() {
        // dayRecords 한 record (recipe 1L) — link-only PUBLIC+UNLISTED + remix 시나리오
        CookingRecord rec = mockRecord(11L, 1L);
        given(cookingRecordService.getDailyRecordEntities(USER_ID, TODAY)).willReturn(List.of(rec));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(linkOnlyRemix(1L)));

        List<DevCookingRecordSummaryDto> result =
                devCookingRecordReadService.getDayRecords(USER_ID, TODAY, key -> "url:" + key);

        assertThat(result).hasSize(1);
        DevCookingRecordSummaryDto dto = result.get(0);
        assertThat(dto.getRecipeId()).isEqualTo(1L);
        assertThat(dto.getVisibility()).isEqualTo("PUBLIC");
        assertThat(dto.isRemix()).isTrue();
    }

    // ---------- monthSummary ----------

    @Test
    @DisplayName("[monthSummary] 빈 month → 빈 dailySummaries + 0 monthlyTotal, projection 미호출")
    void monthSummary_emptyMonth_returnsEmpty() {
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of());

        CalendarMonthSummaryDto result = devCookingRecordReadService.getMonthlySummary(
                USER_ID, 2026, 4, key -> "url:" + key);

        assertThat(result.getDailySummaries()).isEmpty();
        assertThat(result.getMonthlyTotalSavings()).isZero();
        verify(accessProjectionRepository, never()).findAccessProjectionsByIds(anyCollection());
    }

    @Test
    @DisplayName("[monthSummary] 모두 displayable → 날짜별 totalCount/totalSavings/firstImageUrl 정상 집계")
    void monthSummary_allDisplayable_aggregates() {
        // 2026-04-15에 record 2개 (recipe 1L savings 1000, recipe 2L savings 2000)
        // 2026-04-20에 record 1개 (recipe 3L savings 500)
        CookingRecord r1 = mockRecordWithSavings(11L, 1L, "img-1", LocalDate.of(2026, 4, 15), 1000);
        CookingRecord r2 = mockRecordWithSavings(22L, 2L, "img-2", LocalDate.of(2026, 4, 15), 2000);
        CookingRecord r3 = mockRecordWithSavings(33L, 3L, "img-3", LocalDate.of(2026, 4, 20), 500);
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of(r1, r2, r3));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(publicReady(1L), publicReady(2L), publicReady(3L)));

        CalendarMonthSummaryDto result = devCookingRecordReadService.getMonthlySummary(
                USER_ID, 2026, 4, key -> "url:" + key);

        assertThat(result.getDailySummaries()).hasSize(2);
        assertThat(result.getMonthlyTotalSavings()).isEqualTo(3500L);
        // 4-15: 2개, savings=3000
        CalendarDaySummaryDto day15 = result.getDailySummaries().stream()
                .filter(d -> d.getDate().equals(LocalDate.of(2026, 4, 15))).findFirst().orElseThrow();
        assertThat(day15.getTotalCount()).isEqualTo(2L);
        assertThat(day15.getTotalSavings()).isEqualTo(3000L);
        assertThat(day15.getFirstImageUrl()).startsWith("url:img-");
    }

    @Test
    @DisplayName("[monthSummary] **🚨 회귀 차단**: 일부 RESTRICTED → totalCount/totalSavings/firstImageUrl 모두 displayable 기준 재계산")
    void monthSummary_partialRestricted_recomputesAggregates() {
        // 2026-04-15: r1=PUBLIC savings=1000, r2=RESTRICTED savings=99999 → r1만 집계
        CookingRecord r1 = mockRecordWithSavings(11L, 1L, "img-1", LocalDate.of(2026, 4, 15), 1000);
        CookingRecord r2 = mockRecordWithSavings(22L, 2L, "img-2", LocalDate.of(2026, 4, 15), 99999);
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of(r1, r2));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicReady(1L), restricted(2L)));

        CalendarMonthSummaryDto result = devCookingRecordReadService.getMonthlySummary(
                USER_ID, 2026, 4, key -> "url:" + key);

        assertThat(result.getDailySummaries()).hasSize(1);
        CalendarDaySummaryDto day15 = result.getDailySummaries().get(0);
        assertThat(day15.getTotalCount()).isEqualTo(1L);  // r2 제외
        assertThat(day15.getTotalSavings()).isEqualTo(1000L);  // r2 99999 제외 (운영이라면 100999)
        assertThat(day15.getFirstImageUrl()).isEqualTo("url:img-1");  // r2 image-2 노출 안 됨
        assertThat(result.getMonthlyTotalSavings()).isEqualTo(1000L);
    }

    @Test
    @DisplayName("[monthSummary] 날짜의 모든 record가 차단됨 → 그 날짜 dailySummaries에서 제외")
    void monthSummary_allBlockedDay_excludedFromDailySummaries() {
        // 4-15: 모두 RESTRICTED (제외돼야 함)
        // 4-20: PUBLIC (살아남아야 함)
        CookingRecord r1 = mockRecordWithSavings(11L, 1L, "img-1", LocalDate.of(2026, 4, 15), 1000);
        CookingRecord r2 = mockRecordWithSavings(22L, 2L, "img-2", LocalDate.of(2026, 4, 20), 2000);
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of(r1, r2));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(restricted(1L), publicReady(2L)));

        CalendarMonthSummaryDto result = devCookingRecordReadService.getMonthlySummary(
                USER_ID, 2026, 4, key -> "url:" + key);

        assertThat(result.getDailySummaries()).hasSize(1);
        assertThat(result.getDailySummaries().get(0).getDate()).isEqualTo(LocalDate.of(2026, 4, 20));
        assertThat(result.getMonthlyTotalSavings()).isEqualTo(2000L);
    }

    @Test
    @DisplayName("[monthSummary] PENDING image → 차단 (firstImageUrl 누수 회피)")
    void monthSummary_pendingImage_excluded() {
        CookingRecord r1 = mockRecordWithSavings(11L, 1L, "img-1", LocalDate.of(2026, 4, 15), 1000);
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of(r1));
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(publicPending(1L)));

        CalendarMonthSummaryDto result = devCookingRecordReadService.getMonthlySummary(
                USER_ID, 2026, 4, key -> "url:" + key);

        assertThat(result.getDailySummaries()).isEmpty();
        assertThat(result.getMonthlyTotalSavings()).isZero();
    }

    @Test
    @DisplayName("[monthSummary] **MUST 회귀 차단**: 4월 조회 시 [2026-04-01, 2026-05-01) half-open range로 repo 호출 (5월 1일 0시 정각 record 누락)")
    void monthSummary_callsRepoWithHalfOpenRange() {
        given(cookingRecordRepository.findByUserIdAndCreatedAtRange(eq(USER_ID), any(), any()))
                .willReturn(List.of());

        devCookingRecordReadService.getMonthlySummary(USER_ID, 2026, 4, key -> "url:" + key);

        org.mockito.ArgumentCaptor<LocalDateTime> startCap = org.mockito.ArgumentCaptor.forClass(LocalDateTime.class);
        org.mockito.ArgumentCaptor<LocalDateTime> endCap = org.mockito.ArgumentCaptor.forClass(LocalDateTime.class);
        verify(cookingRecordRepository).findByUserIdAndCreatedAtRange(eq(USER_ID), startCap.capture(), endCap.capture());

        // start: 2026-04-01 00:00, end: 2026-05-01 00:00 (exclusive — 5월 1일 0시 record는 제외)
        assertThat(startCap.getValue()).isEqualTo(LocalDate.of(2026, 4, 1).atStartOfDay());
        assertThat(endCap.getValue()).isEqualTo(LocalDate.of(2026, 5, 1).atStartOfDay());
        // repo method 시그니처가 half-open이므로 endCap == 5월 1일 0시면 5월 1일 record는 제외됨 (Spring Data Between 양끝 inclusive 회귀 방어)
    }

    // ---------- detail ----------

    @Test
    @DisplayName("[detail] recipe accessible → dto 그대로 반환")
    void detail_accessibleRecipe_returnsDto() {
        CookingRecordDto dto = mockDto(500L, 1L);
        given(cookingRecordService.getRecordDetail(USER_ID, 500L)).willReturn(dto);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(publicReady(1L)));

        CookingRecordDto result = devCookingRecordReadService.getRecordDetail(USER_ID, 500L);

        assertThat(result).isSameAs(dto);
    }

    @Test
    @DisplayName("[detail] recipe RESTRICTED → COOKING_RECORD_NOT_FOUND (single-record는 silent skip 대신 명시 차단)")
    void detail_restrictedRecipe_throwsNotFound() {
        CookingRecordDto dto = mockDto(500L, 1L);
        given(cookingRecordService.getRecordDetail(USER_ID, 500L)).willReturn(dto);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(restricted(1L)));

        assertThatThrownBy(() -> devCookingRecordReadService.getRecordDetail(USER_ID, 500L))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.COOKING_RECORD_NOT_FOUND);
    }

    @Test
    @DisplayName("[detail] recipe PENDING → COOKING_RECORD_NOT_FOUND")
    void detail_pendingImage_throwsNotFound() {
        CookingRecordDto dto = mockDto(500L, 1L);
        given(cookingRecordService.getRecordDetail(USER_ID, 500L)).willReturn(dto);
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(publicPending(1L)));

        assertThatThrownBy(() -> devCookingRecordReadService.getRecordDetail(USER_ID, 500L))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.COOKING_RECORD_NOT_FOUND);
    }

    // ---------- helpers ----------

    private static java.util.Collection<Long> anyCollection() {
        return org.mockito.ArgumentMatchers.anyCollection();
    }

    private static CookingRecordSummaryDto summary(Long recordId, Long recipeId) {
        // CookingRecordSummaryDto는 setter 없음 → reflection 또는 from() 사용 필요. 여기선 entity mock + from() 우회.
        CookingRecord rec = mockRecord(recordId, recipeId);
        return CookingRecordSummaryDto.from(rec, "url:" + recordId);
    }

    private static CookingRecord mockRecord(Long recordId, Long recipeId) {
        CookingRecord rec = org.mockito.Mockito.mock(CookingRecord.class);
        Recipe recipe = org.mockito.Mockito.mock(Recipe.class);
        org.mockito.Mockito.lenient().when(rec.getId()).thenReturn(recordId);
        org.mockito.Mockito.lenient().when(rec.getRecipe()).thenReturn(recipe);
        org.mockito.Mockito.lenient().when(recipe.getId()).thenReturn(recipeId);
        org.mockito.Mockito.lenient().when(recipe.getImageKey()).thenReturn("img-" + recipeId);
        org.mockito.Mockito.lenient().when(recipe.getTitle()).thenReturn("title-" + recipeId);
        return rec;
    }

    /** monthSummary 테스트용 — savings + createdAt(KST 기준 date) 추가 */
    private static CookingRecord mockRecordWithSavings(Long recordId, Long recipeId, String imageKey,
                                                        LocalDate date, int savings) {
        CookingRecord rec = org.mockito.Mockito.mock(CookingRecord.class);
        Recipe recipe = org.mockito.Mockito.mock(Recipe.class);
        org.mockito.Mockito.lenient().when(rec.getId()).thenReturn(recordId);
        org.mockito.Mockito.lenient().when(rec.getRecipe()).thenReturn(recipe);
        org.mockito.Mockito.lenient().when(recipe.getId()).thenReturn(recipeId);
        org.mockito.Mockito.lenient().when(recipe.getImageKey()).thenReturn(imageKey);
        org.mockito.Mockito.lenient().when(rec.getSavings()).thenReturn(savings);
        // createdAt: KST 기준 12:00로 해석되는 시점 — service의 atZone(KST).toLocalDate()가 date를 반환하도록
        org.mockito.Mockito.lenient().when(rec.getCreatedAt()).thenReturn(date.atTime(12, 0));
        return rec;
    }

    private static CookingRecordDto mockDto(Long id, Long recipeId) {
        CookingRecordDto dto = org.mockito.Mockito.mock(CookingRecordDto.class);
        org.mockito.Mockito.lenient().when(dto.getId()).thenReturn(id);
        org.mockito.Mockito.when(dto.getRecipeId()).thenReturn(recipeId);
        return dto;
    }

    private static CookingRecordFeedResponse.DailyGroup group(LocalDate date, CookingRecordSummaryDto... records) {
        return CookingRecordFeedResponse.DailyGroup.builder().date(date).records(List.of(records)).build();
    }

    private static CookingRecordFeedResponse feedWith(CookingRecordFeedResponse.DailyGroup... groups) {
        return CookingRecordFeedResponse.builder().groups(List.of(groups)).hasNext(false).build();
    }

    private static DevRecipeAccessProjection publicReady(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, null);
    }

    private static DevRecipeAccessProjection publicPending(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.PENDING, null);
    }

    private static DevRecipeAccessProjection restricted(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, null);
    }

    /** PUBLIC + UNLISTED (link-only) + originRecipeId not-null → dev DTO에 isRemix=true 검증용. */
    private static DevRecipeAccessProjection linkOnlyRemix(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, /* originRecipeId */ 999L);
    }
}
