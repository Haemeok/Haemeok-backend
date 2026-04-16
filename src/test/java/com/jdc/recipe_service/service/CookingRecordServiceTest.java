package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.calendar.CookingRecordFeedResponse;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.CookingRecordRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.SliceImpl;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class CookingRecordServiceTest {

    @Mock
    private CookingRecordRepository repo;
    @Mock
    private UserRepository userRepo;
    @Mock
    private RecipeRepository recipeRepo;

    @InjectMocks
    private CookingRecordService service;

    private User user;
    private Recipe recipe;

    @BeforeEach
    void setUp() {
        user = User.builder().id(1L).build();

        recipe = Recipe.builder().id(10L).title("김치찌개").build();
        ReflectionTestUtils.setField(recipe, "user", user);
        ReflectionTestUtils.setField(recipe, "totalIngredientCost", 5000);
        ReflectionTestUtils.setField(recipe, "marketPrice", 12000);
        ReflectionTestUtils.setField(recipe, "imageKey", "recipes/10/main.webp");

        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    private CookingRecord buildRecord(Long id, int ingredientCost, int marketPrice,
                                       LocalDateTime createdAt) {
        CookingRecord record = CookingRecord.builder()
                .user(user)
                .recipe(recipe)
                .ingredientCost(ingredientCost)
                .marketPrice(marketPrice)
                .savings(marketPrice - ingredientCost)
                .protein(new BigDecimal("15.500"))
                .totalCalories(new BigDecimal("298.000"))
                .build();
        ReflectionTestUtils.setField(record, "id", id);
        ReflectionTestUtils.setField(record, "createdAt", createdAt);
        return record;
    }

    @Nested
    @DisplayName("getRecordFeed - 타임라인 피드")
    class GetRecordFeedTest {

        @Test
        @DisplayName("기록이 없으면 빈 그룹과 hasNext=false를 반환한다")
        void emptyRecords_returnsEmptyGroups() {
            // given
            Pageable pageable = PageRequest.of(0, 20);
            given(repo.findDistinctDatesByUserIdRaw(eq(1L), any(Pageable.class)))
                    .willReturn(new SliceImpl<>(List.of(), pageable, false));

            // when
            CookingRecordFeedResponse response = service.getRecordFeed(1L, pageable);

            // then
            assertThat(response.getGroups()).isEmpty();
            assertThat(response.isHasNext()).isFalse();
        }

        @Test
        @DisplayName("날짜별로 그룹핑되어 최신순으로 반환된다")
        void groupedByDateInDescOrder() {
            // given
            LocalDate today = LocalDate.of(2026, 4, 16);
            LocalDate yesterday = LocalDate.of(2026, 4, 15);
            Pageable pageable = PageRequest.of(0, 20);

            given(repo.findDistinctDatesByUserIdRaw(eq(1L), any(Pageable.class)))
                    .willReturn(new SliceImpl<>(List.of(today, yesterday), pageable, false));

            CookingRecord todayRecord1 = buildRecord(3L, 3000, 8000,
                    today.atTime(18, 0));
            CookingRecord todayRecord2 = buildRecord(2L, 4000, 9000,
                    today.atTime(12, 0));
            CookingRecord yesterdayRecord = buildRecord(1L, 5000, 11000,
                    yesterday.atTime(19, 0));

            given(repo.findByUserIdAndDatesIn(1L, List.of(today, yesterday)))
                    .willReturn(List.of(todayRecord1, todayRecord2, yesterdayRecord));

            // when
            CookingRecordFeedResponse response = service.getRecordFeed(1L, pageable);

            // then
            assertThat(response.getGroups()).hasSize(2);
            assertThat(response.getGroups().get(0).getDate()).isEqualTo(today);
            assertThat(response.getGroups().get(0).getRecords()).hasSize(2);
            assertThat(response.getGroups().get(1).getDate()).isEqualTo(yesterday);
            assertThat(response.getGroups().get(1).getRecords()).hasSize(1);
            assertThat(response.isHasNext()).isFalse();
        }

        @Test
        @DisplayName("다음 페이지가 있으면 hasNext=true를 반환한다")
        void hasNextPage_returnsTrue() {
            // given
            LocalDate today = LocalDate.of(2026, 4, 16);
            Pageable pageable = PageRequest.of(0, 1);

            given(repo.findDistinctDatesByUserIdRaw(eq(1L), any(Pageable.class)))
                    .willReturn(new SliceImpl<>(List.of(today), pageable, true));

            CookingRecord record = buildRecord(1L, 3000, 8000, today.atTime(18, 0));
            given(repo.findByUserIdAndDatesIn(1L, List.of(today)))
                    .willReturn(List.of(record));

            // when
            CookingRecordFeedResponse response = service.getRecordFeed(1L, pageable);

            // then
            assertThat(response.isHasNext()).isTrue();
        }

        @Test
        @DisplayName("Pageable의 sort를 제거하고 unsorted로 정규화한다")
        void normalizesPageableToUnsorted() {
            // given
            Pageable sortedPageable = PageRequest.of(0, 10,
                    org.springframework.data.domain.Sort.by("createdAt").descending());

            given(repo.findDistinctDatesByUserIdRaw(eq(1L), any(Pageable.class)))
                    .willReturn(new SliceImpl<>(List.of(), sortedPageable, false));

            // when
            service.getRecordFeed(1L, sortedPageable);

            // then — 실제 호출된 Pageable이 unsorted인지 확인
            var captor = org.mockito.ArgumentCaptor.forClass(Pageable.class);
            verify(repo).findDistinctDatesByUserIdRaw(eq(1L), captor.capture());
            assertThat(captor.getValue().getSort().isUnsorted()).isTrue();
            assertThat(captor.getValue().getPageNumber()).isEqualTo(0);
            assertThat(captor.getValue().getPageSize()).isEqualTo(10);
        }
    }

    @Nested
    @DisplayName("CookingRecordSummaryDto - 가격 스냅샷 검증")
    class SnapshotPriceTest {

        @Test
        @DisplayName("SummaryDto는 레시피 현재 가격이 아니라 기록 당시 스냅샷 가격을 반환한다")
        void summaryDto_usesRecordSnapshotPrice_notCurrentRecipePrice() {
            // given — 기록 당시 가격과 현재 레시피 가격이 다른 상황
            int recordCost = 3000;
            int recordMarket = 8000;
            CookingRecord record = buildRecord(1L, recordCost, recordMarket,
                    LocalDateTime.of(2026, 4, 16, 18, 0));

            // 레시피 가격이 이후 변경됨
            ReflectionTestUtils.setField(recipe, "totalIngredientCost", 9999);
            ReflectionTestUtils.setField(recipe, "marketPrice", 19999);

            // when
            CookingRecordSummaryDto dto = CookingRecordSummaryDto.from(record, "https://img.test/1.webp");

            // then — 기록 스냅샷 가격이어야 함
            assertThat(dto.getIngredientCost()).isEqualTo(recordCost);
            assertThat(dto.getMarketPrice()).isEqualTo(recordMarket);

            // 레시피 현재 가격과 달라야 함
            assertThat(dto.getIngredientCost()).isNotEqualTo(recipe.getTotalIngredientCost());
            assertThat(dto.getMarketPrice()).isNotEqualTo(recipe.getMarketPrice());
        }
    }
}
