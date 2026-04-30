package com.jdc.recipe_service.dev.service.recipebook;

import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookDetailResponse;
import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookItemResponse;
import com.jdc.recipe_service.dev.repository.recipebook.DevRecipeBookItemQueryRepository;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeBookRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeBookService 단위 검증.
 *
 *  1. listBooksDev: 운영 ensureDefaultBook 위임 (신규 사용자 lazy bootstrap 보장)
 *  2. listBooksDev: count는 dev 정책 통과 수 사용
 *  3. getBookDetailDev: ownership check (다른 사람 폴더 → ACCESS_DENIED)
 *  4. getBookDetailDev: 없는 폴더 → NOT_FOUND
 *  5. getBookDetailDev: items 변환 + 4 enum 매핑 + dev count
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeBookServiceTest {

    @Mock RecipeBookRepository bookRepo;
    @Mock DevRecipeBookItemQueryRepository devItemRepo;
    @Mock RecipeBookService recipeBookService;

    private DevRecipeBookService service;

    private static final Long USER_ID = 7L;
    private static final Long OTHER_USER_ID = 99L;
    private static final Long BOOK_ID = 100L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        service = new DevRecipeBookService(bookRepo, devItemRepo, recipeBookService);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    @Test
    @DisplayName("listBooksDev: 운영 ensureDefaultBook 위임 (신규 사용자에게 default book 1개 보장)")
    void listBooksDev_delegatesEnsureDefaultBook() {
        given(bookRepo.findByUserIdOrderByDisplayOrderAsc(USER_ID)).willReturn(List.of());
        given(devItemRepo.countAccessibleDevByUserIdGroupByBookId(USER_ID)).willReturn(Map.of());

        service.listBooksDev(USER_ID);

        verify(recipeBookService).ensureDefaultBook(USER_ID);
    }

    @Test
    @DisplayName("listBooksDev: count는 dev 정책 통과 수 (운영 group count 아닌 dev group count 사용)")
    void listBooksDev_usesDevPolicyCount() {
        RecipeBook book1 = mockBook(BOOK_ID, "book1");
        RecipeBook book2 = mockBook(BOOK_ID + 1, "book2");
        given(bookRepo.findByUserIdOrderByDisplayOrderAsc(USER_ID)).willReturn(List.of(book1, book2));
        // dev 정책 통과 카운트: book1=3, book2=0 (다른 사람 RESTRICTED만 들어 있다고 가정)
        given(devItemRepo.countAccessibleDevByUserIdGroupByBookId(USER_ID))
                .willReturn(Map.of(BOOK_ID, 3));

        List<RecipeBookResponse> result = service.listBooksDev(USER_ID);

        assertThat(result).hasSize(2);
        assertThat(result.get(0).getRecipeCount()).isEqualTo(3);
        assertThat(result.get(1).getRecipeCount()).isEqualTo(0); // group count에 없는 book은 0
    }

    @Test
    @DisplayName("getBookDetailDev: 없는 bookId → RECIPE_BOOK_NOT_FOUND")
    void getBookDetailDev_notFound_throws() {
        given(bookRepo.findById(BOOK_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> service.getBookDetailDev(USER_ID, BOOK_ID, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_BOOK_NOT_FOUND);
    }

    @Test
    @DisplayName("getBookDetailDev: 다른 사람 폴더 → RECIPE_BOOK_ACCESS_DENIED (ownership check)")
    void getBookDetailDev_nonOwner_throws() {
        RecipeBook othersBook = mockBookOwnedBy(BOOK_ID, "others-book", OTHER_USER_ID);
        given(bookRepo.findById(BOOK_ID)).willReturn(Optional.of(othersBook));

        assertThatThrownBy(() -> service.getBookDetailDev(USER_ID, BOOK_ID, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_BOOK_ACCESS_DENIED);
    }

    @Test
    @DisplayName("getBookDetailDev: items 변환 + 4 enum 매핑 + dev count 사용")
    void getBookDetailDev_mapsItemsWith4EnumAndDevCount() {
        RecipeBook book = mockBook(BOOK_ID, "my-book");
        given(bookRepo.findById(BOOK_ID)).willReturn(Optional.of(book));

        // RESTRICTED 본인 레시피 — 4 enum 모두 노출되는지 확인
        Recipe restricted = Recipe.builder()
                .user(mockUser(USER_ID, "owner"))
                .title("own-restricted")
                .dishType(DishType.FRYING)
                .lifecycleStatus(RecipeLifecycleStatus.ACTIVE)
                .visibility(RecipeVisibility.RESTRICTED)
                .listingStatus(RecipeListingStatus.UNLISTED)
                .source(RecipeSourceType.AI)
                .isAiGenerated(true)
                .build();
        ReflectionTestUtils.setField(restricted, "id", 200L);

        RecipeBookItem item = RecipeBookItem.builder().book(book).recipe(restricted).build();
        ReflectionTestUtils.setField(item, "id", 300L);

        Slice<RecipeBookItem> itemSlice = new SliceImpl<>(List.of(item), PAGE_10, false);
        given(devItemRepo.findAccessibleDevByBookIdAndUserId(eq(BOOK_ID), eq(USER_ID), eq(PAGE_10)))
                .willReturn(itemSlice);
        given(devItemRepo.countAccessibleDevByBookIdAndUserId(BOOK_ID, USER_ID)).willReturn(1);

        DevRecipeBookDetailResponse result = service.getBookDetailDev(USER_ID, BOOK_ID, PAGE_10);

        assertThat(result.getId()).isEqualTo(BOOK_ID);
        assertThat(result.getName()).isEqualTo("my-book");
        assertThat(result.getRecipeCount()).isEqualTo(1);
        assertThat(result.getRecipes()).hasSize(1);

        DevRecipeBookItemResponse dto = result.getRecipes().get(0);
        // 4 enum 매핑
        assertThat(dto.getVisibility()).isEqualTo("RESTRICTED");
        assertThat(dto.getListingStatus()).isEqualTo("UNLISTED");
        assertThat(dto.getLifecycleStatus()).isEqualTo("ACTIVE");
        assertThat(dto.getSource()).isEqualTo("AI");
        assertThat(dto.isAiGenerated()).isTrue();
    }

    // ---------- helpers ----------

    private User mockUser(Long id, String nickname) {
        User user = User.builder().nickname(nickname).provider("test").oauthId("oid-" + id).build();
        ReflectionTestUtils.setField(user, "id", id);
        return user;
    }

    private RecipeBook mockBook(Long id, String name) {
        return mockBookOwnedBy(id, name, USER_ID);
    }

    private RecipeBook mockBookOwnedBy(Long id, String name, Long ownerId) {
        RecipeBook book = RecipeBook.builder()
                .user(mockUser(ownerId, "u-" + ownerId))
                .name(name).isDefault(false).displayOrder(0)
                .build();
        ReflectionTestUtils.setField(book, "id", id);
        return book;
    }
}
