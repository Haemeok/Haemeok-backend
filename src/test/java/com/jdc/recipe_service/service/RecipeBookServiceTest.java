package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipebook.*;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeBookServiceTest {

    @Mock private RecipeBookRepository bookRepo;
    @Mock private RecipeBookItemRepository itemRepo;
    @Mock private RecipeFavoriteRepository favoriteRepo;
    @Mock private UserRepository userRepo;
    @Mock private RecipeRepository recipeRepo;

    @InjectMocks private RecipeBookService bookService;

    private User user;
    private RecipeBook defaultBook;

    @BeforeEach
    void setUp() {
        user = User.builder().id(1L).build();
        defaultBook = RecipeBook.builder()
                .id(10L)
                .user(user)
                .name("저장한 레시피")
                .isDefault(true)
                .displayOrder(0)
                .build();
    }

    // ── ensureDefaultBook ──

    @Nested
    @DisplayName("ensureDefaultBook")
    class EnsureDefaultBook {

        @Test
        @DisplayName("기존 기본 레시피북이 있으면 그대로 반환")
        void returnsExistingDefaultBook() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));

            RecipeBook result = bookService.ensureDefaultBook(user.getId());

            assertThat(result.getId()).isEqualTo(defaultBook.getId());
            verify(bookRepo, never()).saveAndFlush(any());
        }

        @Test
        @DisplayName("기본 레시피북이 없으면 새로 생성")
        void createsNewDefaultBook() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.empty());
            given(userRepo.getReferenceById(user.getId())).willReturn(user);
            given(bookRepo.saveAndFlush(any(RecipeBook.class))).willReturn(defaultBook);

            RecipeBook result = bookService.ensureDefaultBook(user.getId());

            assertThat(result.getId()).isEqualTo(defaultBook.getId());
            assertThat(result.isDefault()).isTrue();
        }
    }

    // ── createBook ──

    @Nested
    @DisplayName("createBook")
    class CreateBook {

        @Test
        @DisplayName("정상 생성 시 RecipeBookResponse 반환")
        void createsBookSuccessfully() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(bookRepo.countByUserId(user.getId())).willReturn(1L);
            given(bookRepo.existsByUserIdAndName(user.getId(), "한식 모음")).willReturn(false);
            given(bookRepo.findMaxDisplayOrderByUserId(user.getId())).willReturn(0);
            given(userRepo.getReferenceById(user.getId())).willReturn(user);
            given(bookRepo.save(any(RecipeBook.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));

            CreateRecipeBookRequest request = CreateRecipeBookRequest.builder()
                    .name("한식 모음")
                    .build();

            RecipeBookResponse response = bookService.createBook(user.getId(), request);

            assertThat(response.getName()).isEqualTo("한식 모음");
            verify(bookRepo).save(any(RecipeBook.class));
        }

        @Test
        @DisplayName("같은 이름의 레시피북이 이미 있으면 RECIPE_BOOK_DUPLICATE_NAME 예외")
        void throwsWhenDuplicateName() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(bookRepo.countByUserId(user.getId())).willReturn(1L);
            given(bookRepo.existsByUserIdAndName(user.getId(), "한식 모음")).willReturn(true);

            CreateRecipeBookRequest request = CreateRecipeBookRequest.builder()
                    .name("한식 모음")
                    .build();

            assertThatThrownBy(() -> bookService.createBook(user.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_DUPLICATE_NAME));
        }

        @Test
        @DisplayName("레시피북 20개 초과 시 RECIPE_BOOK_LIMIT_EXCEEDED 예외")
        void throwsWhenLimitExceeded() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(bookRepo.countByUserId(user.getId())).willReturn(20L);

            CreateRecipeBookRequest request = CreateRecipeBookRequest.builder()
                    .name("초과 폴더")
                    .build();

            assertThatThrownBy(() -> bookService.createBook(user.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_LIMIT_EXCEEDED));
        }
    }

    // ── renameBook ──

    @Nested
    @DisplayName("renameBook")
    class RenameBook {

        @Test
        @DisplayName("기본 레시피북은 이름 변경 불가")
        void cannotRenameDefaultBook() {
            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));

            RenameRecipeBookRequest request = RenameRecipeBookRequest.builder()
                    .name("새 이름")
                    .build();

            assertThatThrownBy(() -> bookService.renameBook(user.getId(), defaultBook.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_RENAME));
        }

        @Test
        @DisplayName("다른 유저의 레시피북 접근 시 ACCESS_DENIED 예외")
        void throwsWhenNotOwner() {
            User otherUser = User.builder().id(999L).build();
            RecipeBook otherBook = RecipeBook.builder()
                    .id(20L).user(otherUser).name("남의 폴더").build();
            given(bookRepo.findById(otherBook.getId())).willReturn(Optional.of(otherBook));

            RenameRecipeBookRequest request = RenameRecipeBookRequest.builder()
                    .name("탈취 시도")
                    .build();

            assertThatThrownBy(() -> bookService.renameBook(user.getId(), otherBook.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_ACCESS_DENIED));
        }

        @Test
        @DisplayName("같은 이름으로 저장하면 중복 체크 없이 그대로 통과한다")
        void sameNameIsNoOp() {
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("한식 모음").build();
            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));

            RenameRecipeBookRequest request = RenameRecipeBookRequest.builder()
                    .name("한식 모음")
                    .build();

            RecipeBookResponse result = bookService.renameBook(user.getId(), customBook.getId(), request);

            assertThat(result.getName()).isEqualTo("한식 모음");
            then(bookRepo).should(never()).existsByUserIdAndName(any(), any());
        }

        @Test
        @DisplayName("다른 이름으로 변경 시 중복이면 RECIPE_BOOK_DUPLICATE_NAME 예외")
        void throwsWhenRenameToExistingName() {
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("기존 이름").build();
            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(bookRepo.existsByUserIdAndName(user.getId(), "한식 모음")).willReturn(true);

            RenameRecipeBookRequest request = RenameRecipeBookRequest.builder()
                    .name("한식 모음")
                    .build();

            assertThatThrownBy(() -> bookService.renameBook(user.getId(), customBook.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_DUPLICATE_NAME));
        }
    }

    // ── deleteBook ──

    @Nested
    @DisplayName("deleteBook")
    class DeleteBook {

        @Test
        @DisplayName("기본 레시피북은 삭제 불가")
        void cannotDeleteDefaultBook() {
            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));

            assertThatThrownBy(() -> bookService.deleteBook(user.getId(), defaultBook.getId()))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_DELETE));
        }

        @Test
        @DisplayName("커스텀 폴더 삭제 시 마지막 폴더면 legacy favorite도 삭제된다")
        void deletesLegacyFavoriteWhenLastFolder() {
            // given
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("매운 요리").isDefault(false).build();
            Recipe recipe = Recipe.builder().id(50L).user(user).favoriteCount(1L).build();
            RecipeBookItem item = RecipeBookItem.builder()
                    .id(1L).book(customBook).recipe(recipe).build();
            RecipeFavorite favorite = RecipeFavorite.builder()
                    .id(1L).user(user).recipe(recipe).build();

            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(itemRepo.findByBookId(customBook.getId())).willReturn(List.of(item));
            // 다른 폴더에 없음
            given(itemRepo.countByUserIdAndRecipeIdExcludingBook(user.getId(), recipe.getId(), customBook.getId()))
                    .willReturn(0L);
            given(favoriteRepo.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                    .willReturn(Optional.of(favorite));

            // when
            bookService.deleteBook(user.getId(), customBook.getId());

            // then
            then(favoriteRepo).should().delete(favorite);
            assertThat(recipe.getFavoriteCount()).isZero();
            then(bookRepo).should().delete(customBook);
        }

        @Test
        @DisplayName("커스텀 폴더 삭제 시 다른 폴더에도 있으면 favorite은 유지된다")
        void keepsFavoriteWhenOtherFolderExists() {
            // given
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("매운 요리").isDefault(false).build();
            Recipe recipe = Recipe.builder().id(50L).user(user).favoriteCount(1L).build();
            RecipeBookItem item = RecipeBookItem.builder()
                    .id(1L).book(customBook).recipe(recipe).build();

            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(itemRepo.findByBookId(customBook.getId())).willReturn(List.of(item));
            // 다른 폴더에도 있음
            given(itemRepo.countByUserIdAndRecipeIdExcludingBook(user.getId(), recipe.getId(), customBook.getId()))
                    .willReturn(1L);

            // when
            bookService.deleteBook(user.getId(), customBook.getId());

            // then
            then(favoriteRepo).should(never()).findByUserIdAndRecipeId(any(), any());
            assertThat(recipe.getFavoriteCount()).isEqualTo(1);
            then(bookRepo).should().delete(customBook);
        }
    }

    // ── addRecipesToBook ──

    @Nested
    @DisplayName("addRecipesToBook")
    class AddRecipesToBook {

        @Test
        @DisplayName("첫 폴더에 레시피 추가 시 legacy favorite도 생성")
        void addToDefaultBookSuccessfully() {
            Recipe recipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();

            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));
            given(recipeRepo.findById(recipe.getId())).willReturn(Optional.of(recipe));
            given(itemRepo.existsByBookIdAndRecipeId(defaultBook.getId(), recipe.getId()))
                    .willReturn(false);
            given(itemRepo.save(any(RecipeBookItem.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));
            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), recipe.getId()))
                    .willReturn(false);
            given(favoriteRepo.existsByUserIdAndRecipeId(user.getId(), recipe.getId()))
                    .willReturn(false);
            given(userRepo.getReferenceById(user.getId())).willReturn(user);
            given(favoriteRepo.save(any())).willAnswer(invocation -> invocation.getArgument(0));

            AddRecipesToBookRequest request = AddRecipesToBookRequest.builder()
                    .recipeIds(List.of(recipe.getId()))
                    .build();

            AddRecipesToBookResponse result =
                    bookService.addRecipesToBook(user.getId(), defaultBook.getId(), request);

            assertThat(result.getAddedCount()).isEqualTo(1);
            assertThat(result.getSkippedCount()).isZero();
            verify(itemRepo).save(any(RecipeBookItem.class));
            verify(favoriteRepo).save(any());
        }

        @Test
        @DisplayName("비공개 타인 레시피는 skip 처리")
        void skipsPrivateRecipeNotOwned() {
            User otherUser = User.builder().id(999L).build();
            Recipe privateRecipe = Recipe.builder().id(50L)
                    .user(otherUser).isPrivate(true).build();

            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("커스텀").build();

            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(itemRepo.existsByBookIdAndRecipeId(customBook.getId(), privateRecipe.getId()))
                    .willReturn(false);
            given(recipeRepo.findById(privateRecipe.getId())).willReturn(Optional.of(privateRecipe));

            AddRecipesToBookRequest request = AddRecipesToBookRequest.builder()
                    .recipeIds(List.of(privateRecipe.getId()))
                    .build();

            AddRecipesToBookResponse result =
                    bookService.addRecipesToBook(user.getId(), customBook.getId(), request);

            assertThat(result.getAddedCount()).isZero();
            assertThat(result.getSkippedCount()).isEqualTo(1);
            verify(itemRepo, never()).save(any());
        }

        @Test
        @DisplayName("이미 추가된 레시피는 skip 처리")
        void skipsWhenDuplicate() {
            Recipe recipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();

            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("커스텀").build();

            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(itemRepo.existsByBookIdAndRecipeId(customBook.getId(), recipe.getId()))
                    .willReturn(true);

            AddRecipesToBookRequest request = AddRecipesToBookRequest.builder()
                    .recipeIds(List.of(recipe.getId()))
                    .build();

            AddRecipesToBookResponse result =
                    bookService.addRecipesToBook(user.getId(), customBook.getId(), request);

            assertThat(result.getAddedCount()).isZero();
            assertThat(result.getSkippedCount()).isEqualTo(1);
            verify(itemRepo, never()).save(any());
        }

        @Test
        @DisplayName("여러 레시피 중 일부만 추가되고 나머지는 skip")
        void partialAddAndSkip() {
            Recipe publicRecipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();
            Recipe alreadyAdded = Recipe.builder().id(51L)
                    .user(user).isPrivate(false).build();
            Long nonExistentId = 999L;

            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("커스텀").build();

            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));

            // 50: 새로 추가 가능
            given(itemRepo.existsByBookIdAndRecipeId(customBook.getId(), 50L)).willReturn(false);
            given(recipeRepo.findById(50L)).willReturn(Optional.of(publicRecipe));
            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), 50L)).willReturn(true);
            given(itemRepo.save(any(RecipeBookItem.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));

            // 51: 이미 존재
            given(itemRepo.existsByBookIdAndRecipeId(customBook.getId(), 51L)).willReturn(true);

            // 999: 존재하지 않는 레시피
            given(itemRepo.existsByBookIdAndRecipeId(customBook.getId(), 999L)).willReturn(false);
            given(recipeRepo.findById(999L)).willReturn(Optional.empty());

            AddRecipesToBookRequest request = AddRecipesToBookRequest.builder()
                    .recipeIds(List.of(50L, 51L, 999L))
                    .build();

            AddRecipesToBookResponse result =
                    bookService.addRecipesToBook(user.getId(), customBook.getId(), request);

            assertThat(result.getAddedCount()).isEqualTo(1);
            assertThat(result.getSkippedCount()).isEqualTo(2);
        }

        @Test
        @DisplayName("중복 ID는 자동 제거되어 1번만 처리")
        void deduplicatesIds() {
            Recipe recipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();

            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));
            given(itemRepo.existsByBookIdAndRecipeId(defaultBook.getId(), 50L)).willReturn(false);
            given(recipeRepo.findById(50L)).willReturn(Optional.of(recipe));
            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), 50L)).willReturn(true);
            given(itemRepo.save(any(RecipeBookItem.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));

            AddRecipesToBookRequest request = AddRecipesToBookRequest.builder()
                    .recipeIds(List.of(50L, 50L, 50L))
                    .build();

            AddRecipesToBookResponse result =
                    bookService.addRecipesToBook(user.getId(), defaultBook.getId(), request);

            assertThat(result.getAddedCount()).isEqualTo(1);
            assertThat(result.getSkippedCount()).isZero();
        }
    }

    // ── removeRecipesFromBook ──

    @Nested
    @DisplayName("removeRecipesFromBook")
    class RemoveRecipesFromBook {

        @Test
        @DisplayName("마지막 폴더에서 삭제 시 legacy favorite도 제거")
        void removeFromLastBookDeletesFavorite() {
            Long recipeId = 50L;
            Recipe recipe = Recipe.builder().id(recipeId).user(user).isPrivate(false).build();

            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));
            given(itemRepo.deleteByBookIdAndRecipeIdIn(defaultBook.getId(), List.of(recipeId)))
                    .willReturn(1);
            // 마지막 폴더 → legacy sync
            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), recipeId))
                    .willReturn(false);
            RecipeFavorite fav = RecipeFavorite.builder()
                    .id(300L).user(user).recipe(recipe).build();
            given(favoriteRepo.findByUserIdAndRecipeId(user.getId(), recipeId))
                    .willReturn(Optional.of(fav));

            RemoveRecipesFromBookRequest request = RemoveRecipesFromBookRequest.builder()
                    .recipeIds(List.of(recipeId))
                    .build();

            bookService.removeRecipesFromBook(user.getId(), defaultBook.getId(), request);

            verify(itemRepo).deleteByBookIdAndRecipeIdIn(defaultBook.getId(), List.of(recipeId));
            verify(favoriteRepo).delete(fav);
        }
    }

    // ── reorderBooks ──

    @Nested
    @DisplayName("reorderBooks")
    class ReorderBooks {

        @Test
        @DisplayName("ID 목록이 불완전하면 INVALID_INPUT_VALUE 예외")
        void throwsWhenIdListIncomplete() {
            RecipeBook book2 = RecipeBook.builder()
                    .id(20L).user(user).name("폴더2").displayOrder(1).build();

            given(bookRepo.findByUserIdOrderByDisplayOrderAsc(user.getId()))
                    .willReturn(List.of(defaultBook, book2));

            // defaultBook만 보내고 book2 누락
            ReorderRecipeBooksRequest request = ReorderRecipeBooksRequest.builder()
                    .bookIds(List.of(defaultBook.getId()))
                    .build();

            assertThatThrownBy(() -> bookService.reorderBooks(user.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.INVALID_INPUT_VALUE));
        }

        @Test
        @DisplayName("중복 ID가 있으면 INVALID_INPUT_VALUE 예외")
        void throwsWhenDuplicateIds() {
            given(bookRepo.findByUserIdOrderByDisplayOrderAsc(user.getId()))
                    .willReturn(List.of(defaultBook));

            ReorderRecipeBooksRequest request = ReorderRecipeBooksRequest.builder()
                    .bookIds(List.of(defaultBook.getId(), defaultBook.getId()))
                    .build();

            assertThatThrownBy(() -> bookService.reorderBooks(user.getId(), request))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.INVALID_INPUT_VALUE));
        }
    }

    // ── adjustCountsBeforeRecipeDeletion ──

    @Nested
    @DisplayName("adjustCountsBeforeRecipeDeletion")
    class AdjustCounts {

        @Test
        @DisplayName("레시피 삭제 시 영향받는 모든 폴더의 recipeCount를 감소")
        void decrementsAffectedBookCounts() {
            Long recipeId = 50L;
            RecipeBook book2 = RecipeBook.builder()
                    .id(20L).user(user).name("폴더2").recipeCount(5).build();

            // recipeId가 defaultBook에 1건, book2에 1건 포함
            given(itemRepo.countByRecipeIdGroupByBookId(recipeId))
                    .willReturn(List.of(
                            new Object[]{defaultBook.getId(), 1L},
                            new Object[]{book2.getId(), 1L}
                    ));
            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));
            given(bookRepo.findById(book2.getId())).willReturn(Optional.of(book2));

            bookService.adjustCountsBeforeRecipeDeletion(recipeId);

            assertThat(book2.getRecipeCount()).isEqualTo(4);
        }
    }

    // ── getSaveStatus ──

    @Nested
    @DisplayName("getSaveStatus")
    class GetSaveStatus {

        @Test
        @DisplayName("폴더 2개에 저장된 레시피는 saved=true, savedBookCount=2")
        void returnsSavedStatusWithMultipleBooks() {
            Long recipeId = 50L;
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("한식 모음").build();

            given(itemRepo.findBooksByRecipeIdAndUserId(recipeId, user.getId()))
                    .willReturn(List.of(defaultBook, customBook));

            RecipeSaveStatusResponse response = bookService.getSaveStatus(user.getId(), recipeId);

            assertThat(response.isSaved()).isTrue();
            assertThat(response.getSavedBookCount()).isEqualTo(2);
            assertThat(response.getBooks()).hasSize(2);
            assertThat(response.getBooks().get(0).getName()).isEqualTo("저장한 레시피");
            assertThat(response.getBooks().get(1).getName()).isEqualTo("한식 모음");
        }

        @Test
        @DisplayName("어떤 폴더에도 없는 레시피는 saved=false")
        void returnsNotSavedWhenNoBooks() {
            Long recipeId = 99L;
            given(itemRepo.findBooksByRecipeIdAndUserId(recipeId, user.getId()))
                    .willReturn(List.of());

            RecipeSaveStatusResponse response = bookService.getSaveStatus(user.getId(), recipeId);

            assertThat(response.isSaved()).isFalse();
            assertThat(response.getSavedBookCount()).isEqualTo(0);
            assertThat(response.getBooks()).isEmpty();
        }
    }

    // ── toggleSave ──

    @Nested
    @DisplayName("toggleSave")
    class ToggleSave {

        @Test
        @DisplayName("저장되지 않은 레시피는 기본폴더에 추가 + legacy favorite 생성 후 true 반환")
        void savesToDefaultBookWhenNotSaved() {
            Recipe recipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();

            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), recipe.getId()))
                    .willReturn(false);
            given(recipeRepo.findById(recipe.getId())).willReturn(Optional.of(recipe));
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(itemRepo.save(any(RecipeBookItem.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));
            // legacy sync
            given(favoriteRepo.existsByUserIdAndRecipeId(user.getId(), recipe.getId()))
                    .willReturn(false);
            given(userRepo.getReferenceById(user.getId())).willReturn(user);
            given(favoriteRepo.save(any())).willAnswer(invocation -> invocation.getArgument(0));

            boolean result = bookService.toggleSave(user.getId(), recipe.getId());

            assertThat(result).isTrue();
            verify(itemRepo).save(any(RecipeBookItem.class));
            verify(favoriteRepo).save(any());
        }

        @Test
        @DisplayName("이미 저장된 레시피는 모든 폴더에서 제거 + legacy favorite 삭제 후 false 반환")
        void removesFromAllBooksWhenAlreadySaved() {
            Long recipeId = 50L;
            Recipe recipe = Recipe.builder().id(recipeId).user(user).isPrivate(false).build();
            RecipeBook customBook = RecipeBook.builder()
                    .id(20L).user(user).name("커스텀").recipeCount(3).build();

            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), recipeId))
                    .willReturn(true);
            given(itemRepo.countByUserIdAndRecipeIdGroupByBookId(user.getId(), recipeId))
                    .willReturn(List.of(
                            new Object[]{defaultBook.getId(), 1L},
                            new Object[]{customBook.getId(), 1L}
                    ));
            given(bookRepo.findById(defaultBook.getId())).willReturn(Optional.of(defaultBook));
            given(bookRepo.findById(customBook.getId())).willReturn(Optional.of(customBook));
            given(itemRepo.deleteAllByUserIdAndRecipeId(user.getId(), recipeId)).willReturn(2);
            // legacy sync
            RecipeFavorite fav = RecipeFavorite.builder()
                    .id(300L).user(user).recipe(recipe).build();
            given(favoriteRepo.findByUserIdAndRecipeId(user.getId(), recipeId))
                    .willReturn(Optional.of(fav));

            boolean result = bookService.toggleSave(user.getId(), recipeId);

            assertThat(result).isFalse();
            verify(itemRepo).deleteAllByUserIdAndRecipeId(user.getId(), recipeId);
            verify(favoriteRepo).delete(fav);
            assertThat(customBook.getRecipeCount()).isEqualTo(2);
        }

        @Test
        @DisplayName("비공개 타인 레시피 저장 시 RECIPE_PRIVATE_ACCESS_DENIED 예외")
        void throwsWhenPrivateRecipeNotOwned() {
            User otherUser = User.builder().id(999L).build();
            Recipe privateRecipe = Recipe.builder().id(50L)
                    .user(otherUser).isPrivate(true).build();

            given(itemRepo.existsByUserIdAndRecipeId(user.getId(), privateRecipe.getId()))
                    .willReturn(false);
            given(recipeRepo.findById(privateRecipe.getId())).willReturn(Optional.of(privateRecipe));

            assertThatThrownBy(() -> bookService.toggleSave(user.getId(), privateRecipe.getId()))
                    .isInstanceOf(CustomException.class)
                    .satisfies(e -> assertThat(((CustomException) e).getErrorCode())
                            .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED));
        }
    }

    // ── saveToDefaultBookIfAbsent ──

    @Nested
    @DisplayName("saveToDefaultBookIfAbsent")
    class SaveToDefaultBookIfAbsent {

        @Test
        @DisplayName("기본폴더에 없으면 추가")
        void addsToDefaultBookWhenAbsent() {
            Recipe recipe = Recipe.builder().id(50L)
                    .user(user).isPrivate(false).build();

            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(itemRepo.existsByBookIdAndRecipeId(defaultBook.getId(), recipe.getId()))
                    .willReturn(false);
            given(recipeRepo.findById(recipe.getId())).willReturn(Optional.of(recipe));
            given(itemRepo.save(any(RecipeBookItem.class)))
                    .willAnswer(invocation -> invocation.getArgument(0));

            bookService.saveToDefaultBookIfAbsent(user.getId(), recipe.getId());

            verify(itemRepo).save(any(RecipeBookItem.class));
        }

        @Test
        @DisplayName("이미 기본폴더에 있으면 무시")
        void skipsWhenAlreadyInDefaultBook() {
            given(bookRepo.findByUserIdAndIsDefaultTrue(user.getId()))
                    .willReturn(Optional.of(defaultBook));
            given(itemRepo.existsByBookIdAndRecipeId(defaultBook.getId(), 50L))
                    .willReturn(true);

            bookService.saveToDefaultBookIfAbsent(user.getId(), 50L);

            verify(itemRepo, never()).save(any());
        }
    }
}
