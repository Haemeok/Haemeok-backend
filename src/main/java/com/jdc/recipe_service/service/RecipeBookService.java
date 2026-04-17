package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipebook.*;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.repository.RecipeBookItemRepository;
import com.jdc.recipe_service.domain.repository.RecipeBookRepository;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class RecipeBookService {

    private static final int MAX_BOOKS_PER_USER = 20;
    private static final String DEFAULT_BOOK_NAME = "저장한 레시피";

    private final RecipeBookRepository bookRepo;
    private final RecipeBookItemRepository itemRepo;
    private final RecipeFavoriteRepository favoriteRepo;
    private final UserRepository userRepo;
    private final RecipeRepository recipeRepo;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s",
                bucketName, region, key);
    }

    // ── 기본 레시피북 자동 생성 (public: FavoriteService에서도 사용) ──
    // DB의 uk_recipe_books_one_default_per_user 유니크 제약으로 유저당 1개 보장.

    @Transactional
    public RecipeBook ensureDefaultBook(Long userId) {
        return bookRepo.findByUserIdAndIsDefaultTrue(userId)
                .orElseGet(() -> createDefaultBookSafely(userId));
    }

    private RecipeBook createDefaultBookSafely(Long userId) {
        try {
            var user = userRepo.getReferenceById(userId);
            RecipeBook defaultBook = RecipeBook.builder()
                    .user(user)
                    .name(DEFAULT_BOOK_NAME)
                    .isDefault(true)
                    .displayOrder(0)
                    .build();
            return bookRepo.saveAndFlush(defaultBook);
        } catch (DataIntegrityViolationException e) {
            log.debug("default book already created by concurrent request: userId={}", userId);
            return bookRepo.findByUserIdAndIsDefaultTrue(userId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_BOOK_NOT_FOUND));
        }
    }

    // ── 레시피북 CRUD ──

    @Transactional
    public RecipeBookResponse createBook(Long userId, CreateRecipeBookRequest request) {
        ensureDefaultBook(userId);

        long count = bookRepo.countByUserId(userId);
        if (count >= MAX_BOOKS_PER_USER) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_LIMIT_EXCEEDED);
        }

        if (bookRepo.existsByUserIdAndName(userId, request.getName())) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_DUPLICATE_NAME);
        }

        int nextOrder = bookRepo.findMaxDisplayOrderByUserId(userId) + 1;
        var user = userRepo.getReferenceById(userId);

        RecipeBook book = RecipeBook.builder()
                .user(user)
                .name(request.getName())
                .displayOrder(nextOrder)
                .build();

        bookRepo.save(book);
        return RecipeBookResponse.from(book, 0);
    }

    @Transactional
    public List<RecipeBookResponse> listBooks(Long userId) {
        ensureDefaultBook(userId);

        List<RecipeBook> books = bookRepo.findByUserIdOrderByDisplayOrderAsc(userId);
        Map<Long, Integer> countByBookId = loadAccessibleCounts(userId);

        return books.stream()
                .map(b -> RecipeBookResponse.from(
                        b, countByBookId.getOrDefault(b.getId(), 0)))
                .toList();
    }

    public RecipeBookDetailResponse getBookDetail(Long userId, Long bookId, Pageable pageable) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        // 쿼리 레벨에서 접근 가능한 레시피만 조회 (공개 OR 내 레시피)
        Slice<RecipeBookItem> itemSlice =
                itemRepo.findAccessibleByBookIdAndUserId(bookId, userId, pageable);

        List<RecipeBookItemResponse> items = itemSlice.getContent().stream()
                .map(item -> RecipeBookItemResponse.from(
                        item, generateImageUrl(item.getRecipe().getImageKey())))
                .toList();

        // 접근 가능한 레시피 수로 count 계산 (denormalized count 대신)
        int accessibleCount = itemRepo.countAccessibleByBookIdAndUserId(bookId, userId);

        return RecipeBookDetailResponse.builder()
                .id(book.getId())
                .name(book.getName())
                .isDefault(book.isDefault())
                .recipeCount(accessibleCount)
                .recipes(items)
                .hasNext(itemSlice.hasNext())
                .build();
    }

    @Transactional
    public RecipeBookResponse renameBook(Long userId, Long bookId, RenameRecipeBookRequest request) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        if (book.isDefault()) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_RENAME);
        }

        if (!book.getName().equals(request.getName())
                && bookRepo.existsByUserIdAndName(userId, request.getName())) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_DUPLICATE_NAME);
        }

        book.rename(request.getName());
        int accessibleCount = itemRepo.countAccessibleByBookIdAndUserId(bookId, userId);
        return RecipeBookResponse.from(book, accessibleCount);
    }

    @Transactional
    public void deleteBook(Long userId, Long bookId) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        if (book.isDefault()) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_DELETE);
        }

        // 이 폴더에만 있던 레시피는 legacy favorite도 함께 제거
        List<RecipeBookItem> items = itemRepo.findByBookId(bookId);
        for (RecipeBookItem item : items) {
            Long recipeId = item.getRecipe().getId();
            // 다른 폴더에도 있는지 확인 (현재 폴더 제외)
            long otherFolderCount = itemRepo.countByUserIdAndRecipeIdExcludingBook(
                    userId, recipeId, bookId);
            if (otherFolderCount == 0) {
                favoriteRepo.findByUserIdAndRecipeId(userId, recipeId).ifPresent(fav -> {
                    favoriteRepo.delete(fav);
                    fav.getRecipe().decreaseFavoriteCount();
                });
            }
        }

        // JPA 레벨에서 items를 먼저 삭제해야 TransientObjectException 방지
        itemRepo.deleteByBookId(bookId);
        bookRepo.delete(book);
    }

    @Transactional
    public List<RecipeBookResponse> reorderBooks(Long userId, ReorderRecipeBooksRequest request) {
        List<RecipeBook> books = bookRepo.findByUserIdOrderByDisplayOrderAsc(userId);
        Map<Long, RecipeBook> bookMap = books.stream()
                .collect(Collectors.toMap(RecipeBook::getId, Function.identity()));

        List<Long> orderedIds = request.getBookIds();

        Set<Long> requestIdSet = new HashSet<>(orderedIds);
        if (requestIdSet.size() != orderedIds.size()
                || requestIdSet.size() != bookMap.size()
                || !requestIdSet.equals(bookMap.keySet())) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE);
        }

        for (int i = 0; i < orderedIds.size(); i++) {
            bookMap.get(orderedIds.get(i)).updateDisplayOrder(i);
        }

        Map<Long, Integer> countByBookId = loadAccessibleCounts(userId);

        return books.stream()
                .sorted((a, b) -> Integer.compare(a.getDisplayOrder(), b.getDisplayOrder()))
                .map(b -> RecipeBookResponse.from(
                        b, countByBookId.getOrDefault(b.getId(), 0)))
                .toList();
    }

    private Map<Long, Integer> loadAccessibleCounts(Long userId) {
        List<Object[]> rows = itemRepo.countAccessibleByUserIdGroupByBookId(userId);
        Map<Long, Integer> result = new HashMap<>();
        for (Object[] row : rows) {
            Long bookId = ((Number) row[0]).longValue();
            int count = ((Number) row[1]).intValue();
            result.put(bookId, count);
        }
        return result;
    }

    // ── 레시피북 아이템 관리 ──

    @Transactional
    public AddRecipesToBookResponse addRecipesToBook(Long userId, Long bookId, AddRecipesToBookRequest request) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        // 중복 제거
        List<Long> uniqueIds = request.getRecipeIds().stream().distinct().toList();

        int addedCount = 0;
        int skippedCount = 0;

        for (Long recipeId : uniqueIds) {
            // 이미 이 폴더에 있으면 skip
            if (itemRepo.existsByBookIdAndRecipeId(bookId, recipeId)) {
                skippedCount++;
                continue;
            }

            Recipe recipe = recipeRepo.findById(recipeId).orElse(null);
            if (recipe == null || !isAccessible(recipe, userId)) {
                skippedCount++;
                continue;
            }

            RecipeBookItem item = RecipeBookItem.builder()
                    .book(book)
                    .recipe(recipe)
                    .build();

            // 첫 폴더 추가인 경우 legacy favorite도 동기화
            boolean wasAlreadySaved = itemRepo.existsByUserIdAndRecipeId(userId, recipeId);

            itemRepo.save(item);
            book.incrementRecipeCount();
            addedCount++;

            if (!wasAlreadySaved && !favoriteRepo.existsByUserIdAndRecipeId(userId, recipeId)) {
                var user = userRepo.getReferenceById(userId);
                favoriteRepo.save(RecipeFavorite.builder()
                        .user(user)
                        .recipe(recipe)
                        .build());
                recipe.increaseFavoriteCount();
            }
        }

        return AddRecipesToBookResponse.builder()
                .addedCount(addedCount)
                .skippedCount(skippedCount)
                .build();
    }

    @Transactional
    public void removeRecipesFromBook(Long userId, Long bookId, RemoveRecipesFromBookRequest request) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        int deletedCount = itemRepo.deleteByBookIdAndRecipeIdIn(bookId, request.getRecipeIds());
        book.decrementRecipeCount(deletedCount);

        // 마지막 폴더에서 빠진 레시피는 legacy favorite도 제거
        for (Long recipeId : request.getRecipeIds()) {
            if (!itemRepo.existsByUserIdAndRecipeId(userId, recipeId)) {
                favoriteRepo.findByUserIdAndRecipeId(userId, recipeId).ifPresent(fav -> {
                    favoriteRepo.delete(fav);
                    fav.getRecipe().decreaseFavoriteCount();
                });
            }
        }
    }

    /**
     * 레시피 삭제 시 모든 폴더의 recipeCount를 보정한다.
     * RecipeFavoriteService.deleteByRecipeId()에서 호출.
     */
    @Transactional
    public void adjustCountsBeforeRecipeDeletion(Long recipeId) {
        List<Object[]> affectedBooks = itemRepo.countByRecipeIdGroupByBookId(recipeId);
        for (Object[] row : affectedBooks) {
            Long bookId = ((Number) row[0]).longValue();
            int count = ((Number) row[1]).intValue();
            bookRepo.findById(bookId).ifPresent(book -> book.decrementRecipeCount(count));
        }
    }

    // ── 저장 토글 (하트 ON/OFF) ──

    /**
     * 저장 토글: 어떤 폴더에도 없으면 기본폴더에 추가, 하나라도 있으면 모든 폴더에서 제거.
     * legacy recipe_favorites와 Recipe.favoriteCount도 함께 갱신한다 (Phase 2에서 제거 예정).
     * @return true = 저장됨 (기본폴더에 추가), false = 저장 해제됨 (모든 폴더에서 제거)
     */
    @Transactional
    public boolean toggleSave(Long userId, Long recipeId) {
        boolean alreadySaved = itemRepo.existsByUserIdAndRecipeId(userId, recipeId);

        if (alreadySaved) {
            // 모든 폴더에서 제거 + recipeCount 보정
            List<Object[]> affectedBooks = itemRepo.countByUserIdAndRecipeIdGroupByBookId(userId, recipeId);
            for (Object[] row : affectedBooks) {
                Long bookId = ((Number) row[0]).longValue();
                int count = ((Number) row[1]).intValue();
                bookRepo.findById(bookId).ifPresent(book -> book.decrementRecipeCount(count));
            }
            itemRepo.deleteAllByUserIdAndRecipeId(userId, recipeId);

            // legacy: recipe_favorites도 제거 + favoriteCount 감소
            favoriteRepo.findByUserIdAndRecipeId(userId, recipeId).ifPresent(fav -> {
                favoriteRepo.delete(fav);
                fav.getRecipe().decreaseFavoriteCount();
            });

            return false;
        }

        // 기본폴더에 추가
        Recipe recipe = recipeRepo.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (!isAccessible(recipe, userId)) {
            throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
        }

        RecipeBook defaultBook = ensureDefaultBook(userId);
        RecipeBookItem item = RecipeBookItem.builder()
                .book(defaultBook)
                .recipe(recipe)
                .build();
        itemRepo.save(item);
        defaultBook.incrementRecipeCount();

        // legacy: recipe_favorites도 추가 + favoriteCount 증가
        if (!favoriteRepo.existsByUserIdAndRecipeId(userId, recipeId)) {
            var user = userRepo.getReferenceById(userId);
            favoriteRepo.save(RecipeFavorite.builder()
                    .user(user)
                    .recipe(recipe)
                    .build());
            recipe.increaseFavoriteCount();
        }

        return true;
    }

    /**
     * 기본폴더에 레시피를 저장한다 (이미 있으면 무시).
     * YouTube 추출 등 자동 저장에서 사용.
     */
    @Transactional
    public void saveToDefaultBookIfAbsent(Long userId, Long recipeId) {
        RecipeBook defaultBook = ensureDefaultBook(userId);

        if (itemRepo.existsByBookIdAndRecipeId(defaultBook.getId(), recipeId)) {
            return;
        }

        Recipe recipe = recipeRepo.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        RecipeBookItem item = RecipeBookItem.builder()
                .book(defaultBook)
                .recipe(recipe)
                .build();
        itemRepo.save(item);
        defaultBook.incrementRecipeCount();
    }

    // ── 레시피 저장 상태 조회 ──

    public RecipeSaveStatusResponse getSaveStatus(Long userId, Long recipeId) {
        List<RecipeBook> books = itemRepo.findBooksByRecipeIdAndUserId(recipeId, userId);

        List<RecipeSaveStatusResponse.SavedBookInfo> bookInfos = books.stream()
                .map(b -> RecipeSaveStatusResponse.SavedBookInfo.builder()
                        .id(b.getId())
                        .name(b.getName())
                        .isDefault(b.isDefault())
                        .build())
                .toList();

        return RecipeSaveStatusResponse.builder()
                .saved(!bookInfos.isEmpty())
                .savedBookCount(bookInfos.size())
                .books(bookInfos)
                .build();
    }

    // ── 내부 헬퍼 ──

    private boolean isAccessible(Recipe recipe, Long userId) {
        if (recipe.getUser() != null && recipe.getUser().getId().equals(userId)) {
            return true;
        }
        return !recipe.getIsPrivate();
    }

    private RecipeBook findBookAndVerifyOwner(Long userId, Long bookId) {
        RecipeBook book = bookRepo.findById(bookId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_BOOK_NOT_FOUND));

        if (!book.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_ACCESS_DENIED);
        }

        return book;
    }
}
