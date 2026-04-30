package com.jdc.recipe_service.dev.service.recipebook;

import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookDetailResponse;
import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookItemResponse;
import com.jdc.recipe_service.dev.repository.recipebook.DevRecipeBookItemQueryRepository;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.repository.RecipeBookRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;

/**
 * Dev V3 레시피북 read service.
 *
 * 운영 {@code RecipeBookService}의 dev 미러:
 *  - book 자체 조회/ownership check은 운영 {@link RecipeBookRepository} 그대로 (book은 사용자 컬렉션이라 정책 무관)
 *  - book 안 레시피만 dev 정책으로 필터 ({@link DevRecipeBookItemQueryRepository})
 *
 * 응답:
 *  - listBooks → {@code List<RecipeBookResponse>} (운영 DTO 그대로 — book 자체 정보)
 *  - getBookDetail → {@link DevRecipeBookDetailResponse} (recipes만 dev DTO로 4 enum 노출)
 *
 * write 계열(create/rename/delete/addRecipes/removeRecipes/reorder)은 후속 phase.
 * ensureDefaultBook은 운영 {@link RecipeBookService}에 위임 — 신규 사용자가 dev API를 먼저 호출해도
 * 기본 북 1개 lazy bootstrap 보장.
 */
@Service
@RequiredArgsConstructor
public class DevRecipeBookService {

    private final RecipeBookRepository bookRepo;
    private final DevRecipeBookItemQueryRepository devItemRepo;
    /** 운영 service의 ensureDefaultBook 위임용 — 신규 사용자가 dev API를 먼저 호출해도 기본 북 1개 보장. */
    private final RecipeBookService recipeBookService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    /**
     * 신규 사용자가 dev API를 먼저 호출해도 운영과 동일하게 기본 북 1개 보장 (lazy bootstrap).
     * INSERT 발생 가능성 때문에 readOnly 트랜잭션으로 두지 않음 — 운영 listBooks와 동일 패턴.
     */
    @Transactional
    public List<RecipeBookResponse> listBooksDev(Long userId) {
        recipeBookService.ensureDefaultBook(userId);

        List<RecipeBook> books = bookRepo.findByUserIdOrderByDisplayOrderAsc(userId);
        Map<Long, Integer> countByBookId = devItemRepo.countAccessibleDevByUserIdGroupByBookId(userId);

        return books.stream()
                .map(b -> RecipeBookResponse.from(b, countByBookId.getOrDefault(b.getId(), 0)))
                .toList();
    }

    @Transactional(readOnly = true)
    public DevRecipeBookDetailResponse getBookDetailDev(Long userId, Long bookId, Pageable pageable) {
        RecipeBook book = findBookAndVerifyOwner(userId, bookId);

        Slice<RecipeBookItem> itemSlice = devItemRepo.findAccessibleDevByBookIdAndUserId(bookId, userId, pageable);

        List<DevRecipeBookItemResponse> items = itemSlice.getContent().stream()
                .map(item -> DevRecipeBookItemResponse.from(
                        item, generateImageUrl(item.getRecipe().getImageKey())))
                .toList();

        int accessibleCount = devItemRepo.countAccessibleDevByBookIdAndUserId(bookId, userId);

        return DevRecipeBookDetailResponse.builder()
                .id(book.getId())
                .name(book.getName())
                .isDefault(book.isDefault())
                .recipeCount(accessibleCount)
                .recipes(items)
                .hasNext(itemSlice.hasNext())
                .build();
    }

    /** 운영 RecipeBookService.findBookAndVerifyOwner와 동일 패턴 (private이라 복제). */
    private RecipeBook findBookAndVerifyOwner(Long userId, Long bookId) {
        RecipeBook book = bookRepo.findById(bookId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_BOOK_NOT_FOUND));
        if (!book.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_BOOK_ACCESS_DENIED);
        }
        return book;
    }
}
