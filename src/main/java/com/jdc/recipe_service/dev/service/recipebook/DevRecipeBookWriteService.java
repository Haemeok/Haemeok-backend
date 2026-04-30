package com.jdc.recipe_service.dev.service.recipebook;

import com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.CreateRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RemoveRecipesFromBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RenameRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.ReorderRecipeBooksRequest;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 recipe-book write dispatcher.
 *
 * 6 endpoint mirror — 대부분은 단순 위임이지만 {@code addRecipesToBook}만 dev V3 strict 필터 적용.
 *
 * <h3>🚨 운영 leak 차단 (addRecipesToBook)</h3>
 * 운영 {@link RecipeBookService#addRecipesToBook}은 내부 {@code isAccessible(recipe, userId)}로 visibility 검사하지만
 * 이는 V1 {@code isPrivate=false} 만 검사 → RESTRICTED는 {@code isPrivate=false}로 매핑되므로 통과 → 누구나 RESTRICTED
 * non-owner 레시피를 자기 폴더에 추가 가능 (존재 누설 + 본인 컬렉션 변조).
 *
 * dev V3는 {@link DevRecipeAccessPolicy#isAccessibleBy}로 batch projection 사전 필터 → 접근 가능 ID만 운영 service에
 * 전달. {@code skippedCount}는 운영 skipped (이미 폴더에 있음) + dev filtered (RESTRICTED 등) 합산.
 *
 * <h3>나머지 endpoint</h3>
 * createBook / renameBook / deleteBook / reorderBooks / removeRecipesFromBook 모두 ownership + 비즈니스 검증을 운영
 * service가 처리 + recipe visibility 무관 → 단순 위임 + payload null guard.
 */
@Service
@RequiredArgsConstructor
public class DevRecipeBookWriteService {

    private final RecipeBookService recipeBookService;
    private final DevRecipeAccessProjectionRepository accessProjectionRepository;

    @Transactional
    public RecipeBookResponse createBook(Long userId, CreateRecipeBookRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        return recipeBookService.createBook(userId, request);
    }

    @Transactional
    public RecipeBookResponse renameBook(Long userId, Long bookId, RenameRecipeBookRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        return recipeBookService.renameBook(userId, bookId, request);
    }

    @Transactional
    public void deleteBook(Long userId, Long bookId) {
        recipeBookService.deleteBook(userId, bookId);
    }

    @Transactional
    public List<RecipeBookResponse> reorderBooks(Long userId, ReorderRecipeBooksRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        return recipeBookService.reorderBooks(userId, request);
    }

    @Transactional
    public AddRecipesToBookResponse addRecipesToBook(Long userId, Long bookId, AddRecipesToBookRequest request) {
        if (request == null || request.getRecipeIds() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문 또는 recipeIds가 비어있습니다.");
        }

        // 중복 제거 (운영도 동일 처리)
        List<Long> requestedIds = request.getRecipeIds().stream().distinct().toList();

        // dev V3 strict pre-filter: 운영 isAccessible은 isPrivate만 봐서 RESTRICTED 통과 → 4-enum accessibleBy로 차단.
        // empty input도 short-circuit 없이 운영 service에 위임 — findBookAndVerifyOwner가 ownership check를 강제하도록.
        // (HTTP 진입은 @NotEmpty로 막히지만 service-layer 정책 경계에서 ownership 우회를 허용하지 않음)
        Set<Long> accessibleIds = filterAccessibleIds(requestedIds, userId);
        int devFilteredCount = requestedIds.size() - accessibleIds.size();
        List<Long> filteredIds = requestedIds.stream().filter(accessibleIds::contains).toList();

        // 운영 service 항상 호출 — empty/all-blocked 경우에도 book ownership check 강제
        AddRecipesToBookResponse opResponse = recipeBookService.addRecipesToBook(
                userId, bookId,
                AddRecipesToBookRequest.builder().recipeIds(filteredIds).build());

        // 합산: 운영 skipped (이미 폴더에 있음) + dev filtered (RESTRICTED/non-ACTIVE/존재 안 함 등)
        return AddRecipesToBookResponse.builder()
                .addedCount(opResponse.getAddedCount())
                .skippedCount(opResponse.getSkippedCount() + devFilteredCount)
                .build();
    }

    @Transactional
    public void removeRecipesFromBook(Long userId, Long bookId, RemoveRecipesFromBookRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        // 게이트 없음 — cleanup right (본인 폴더에서 RESTRICTED 레시피라도 제거 가능)
        recipeBookService.removeRecipesFromBook(userId, bookId, request);
    }

    private Set<Long> filterAccessibleIds(List<Long> recipeIds, Long viewerId) {
        if (recipeIds.isEmpty()) return Collections.emptySet();
        List<DevRecipeAccessProjection> projections =
                accessProjectionRepository.findAccessProjectionsByIds(recipeIds);
        return projections.stream()
                .filter(p -> DevRecipeAccessPolicy.isAccessibleBy(
                        p.lifecycleStatus(), p.visibility(), p.listingStatus(), viewerId, p.ownerId()))
                .map(DevRecipeAccessProjection::recipeId)
                .collect(Collectors.toSet());
    }
}
