package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

/**
 * Dev V3 레시피 가시성 변경 서비스.
 *
 * <p>V1 prod의 {@code togglePrivacy}와 다른 점:
 * <ul>
 *   <li>PUBLIC↔PRIVATE 단순 토글이 아닌 <b>명시적 visibility 지정</b></li>
 *   <li>트리플 동기화는 의미별 헬퍼({@link Recipe#applyPublicListed}, {@link Recipe#applyPublicUnlisted},
 *       {@link Recipe#applyPrivate})만 사용 — 단일 setter 직접 호출 금지</li>
 *   <li>V1과 동일한 도메인 가드: AI 생성 + PUBLIC 전환 + 이미지 없음 → CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE</li>
 *   <li>V1과 동일하게 OpenSearch 인덱스 갱신 (afterCommit hook → updatePrivacyStatusSafely)</li>
 *   <li>dev V3 강화: 라이프사이클 가드 — non-ACTIVE 레시피의 visibility 변경 차단 (V1엔 없음)</li>
 * </ul>
 *
 * <p><b>PUBLIC 전환 정책 (V1.x)</b>:
 * <ul>
 *   <li>리믹스(originRecipe != null) → {@link Recipe#applyPublicUnlisted} (link-only, discovery 미노출)</li>
 *   <li>일반 원본(originRecipe == null) → {@link Recipe#applyPublicListed} (검색/추천 노출)</li>
 * </ul>
 * 같은 리믹스를 PRIVATE로 숨겼다가 다시 풀 때도 검색/추천에 갑자기 노출되지 않게 보장.
 *
 * <p><b>RESTRICTED는 신규 입력 거부</b>: ACL 기반 권한 제어용으로 도입됐으나 ACL 기능 미보유로 신규 사용 중지.
 * 기존 DB row 디시리얼라이즈 호환을 위해 enum 값과 entity의 deprecated {@code applyVisibility(RESTRICTED)} 분기는
 * 보존하되, 이 service는 외부 입력으로 들어오면 즉시 INVALID_INPUT_VALUE로 거부한다.
 *
 * <p>권한: 작성자 본인만 변경 가능 (ADMIN 권한 분기는 미포함 — 필요 시 별도 admin endpoint).
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeVisibilityService {

    private final RecipeRepository recipeRepository;
    private final RecipeIndexingService recipeIndexingService;

    @Transactional
    public DevVisibilityUpdateResponse updateVisibility(Long recipeId,
                                                        RecipeVisibility newVisibility,
                                                        Long currentUserId) {
        // RESTRICTED는 ACL 기능 부재로 신규 사용 중지. 기존 row만 enum 호환 위해 보존.
        // 외부 입력으로 들어오면 명확한 메시지로 거부 — 운영 검색이 isPrivate=false만 보던 시절
        // RESTRICTED 노출 사고를 막던 feature flag 가드는 이제 의미가 없다 (정책 자체가 차단).
        if (newVisibility == RecipeVisibility.RESTRICTED) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "RESTRICTED는 더 이상 사용할 수 없는 visibility 값입니다. PUBLIC 또는 PRIVATE를 사용해주세요.");
        }

        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        // 2a. 라이프사이클 가드 — DELETED/HIDDEN/BANNED 레시피의 가시성 변경 차단.
        // V1 prod의 togglePrivacy는 이 가드가 없지만 (경계 케이스 미정의), dev V3는 "미래 prod" 기준으로
        // 명시적 차단. 비활성 레시피를 owner가 PUBLIC으로 만들어 admin 조치를 우회하는 것을 막음.
        // 사용자에겐 RECIPE_NOT_FOUND로 응답 (존재하지 않는 것처럼 — V2 detail의 PRIVATE 가드 패턴과 정합).
        if (recipe.getLifecycleStatus() != RecipeLifecycleStatus.ACTIVE) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }

        // 2. 작성자 본인 검증 (ownership)
        if (!isOwner(recipe, currentUserId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }

        // 3. 도메인 가드 — AI 생성 레시피를 이미지 없이 PUBLIC으로 만들 수 없음 (V1 prod와 동일 정책)
        if (newVisibility == RecipeVisibility.PUBLIC
                && recipe.isAiGenerated()
                && recipe.getImageKey() == null) {
            throw new CustomException(ErrorCode.CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE);
        }

        // 4. 트리플 동기화 — 의미별 헬퍼만 사용. PUBLIC 전환은 origin 여부로 listed/unlisted 분기.
        applyTripleSync(recipe, newVisibility);
        boolean newIsPrivate = recipe.getIsPrivate();

        // 5. OpenSearch 인덱스 갱신 — V1과 동일 패턴 (afterCommit으로 DB 커밋 후 비동기 안전).
        // PRIVATE 전환 시 검색 결과에서 즉시 빠지도록 보장 (V1의 RecipeService.togglePrivacy와 동등).
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                recipeIndexingService.updatePrivacyStatusSafely(recipeId, newIsPrivate);
            }
        });

        log.info("✅ [DevRecipeVisibility] 가시성 변경 완료 — recipeId={}, visibility={}, listingStatus={}, isPrivate={}",
                recipeId, recipe.getVisibility(), recipe.getListingStatus(), newIsPrivate);

        return new DevVisibilityUpdateResponse(
                recipe.getVisibility(),
                newIsPrivate
        );
    }

    /**
     * 트리플 동기화 + PUBLIC 전환 시 origin 분기.
     *
     * <ul>
     *   <li>PRIVATE → {@link Recipe#applyPrivate}</li>
     *   <li>PUBLIC + 리믹스(origin 있음) → {@link Recipe#applyPublicUnlisted} (link-only)</li>
     *   <li>PUBLIC + 일반 원본(origin 없음) → {@link Recipe#applyPublicListed} (discovery 노출)</li>
     * </ul>
     */
    private static void applyTripleSync(Recipe recipe, RecipeVisibility newVisibility) {
        switch (newVisibility) {
            case PRIVATE -> recipe.applyPrivate();
            case PUBLIC -> {
                if (recipe.getOriginRecipe() != null) {
                    recipe.applyPublicUnlisted();
                } else {
                    recipe.applyPublicListed();
                }
            }
            case RESTRICTED -> {
                // unreachable — 위에서 INVALID_INPUT_VALUE로 차단됨. 안전망.
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                        "RESTRICTED는 더 이상 사용할 수 없는 visibility 값입니다.");
            }
        }
    }

    /**
     * Recipe 작성자가 currentUser인지 판정. recipe.user는 LAZY라 id만 비교 (불필요한 join 회피).
     * (DevRecipeDetailService와 동일 helper — 후속에 사용처 늘면 별도 util로 추출 검토.)
     */
    private static boolean isOwner(Recipe recipe, Long currentUserId) {
        if (currentUserId == null) return false;
        return recipe.getUser() != null && currentUserId.equals(recipe.getUser().getId());
    }
}
