package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.config.DevVisibilityProperties;
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
 * V1 prod의 `togglePrivacy`와 다른 점:
 *  - PUBLIC↔PRIVATE 단순 토글이 아닌 **명시적 visibility 지정** (PRIVATE/PUBLIC/RESTRICTED 모두 지원)
 *  - 트리플 동기화는 항상 `recipe.applyVisibility()` 헬퍼만 사용 → 단일 setter로 인한 invariant 깨짐 차단
 *  - V1과 동일한 도메인 가드 유지: AI 생성 + PUBLIC 전환 + 이미지 없음 → CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE
 *  - V1과 동일하게 OpenSearch 인덱스 갱신 (afterCommit hook → updatePrivacyStatusSafely)
 *  - dev V3 강화: 라이프사이클 가드 — non-ACTIVE 레시피의 visibility 변경 차단 (V1엔 없음)
 *
 * RESTRICTED 활성화 (Phase A3 완료 후, feature flag로 환경별 통제):
 *  - {@code dev.visibility.restricted-enabled=true} (dev 검증 환경)일 때만 허용
 *  - default false (운영) — applyVisibility(RESTRICTED)가 isPrivate=false로 매핑되어 운영 V1/V2 검색이
 *    isPrivate=false만 보면 RESTRICTED가 운영에 노출됨. 운영 search가 listingStatus 흡수까지 차단 유지.
 *  - 활성 시 트리플: visibility=RESTRICTED, listingStatus=UNLISTED, isPrivate=false
 *    → dev 검색/목록 경로(A2/A3)는 listingStatus=LISTED 필터로 자연 차단
 *    → owner detail direct link 접근은 허용 (검색에서는 빠지고 link 공유 전용)
 *  - dev mirror enabled면 OpenSearch dev alias도 자동 갱신 (A1 Batch 2의 mirrorReindex hook)
 *
 * 권한: 작성자 본인만 변경 가능 (ADMIN 권한 분기는 미포함 — 필요 시 별도 admin endpoint).
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeVisibilityService {

    private final RecipeRepository recipeRepository;
    private final RecipeIndexingService recipeIndexingService;
    private final DevVisibilityProperties devVisibilityProperties;

    @Transactional
    public DevVisibilityUpdateResponse updateVisibility(Long recipeId,
                                                        RecipeVisibility newVisibility,
                                                        Long currentUserId) {
        // RESTRICTED는 feature flag로 환경별 통제 (default false).
        // 운영 V1/V2 검색이 isPrivate=false만 보므로, flag off에서 RESTRICTED 허용 시 PRIVATE→RESTRICTED 전환이
        // 운영 검색에 갑자기 노출되는 운영 데이터 노출 사고가 발생함.
        if (newVisibility == RecipeVisibility.RESTRICTED && !devVisibilityProperties.isRestrictedEnabled()) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                    "RESTRICTED는 dev.visibility.restricted-enabled=true 환경에서만 허용 (운영 검색 노출 방지).");
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

        // 4. 트리플 동기화 — applyVisibility 헬퍼만 사용 (단일 setter 직접 호출 금지)
        recipe.applyVisibility(newVisibility);
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
                recipe.getListingStatus(),
                newIsPrivate
        );
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
