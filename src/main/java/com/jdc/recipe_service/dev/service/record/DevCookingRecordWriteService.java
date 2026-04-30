package com.jdc.recipe_service.dev.service.record;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.entity.CookingRecord;
import com.jdc.recipe_service.service.CookingRecordService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Dev V3 cooking-record write dispatcher.
 *
 * <h3>🚨 운영 leak 차단 (createCookingRecord)</h3>
 * 운영 {@link CookingRecordService#createCookingRecord}는 recipe를 {@code findById}만 하고 visibility 검사 없음 →
 * RESTRICTED/PRIVATE non-owner 또는 non-ACTIVE 레시피에도 record 생성 가능 → 본인 collection에 RESTRICTED 흔적이 남음.
 * dev V3는 {@link DevRecipeAccessValidator} 게이트로 차단.
 *
 * <h3>delete: cleanup right</h3>
 * 운영 {@link CookingRecordService#deleteCookingRecord}는 ownership check ({@code USER_ACCESS_DENIED}) 있음 + recipe
 * 가시성 무관. dev V3도 동일 — 본인 record는 lifecycle/visibility 무관 정리 가능 (예: 예전 PUBLIC일 때 만든 record를
 * recipe가 나중에 RESTRICTED로 바뀌어도 정리 가능).
 */
@Service
@RequiredArgsConstructor
public class DevCookingRecordWriteService {

    private final DevRecipeAccessValidator accessValidator;
    private final CookingRecordService cookingRecordService;

    @Transactional
    public CookingRecord createCookingRecord(Long userId, Long recipeId) {
        // dev V3 strict: recipe 가시성 게이트 (운영은 visibility 검사 없음)
        accessValidator.loadAndCheckInteractable(recipeId, userId);
        return cookingRecordService.createCookingRecord(userId, recipeId);
    }

    @Transactional
    public void deleteCookingRecord(Long userId, Long recordId) {
        // cleanup right — 운영의 ownership check만으로 충분
        cookingRecordService.deleteCookingRecord(userId, recordId);
    }
}
