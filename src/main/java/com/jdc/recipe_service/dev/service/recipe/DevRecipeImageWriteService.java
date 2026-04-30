package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.RecipeUploadService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

/**
 * Dev V3 recipe 이미지 write (presigned-urls / finalize / image keys) dispatcher.
 *
 * <h3>운영 leak 차단</h3>
 * <ul>
 *   <li><b>presigned-urls</b>: 운영 {@link RecipeUploadService#generatePresignedUrlsForUpdate}는 ownership 검사 자체가
 *       없음 → 누구나 임의 recipeId로 presigned URL 받아서 다른 사람 이미지 폴더에 업로드 가능 (이미지 덮어쓰기 공격).
 *       dev V3가 owner + ACTIVE 게이트로 차단.</li>
 *   <li><b>finalize</b>: 운영은 owner 또는 admin만 허용 (line 502-504). dev V3는 admin escape 없음 — owner + ACTIVE
 *       strict (관리자는 운영 endpoint 사용).</li>
 *   <li><b>image keys</b>: 운영은 owner check 있음. dev V3는 ACTIVE only 추가 + payload null guard.</li>
 * </ul>
 *
 * 세 endpoint 공통: {@link #validateOwnerAndActive(Long, Long)}로 owner + ACTIVE invariant 통일.
 * non-ACTIVE → RECIPE_NOT_FOUND (admin이 처리한 레시피는 owner에게도 "존재하지 않는 것처럼" — Batch 1 update와 일관).
 */
@Service
@RequiredArgsConstructor
public class DevRecipeImageWriteService {

    private final RecipeRepository recipeRepository;
    private final RecipeUploadService recipeUploadService;
    private final RecipeService recipeService;

    @Transactional(readOnly = true)
    public UpdatePresignedUrlResponse generatePresignedUrlsForUpdate(
            Long userId, Long recipeId, List<FileInfoRequest> files) {
        // payload null guard — pre-DB 차단 (운영 RecipeUploadService는 files.stream() 진입 전 null 체크 없음 → NPE 회피)
        if (files == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "업로드 파일 목록(files)이 비어있습니다.");
        }
        // 운영 leak 차단: ownership 검사 없는 운영 method 앞에 owner + ACTIVE 게이트
        validateOwnerAndActive(recipeId, userId);
        return recipeUploadService.generatePresignedUrlsForUpdate(recipeId, userId, files);
    }

    @Transactional
    public FinalizeResponse finalizeRecipeImages(Long userId, Long recipeId) {
        validateOwnerAndActive(recipeId, userId);
        // dev V3는 admin escape 없음 — isAdmin=false 강제 전달
        return recipeService.finalizeRecipeImages(recipeId, userId, false);
    }

    @Transactional
    public void updateImageKeys(Long userId, Long recipeId, RecipeImageKeyUpdateRequest request) {
        // payload null guard — 운영 NPE 경로 회피
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        validateOwnerAndActive(recipeId, userId);
        // stepImageKeys=null normalize → 빈 리스트 (main-image-only update 지원, 운영 IntStream.range NPE 회피)
        // RecipeImageKeyUpdateRequest는 setter 없는 immutable이라 새 instance로 normalize
        RecipeImageKeyUpdateRequest normalized = request.getStepImageKeys() != null
                ? request
                : new RecipeImageKeyUpdateRequest(request.getImageKey(), Collections.emptyList());
        recipeService.updateImageKeys(recipeId, userId, normalized);
    }

    private void validateOwnerAndActive(Long recipeId, Long userId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        if (recipe.getUser() == null || !recipe.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.RECIPE_ACCESS_DENIED);
        }
        if (recipe.getLifecycleStatus() != RecipeLifecycleStatus.ACTIVE) {
            // admin이 HIDDEN/BANNED 처리한 레시피는 owner에게도 "존재하지 않는 것처럼"
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }
    }
}
