package com.jdc.recipe_service.dev.service.report;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import com.jdc.recipe_service.domain.repository.RecipeIngredientReportRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientNormalizer;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

/**
 * Dev V3 재료 신고(report) 처리.
 *
 * <h3>운영 ReportService와 다른 점</h3>
 * <ul>
 *   <li><b>visibility 게이트</b>: dev V3 strict — RESTRICTED/PRIVATE non-owner / non-ACTIVE 레시피
 *       신고 차단. 운영은 visibility 검사 없이 report 생성 → report 테이블에 비공개 레시피 진입 +
 *       ingredients 조회로 간접 정보 누설 가능.</li>
 *   <li><b>raw-first 매칭</b>: 운영은 ingredient.name 또는 customName만 비교. dev는 raw 보존 정책상
 *       상세 화면이 rawName > customName > ingredient.name 우선순위로 표시되므로, 신고 매칭도
 *       같은 우선순위 + canonicalizeName 정규화로 진행해야 사용자가 본 row와 실제 신고가 정확히
 *       일치한다. 안 그러면 "감자"로 보이는 row를 신고했는데 다른 row에 연결되거나 매칭 실패.</li>
 * </ul>
 *
 * <p>운영 {@code ReportService}는 V1 호환을 위해 그대로 유지 — dev path만 자체 매칭 로직 사용.
 */
@Service
@RequiredArgsConstructor
public class DevReportService {

    private final DevRecipeAccessValidator accessValidator;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeIngredientReportRepository reportRepository;

    @Transactional
    public void createReportByName(Long userId, Long recipeId, IngredientReportRequest request) {
        if (request == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "요청 본문이 비어있습니다.");
        }
        // dev V3 strict: recipe 가시성 게이트
        accessValidator.loadAndCheckInteractable(recipeId, userId);

        String rawTarget = request.getIngredientName();
        if (rawTarget == null || rawTarget.isBlank()) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "신고할 재료 이름이 비어있습니다.");
        }
        String canonicalTarget = RecipeIngredientNormalizer.canonicalizeName(rawTarget);

        List<RecipeIngredient> ingredients = recipeIngredientRepository.findByRecipeId(recipeId);

        // raw-first: rawName > customName > ingredient.name. canonicalize로 공백 변형 흡수.
        Optional<RecipeIngredient> match = ingredients.stream()
                .filter(ri -> matchesByRawFirst(ri, canonicalTarget))
                .findFirst();

        // legacy 호환: 매칭되면 ingredient.id (없으면 ri.id) 저장.
        Long matchedId = null;
        if (match.isPresent()) {
            RecipeIngredient target = match.get();
            matchedId = (target.getIngredient() != null)
                    ? target.getIngredient().getId()
                    : target.getId();
        }

        RecipeIngredientReport report = RecipeIngredientReport.builder()
                .recipeId(recipeId)
                .ingredientId(matchedId)
                .memberId(userId)
                .reason(request.getReason())
                .userMemo(request.getMemo())
                .proposedName(rawTarget)
                .build();

        reportRepository.save(report);
    }

    /**
     * raw-first 매칭: row의 <b>표시명 하나</b>(rawName ?? customName ?? ingredient.name)와 신고 target을 비교.
     *
     * <p><b>중요</b>: 셋 중 하나라도 일치하면 true가 아님 — 사용자가 화면에서 본 표시명만 비교한다.
     * rawName="청양고추" + ingredient.name="고추"인 row에 "고추" 신고가 잘못 붙는 사고를 막는다 (이 row의
     * 표시명은 "청양고추"이지 "고추"가 아님). 표시 우선순위는 {@link RecipeIngredientDisplayResolver}와
     * 동일해야 한다.
     */
    private static boolean matchesByRawFirst(RecipeIngredient ri, String canonicalTarget) {
        if (canonicalTarget.isEmpty()) return false;
        String displayName = pickDisplayName(ri);
        if (displayName == null) return false;
        String canonical = RecipeIngredientNormalizer.canonicalizeName(displayName);
        return !canonical.isEmpty() && canonical.equals(canonicalTarget);
    }

    /** 표시명 결정: rawName > customName > ingredient.name. 셋 다 비면 null. */
    private static String pickDisplayName(RecipeIngredient ri) {
        if (ri.getRawName() != null && !ri.getRawName().isBlank()) return ri.getRawName();
        if (ri.getCustomName() != null && !ri.getCustomName().isBlank()) return ri.getCustomName();
        if (ri.getIngredient() != null && ri.getIngredient().getName() != null
                && !ri.getIngredient().getName().isBlank()) {
            return ri.getIngredient().getName();
        }
        return null;
    }
}
