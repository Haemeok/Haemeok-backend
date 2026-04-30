package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;

import java.math.BigDecimal;

/**
 * Normalizer 출력 — 한 줄에 대한 해결 결과.
 *
 * <p>1.3 persist 단계에서 이 record를 읽어 RecipeIngredient entity 필드를 채운다.
 * record는 entity 의존을 가지지만 read-only 참조라 DB 의존이 아닌 in-memory 데이터일 뿐이다.
 *
 * <p><b>C' bypass row 식별</b>: status=PARTIAL이고 {@code ingredientId}=null이고
 * {@code ingredientUnitId}!=null이면 C' bypass. 이 경우 calculator는 unitOwnerIngredient를
 * 통해 canonical 영양 source를 복원한다.
 */
public record NormalizedLine(
        // raw 보존 (모든 row)
        String rawName,
        String rawQuantityText,
        String rawUnitText,
        boolean customByUser,

        // 정규화
        BigDecimal amountValue,        // 파싱 가능했으면 채움
        BigDecimal normalizedGrams,    // amount × edibleGramsPerUnit, 둘 다 있을 때만

        // 매핑 ID — DB persist 시 그대로 사용
        Long ingredientId,             // C' bypass / UNRESOLVED / CUSTOM에서는 null
        Long ingredientUnitId,         // unit 매칭 안 됐거나 의도적으로 안 들어가면 null

        IngredientResolutionStatus status,

        // entity 참조 — calculator가 그대로 받아 쓸 수 있게
        Ingredient resolvedIngredient,    // ingredient_id != null일 때 그 entity
        Ingredient unitOwnerIngredient,   // C' bypass row에서 unit→ingredient로 복원한 entity (calc source)
        IngredientUnit resolvedUnit,      // unit_id != null일 때 그 entity

        // 명시값 — path 3 계산용 (custom 또는 사용자 명시 입력)
        BigDecimal customCalorie,
        Integer customPrice,
        BigDecimal customCarbohydrate,
        BigDecimal customProtein,
        BigDecimal customFat,
        BigDecimal customSugar,
        BigDecimal customSodium,
        String customLink
) {

    public boolean isBypassRow() {
        return status == IngredientResolutionStatus.PARTIAL
                && ingredientId == null
                && ingredientUnitId != null;
    }
}
