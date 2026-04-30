package com.jdc.recipe_service.service.ingredient.normalize;

/**
 * recipe_ingredients.resolution_status (DB는 String).
 *
 * <p>Helper 패키지에서는 typed enum을 사용하고, persist boundary(1.3)에서 String으로 변환한다.
 * 의미를 코드에서 흐리지 않기 위해 enum 사용.
 *
 * <ul>
 *   <li>MAPPED: ingredient + unit + grams 모두 확정 — 신구조 계산 가능</li>
 *   <li>PARTIAL: 일부 확정. 두 갈래가 있다:
 *     <ul>
 *       <li>ingredient hit + unit miss — 이름은 알지만 단위/수량 미해결</li>
 *       <li>C' bypass row: 같은 레시피에 같은 ingredient_id가 다른 unit으로 또 나타남.
 *           UNIQUE(recipe_id, ingredient_id) 회피로 ingredient_id=null로 저장. 단,
 *           ingredient_unit_id가 살아있으면 calculator가 unit→ingredient로 canonical 복원 가능.</li>
 *     </ul>
 *   </li>
 *   <li>UNRESOLVED: raw는 보존했으나 ingredient 매칭 실패 (시스템적 실패)</li>
 *   <li>CUSTOM: 사용자가 의도적으로 마스터 매핑을 거부. customByUser=true에서만 발생.
 *       AI/YouTube path는 절대 CUSTOM이 될 수 없다 (UNRESOLVED 또는 PARTIAL)</li>
 * </ul>
 */
public enum IngredientResolutionStatus {
    MAPPED,
    PARTIAL,
    UNRESOLVED,
    CUSTOM
}
