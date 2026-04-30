package com.jdc.recipe_service.dev.util.prompt;

/**
 * Dev V3 ingredient fallback override 섹션.
 *
 * <p>운영/dev가 공유하는 prompt(`RecipeExtractionService.getExtractionPrompt()` 등) 뒤에 dev facade가
 * 명시적으로 append하는 보조 섹션. 기존 prompt의 "DB 재료면 custom* 금지" 규칙이 있더라도 이 섹션이
 * 마지막 선언이라 우선권을 가진다. dev path에서만 호출되므로 운영 prompt는 변경 없이 안전.
 *
 * <p><b>3 규칙</b>
 * <ul>
 *   <li>DB 재료 + DB unit 모두 인식: custom* 출력 금지 (canonical 우선)</li>
 *   <li>DB 재료는 인식, unit은 알 수 없음: line-total custom* 필수</li>
 *   <li>DB 재료 자체가 미인식: line-total custom* 필수</li>
 * </ul>
 *
 * <p>그리고 단위 변환 절대 금지 — raw 단위/수량을 그대로 둔다.
 *
 * <p>custom* 의미: 그 재료 라인 전체의 원가/영양값. per-unit/per-g 아님.
 * 예: "감자 2덩이"의 customCalories=280이면 "2덩이 전체 280kcal".
 */
public final class DevIngredientPromptOverride {

    private DevIngredientPromptOverride() {}

    /**
     * 명시적 "Dev V3 override" 표지로 시작 — 기존 prompt 규칙보다 우선임을 모델에 알림.
     */
    public static final String SECTION = """

            ## [Dev V3 ingredient fallback override — 이 섹션은 위 모든 규칙보다 우선]

            **단위 변환 절대 금지**: raw_quantity_text와 raw_unit_text는 영상/입력 그대로 둔다.
            "1 국자"를 "30g"로 환산하는 행위 금지. "3쪽"을 "15g"로 바꾸지 마라.

            **custom* (customCalories/customPrice/customCarbohydrate/customProtein/customFat/customSugar/customSodium)
            출력 규칙**:

            1. DB 재료 + DB unit 모두 인식 가능: **custom*는 출력하지 마라**. 서버가 canonical(per-g 기반)으로 계산함.
            2. DB 재료는 인식되지만 unit이 미지원("덩이", "한 줌" 등): **line-total custom*를 반드시 출력**.
               서버가 candidate(UNIT)를 만들어 추후 정식 등록할 때까지 임시 fallback으로 사용한다.
            3. DB 재료 자체가 미인식("황태머리" 등): **line-total custom*를 반드시 출력**.
               서버가 candidate(INGREDIENT)를 만들어 추후 정식 등록할 때까지 fallback으로 사용한다.

            **custom* 의미**: 그 재료 라인 전체에 해당하는 원가(원)/영양(g/mg)값. per-unit 아님, per-g 아님.
            예: "감자 2덩이"의 customCalories는 "2덩이 전체 280kcal"를 의미하는 280.

            DB 재료/단위 인식 여부는 모델이 알 수 없으므로, **확신이 없으면 custom*를 출력하라** (안전한 기본).
            """;
}
