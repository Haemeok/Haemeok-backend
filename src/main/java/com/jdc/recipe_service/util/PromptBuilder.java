package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.service.SurveyService;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
public class PromptBuilder {

    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final SurveyService surveyService;

    private static final List<String> BASE_SEASONINGS = List.of(
            "소금","후추","설탕","양조간장","진간장","국간장",
            "식용유","올리브유","참기름","들기름",
            "고춧가루","고추장","된장","식초",
            "다진마늘","다진생강",
            "멸치액젓","까나리액젓","김치국물",
            "물엿","올리고당","조청","꿀",
            "치킨스톡","다시다"
    );

    public PromptBuilder(UnitService unitService, IngredientRepository ingredientRepo, SurveyService surveyService) {
        this.unitService = unitService;
        this.ingredientRepo = ingredientRepo;
        this.surveyService = surveyService;
    }

    public String buildPrompt(AiRecipeRequestDto request, RobotType type) {
        UserSurveyDto survey = surveyService.getSurvey(request.getUserId());
        Integer spicePref = (survey != null && survey.getSpiceLevel() != null)
                ? survey.getSpiceLevel()
                : request.getSpiceLevel();
        String allergyPref = (survey != null && survey.getAllergy() != null && !survey.getAllergy().isBlank())
                ? survey.getAllergy()
                : request.getAllergy();

        Set<String> themePrefs;
        if (survey != null && survey.getTags() != null && !survey.getTags().isEmpty()) {
            themePrefs = survey.getTags();
        } else if (request.getTagNames() != null && !request.getTagNames().isEmpty()) {
            themePrefs = new HashSet<>(request.getTagNames());
        } else {
            themePrefs = Collections.emptySet();
        }

        List<String> names = request.getIngredients();

        List<String> known = ingredientRepo.findAllByNameIn(names)
                .stream()
                .map(Ingredient::getName)
                .collect(Collectors.toList());
        List<String> unknown = names.stream()
                .filter(n -> !known.contains(n))
                .collect(Collectors.toList());
        String knownList = known.isEmpty() ? "없음" : String.join(", ", known);
        String unknownList = unknown.isEmpty() ? "없음" : String.join(", ", unknown);

        Map<String, String> unitLockMap = unitService.getUnitsFor(names);
        unitService.getUnitsFor(BASE_SEASONINGS).forEach(unitLockMap::putIfAbsent);
        String unitLockJson = unitService.toUnitLockJson(unitLockMap);


        String unitLockBlock = String.format("""
                [UNIT_LOCK – 재료별 단위 고정]
                아래 매핑에 포함된 재료만 단위를 고정한다. (매핑에 없는 재료 = 신규 재료)
                unit_lock = %s
                
                출력 규칙:
                1) recipe.ingredients 는 "요청 재료"에 더해, 꼭 필요한 보조 재료(기본 양념/오일/육수 등)를 최소한으로 추가할 수 있다.
                   추가한 보조 재료가 DB에 있는 재료라면 해당 재료의 기본 단위를 사용해야 한다(정확히 일치).
                2) unit_lock에 존재하는 재료:
                   - ingredients[*].unit 은 해당 name의 unit과 **완전히 동일**해야 한다.
                     (띄어쓰기/표기변형/한영치환/접두·접미 금지)
                   - quantity 값은 자유지만, 단위는 반드시 unit_lock 고정값 사용.
                   - quantity는 실제 요리 맥락에 자연스러운 값이어야 한다. 0/음수 금지, 과도한 소수 금지(소수점 2자리 이내).
                   - (스케일 규칙) 예시 JSON은 2인분 기준이며, 모든 quantity는 (요청 인분 ÷ 2)로 비례 조정 후 반올림한다.
                     g/ml는 5 단위로 반올림, 큰술/작은술/컵은 0.25 단위, 개/대/모는 0.25 단위로 반올림한다.
                3) unit_lock에 **없는** 재료(신규 재료):
                   - 단, unit_lock에는 없지만 DB에 존재하는 재료는 customPrice/caloriesPerUnit를 포함하지 않는다.
                   - 자연스러운 unit을 하나 선택해 사용(g/개/마리/모/컵/ml/큰술/작은술 등).
                   - [재료 필드 확장] 규칙에 따라 `customPrice`(선택 unit 1개당 가격, 정수),
                     `caloriesPerUnit`(선택 unit 1개당 칼로리, double) **반드시 포함**.
                4) steps[*].ingredients 에서는 unit 필드를 쓰지 말고 name(필요 시 quantity)만 표기.
                5) unit_lock 위반 시 아래 오류 JSON만 반환:
                   {"error":"UNIT_MISMATCH","details":["<name>: expected=<unit>, actual=<unit>"]}
                """, unitLockJson);


        String persona = switch (type) {
            case CREATIVE -> "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가야.";
            case HEALTHY -> "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사야.";
        };

        String stepRules = """
                **[요리 단계 설명 규칙]**
                1. 조리 시간은 ‘MM분 SS초’ 형식. 0인 단위는 생략(예: 30초, 3분).
                2. 색/향/식감 변화 묘사.
                3. 불 세기(강/중/약), 투입 타이밍, 뚜껑 사용 등 주의사항 명시.
                """;

        String cookingTimePart = (request.getCookingTime() != null && request.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", request.getCookingTime())
                : "- 희망 조리 시간 정보가 없음. AI가 적절히 추정.";

        String servingsPart = (request.getServings() != null && request.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", request.getServings())
                : "- 인분 수 정보가 없음. AI가 적절히 판단.";

        String tagsJson = (themePrefs == null || themePrefs.isEmpty())
                ? "[]"
                : "[\"" + String.join("\", \"", themePrefs) + "\"]";

        String preferencePart = String.format("""
                        - 매운맛 선호도: %s/5
                        - 알레르기 정보: %s
                        - 요리 테마 선호 태그: %s
                        """,
                spicePref != null ? spicePref : "기본",
                allergyPref != null && !allergyPref.isBlank() ? allergyPref : "없음",
                tagsJson
        );

        String ingredientsWithUnits = names.stream()
                .map(n -> {
                    String u = unitLockMap.get(n);
                    return (u != null) ? n + "(" + u + ")" : n;
                })
                .collect(Collectors.joining(", "));

        String fieldExtension = """
                **[재료 필드 확장]**
                - DB에 없는 재료만 아래 필드를 포함하세요(기존 재료는 절대 포함 금지):
                  - `customPrice`: 선택한 unit 1개당 가격(원, 정수)
                  - `caloriesPerUnit`: 선택한 unit 1개당 칼로리(double, 숫자)
                """;

        String fewShotExample = """
                {
                  "title": "돼지고기 김치찌개",
                  "dishType": "국/찌개/탕",
                  "description": "기름에 김치와 돼지고기를 충분히 볶아내어 깊고 진한 국물 맛이 일품인 정통 김치찌개입니다.",
                  "cookingTime": 30,
                  "cookingTools": ["냄비", "도마", "칼"],
                  "servings": 2.0,
                  "ingredients": [
                     { "name": "돼지고기", "quantity": "150", "unit": "g" },
                     { "name": "신김치",   "quantity": "200", "unit": "g", "customPrice": 300, "caloriesPerUnit": 15 },
                     { "name": "김치국물", "quantity": "0.5", "unit": "컵" },
                     { "name": "두부",     "quantity": "0.5", "unit": "모" },
                     { "name": "대파",     "quantity": "0.5", "unit": "대" },
                     { "name": "양파",     "quantity": "0.25", "unit": "개" },
                     { "name": "들기름",   "quantity": "1",   "unit": "큰술" },
                     { "name": "고춧가루", "quantity": "1",   "unit": "큰술" },
                     { "name": "다진마늘", "quantity": "0.5", "unit": "큰술" },
                     { "name": "설탕",     "quantity": "0.5", "unit": "큰술" },
                     { "name": "멸치육수", "quantity": "500", "unit": "ml" }
                  ],
                  "steps": [
                    { "stepNumber": 0, "instruction": "돼지고기는 한입 크기로, 김치는 2cm 폭으로 썰고, 양파는 채썰고 대파는 어슷썹니다. 두부는 1.5cm 두께로 준비합니다.", "action": "손질하기" },
                    { "stepNumber": 1, "instruction": "중불로 달군 냄비에 들기름 1큰술을 두르고 돼지고기를 넣어 겉면이 익을 때까지 볶습니다.", "action": "볶기" },
                    { "stepNumber": 2, "instruction": "김치를 넣고 3~5분간 충분히 볶아 신맛을 부드럽게 만들고 풍미를 끌어올립니다.", "action": "볶기" },
                    { "stepNumber": 3, "instruction": "멸치육수 500ml와 김치국물 0.5컵을 붓고, 고춧가루·다진마늘·설탕을 넣어 10분간 끓입니다.", "action": "끓이기" },
                    { "stepNumber": 4, "instruction": "양파와 두부를 넣고 5분 더 끓인 뒤, 마지막에 대파를 넣어 한소끔 더 끓여 마무리합니다.", "action": "끓이기" }
                  ],
                  "tagNames": ["🍲 해장", "🍽️ 혼밥"]
                }
                """;

        return String.join("\n\n",
                unitLockBlock,
                persona,
                stepRules,
                "**DB에 이미 있는 재료**: [" + knownList + "]\n**DB에 없는 재료**: [" + unknownList + "]",
                "**오직 단 하나의 JSON 객체 형태로만 출력하세요.**\n\n" +
                        "**[요리 원리 규칙]**\n" +
                        "1. (핵심) 찌개·볶음·조림 요리에서는 기름에 주재료나 향신채를 먼저 볶아 풍미 기초를 만든다.\n" +
                        "2. 손질→조리 순으로 효율적 단계 구성.\n" +
                        "3. 요청에 없더라도 필수 보조 재료는 자유롭게 추가 가능(ingredients에 포함).\n" +
                        "4. 예시 JSON은 2인분 기준. quantity는 “예시 × (요청 인분 ÷ 2)” 비례 조정.\n" +
                        "5. 알레르기/식이 제한 준수(부적합 재료 제외/대체).",
                String.format("""
                        **[출력 형식 규칙]**
                        1) 요청한 "dishType"(%s) 절대 수정/누락 금지.
                        2) 요청한 "tagNames" 배열 %s 의 순서 유지. 
                           - 만약 %s 가 []라면, 아래 허용 목록 중 상황에 맞는 태그 최대 3개 선택:
                             🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥,
                             🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절,
                             🍱 도시락, 🔌 에어프라이어, 🍲 해장
                        3) "steps" 의 "action" 값은 다음 중 하나만 사용:
                           썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림,
                           무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
                        4) 모든 필드는 의미 있는 한글 내용. 빈 문자열 금지.
                        5) 각 step은 "stepNumber", "instruction", "action" 모두 포함.
                        6) JSON 외 불필요 텍스트/주석 금지.
                        7) **ingredients[*].unit 은 UNIT_LOCK에 있는 재료에 한해 지정값과 동일해야 하며, UNIT_LOCK에 없는 재료는 자연스러운 단위를 사용해도 된다.**
                        8) **steps[*].ingredients 에서는 unit 필드 사용 금지.**
                        """, request.getDishType(), tagsJson, tagsJson),
                fieldExtension,
                "--- 예시 JSON ---\n" + fewShotExample + "\n--- 예시 끝 ---",
                String.format("""
                                요청 조건:
                                - 요리 유형: %s
                                %s
                                %s
                                %s
                                - 주요 재료: %s
                                - 태그: %s
                                """,
                        request.getDishType(),
                        cookingTimePart,
                        servingsPart,
                        preferencePart,
                        ingredientsWithUnits,
                        tagsJson)
        );
    }
}