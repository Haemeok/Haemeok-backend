package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.service.SurveyService;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class PromptBuilder {

    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final SurveyService surveyService;

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

        String unitMapping = unitService.mappingAsString();
        String allowedUnits = unitService.unitsAsString();
        String unitTable = String.format("""
                다음 재료들은 반드시 기본 단위로 작성해야 합니다:
                {%s}
                
                ※ 'unit' 필드는 위 매핑에서 지정된 단위 외에는 절대 사용 불가합니다.
                """, unitMapping);

        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가야.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> persona = "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사야.";
        }

        String stepRules = """
                **[요리 단계 설명 규칙]**
                1. 조리 시간은 ‘MM분 SS초’ 형식으로 작성하되,
                    - 분 또는 초가 0이면 해당 단위를 생략하세요.
                       (예: 0분 30초 → 30초, 3분 0초 → 3분)
                2. 조리 중 식재료의 색상·향·식감 변화를 묘사하세요.
                3. 불 세기(강불·중불·약불), 재료 투입 타이밍, 뚜껑 사용 등 구체적 주의사항을 안내하세요.
                """;

        String cookingTimePart = (request.getCookingTime() != null && request.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", request.getCookingTime())
                : "- 희망 조리 시간 정보가 제공되지 않았습니다. AI 모델은 자동으로 예상 조리 시간을 추정하세요.";

        String servingsPart = (request.getServings() != null && request.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", request.getServings())
                : "- 인분 수 정보가 제공되지 않았습니다. AI 모델이 적절히 판단하여 작성하세요.";

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
                .map(name -> name + "(" + unitService.getDefaultUnit(name).orElse("g") + ")")
                .collect(Collectors.joining(", "));

        String fieldExtension = """
                **[재료 필드 확장]**
                11) DB에 없는 재료에 대해서만 아래 두 필드를 포함하세요:
                   - `customPrice`: 100g당 가격(원 단위, 정수)
                   - `caloriesPerUnit`: 100g당 칼로리(kcal 단위, 정수)
                12) DB에 있는 재료는 `customPrice`, `caloriesPerUnit` 필드를 절대 포함하지 마세요.
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

        return String.format("""
                        %s
                        %s
                        %s
                        **DB에 이미 있는 재료**: [%s]
                        **DB에 없는 재료**: [%s]
                        
                        **오직 단 하나의 JSON 객체 형태로만 출력하세요.**
                        
                        **아래 규칙을 반드시 준수하여 요리 원리를 고려한 레시피를 생성하세요.**
                        
                        **[요리 원리 규칙]**
                        1. **(핵심)** 찌개·볶음·조림 요리에서는 기름에 주재료나 향신채(마늘·파 등)를 먼저 볶아 풍미의 기초를 다지는 과정을 최우선으로 고려하세요.
                        2. 효율적이고 논리적인 순서로 단계를 구성하세요. (예: 모든 재료 손질 후 조리 시작)
                        3. 요청에 없더라도 필수 보조 재료(기름·맛술·설탕 등)를 자유롭게 추가하고 'ingredients'에 포함시키세요.
                        4. **예시 JSON은 2인분 기준이며, 각 재료의 quantity는 “예시 양 × (요청 인분 수 ÷ 2)” 공식을 적용해 비례 조정할 것.**
                        5. **알레르기 및 식이 제한(예: 견과류 알레르기 시 견과류 완전 배제, 락토-오보 식단 시 버터 대신 들기름 사용) 에 맞춰 부적합 재료는 반드시 제외하거나 대체 재료로 변경하세요.**
                        
                        **[출력 형식 규칙]**
                        1) 요청한 \"dishType\"(%s)을 절대로 수정·누락하지 말 것.
                        2) 요청한 \"tagNames\" 배열 %s의 순서를 절대로 수정·누락하지 말 것.
                           - 만약 %s가 []라면, AI는 아래 허용 목록 중 음식 분위기에 맞는 태그를 최대 3개 골라서 반환해야 합니다.
                           - 허용 목록 (최대 3개 선택):
                             🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥,
                             🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절,
                             🍱 도시락, 🔌 에어프라이어, 🍲 해장
                        3) \"steps\" 배열의 \"action\" 필드는 반드시 아래 19개 중 하나만 사용해야 합니다:
                           썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기,
                           구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기,
                           로스팅, 캐러멜라이즈, 부치기
                        4) 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값(\"\")이 될 수 없습니다.
                        5) \"steps\" 배열 안의 각 객체는 \"stepNumber\", \"instruction\", \"action\" 키를 모두 포함해야 합니다.
                        6) JSON 외에 어떤 텍스트(설명·주석·마커 등)도 절대로 포함하지 마세요.
                        7) \"unit\" 필드는 다음 허용 단위 중 하나만 사용해야 합니다: [%s]
                        8) 재료별 기본 단위 매핑: {%s}
                        
                        %s
                        
                        --- 예시 JSON ---
                        %s
                        --- 예시 끝 ---
                        
                        요청 조건:
                        - 요리 유형: %s
                        %s
                        %s
                        %s
                        - 주요 재료: %s
                        - 태그: %s
                        
                        """,
                unitTable,
                persona,
                stepRules,
                knownList,
                unknownList,
                request.getDishType(),
                tagsJson,
                tagsJson,
                allowedUnits,
                unitMapping,
                fieldExtension,
                fewShotExample,
                request.getDishType(),
                cookingTimePart,
                servingsPart,
                preferencePart,
                ingredientsWithUnits,
                tagsJson
        );
    }
}
