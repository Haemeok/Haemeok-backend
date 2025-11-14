package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.service.SurveyService;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class PromptBuilderV3 {

    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final SurveyService surveyService;

    public PromptBuilderV3(UnitService unitService, IngredientRepository ingredientRepo, SurveyService surveyService) {
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
        } else if (request.getTags() != null && !request.getTags().isEmpty()) {
            themePrefs = new HashSet<>(request.getTags());
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

        String criticalIngredientRule = String.format("""
                **[!! Critical Rule: DB에 없는 재료의 가격/칼로리 !!]**
                - DB에 없는 재료(%s)는 **반드시** JSON의 'ingredients' 배열 내에서
                  **'customPrice' (1단위당 가격)와 'caloriesPerUnit' (1단위당 칼로리)** 필드를 **누락 없이** 포함해야 합니다.
                - 이 필드들의 누락은 전체 JSON 응답의 무효화로 간주되며, **서버 오류를 유발합니다**.
                """, unknownList);

        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가야.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> persona = "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사야.";
        }

        String stepRules = """
                **[단계 설명 규칙 - 전문 레시피처럼 자연스럽고 품질감 있게]**
                1. **각 단계는 자연스럽고 논리적인 흐름으로 구성**
                   - 1단계: 재료 손질 → 2단계: 풍미 베이스 → 3단계: 본 조리 → 4단계: 마무리
                2. **초보자도 바로 따라할 수 있도록 (묘사 강화)**
                   - 불 세기, 시간, 재료 상태 변화 명확히
                   - '볶아', '끓여', '익혀' 등 동사 활용
                   - '충분히', '골고루', '약불로 줄여' 등 감각적 표현
                   - **조리 중 재료의 색깔, 농도, 향 등 상태 변화를 구체적인 형용사나 부사를 사용하여 묘사하세요.**
                3. **조리 과정에 대한 보조 설명이나 팁은 별도로 티 내지 않고, 현재 단계의 instruction 뒤에 자연스러운 다음 문장으로 연결하여 추가하세요. (추가할 내용이 없다면 생략 가능합니다.)**
                   - 예시: "냄비에 식용유를 두르고 중불에서 마늘과 돼지고기를 볶아 고기 표면이 하얗게 익을 때까지 2분간 볶습니다. 이때 돼지고기가 눌어붙지 않도록 계속 저어주는 것이 좋습니다."
                4. **문장은 2~3개로 구성 가능, 최대 150자 이내**
                   - 문장 끝은 '주세요', '하세요', '합니다' 등 자연스럽게
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

        String systemRole = """
                [SYSTEM] 너는 요리 전문가 AI로서, 오직 하나의 완전한 JSON만 출력해야 합니다.
                설명, 주석, 마크다운, ```json 등 절대 포함하지 마세요.
                반드시 아래 규칙을 100% 준수하세요.
                """;

        String dishTypeRule;
        if (request.getDishType() == null || request.getDishType().isBlank()) {
            dishTypeRule = """
                    **[dishType 자동 선택 규칙]**
                    - 요청에 요리 유형이 없으면, 주요 재료와 요리 스타일에 맞춰 아래 목록에서 **하나만 선택**하세요:
                      볶음, 국/찌개/탕, 구이, 무침/샐러드, 튀김/부침, 찜/조림, 오븐요리, 생식/회, 절임/피클류, 밥/면/파스타, 디저트/간식류
                    - 예: [돼지고기, 김치] → "국/찌개/탕"
                    - 예: [밥, 계란] → "볶음" 또는 "밥/면/파스타"
                    - 절대 "김치찌개", "볶음밥" 같은 요리명 사용 금지
                    """;
        } else {
            dishTypeRule = String.format("""
                    **[dishType 강제 규칙]**
                    - `dishType`은 반드시 요청된 값("%s")을 **그대로 사용**하세요.
                    - 절대 변경하지 마세요.
                    """, request.getDishType());
        }

        String fieldExtension = """
                    **[재료 필드 강제 규칙 - 반드시 준수]**
                    11) **DB에 없는 재료**(아래 목록)는 **반드시** 아래 2개 필드 포함:
                        - `customPrice`: **1단위당 가격** (정수, 원)
                          → `unit`이 `g`면 1g당, `개`면 1개당
                          → 예: 아보카도 `unit="개"`, `customPrice=1600`
                        - `caloriesPerUnit`: **1단위당 칼로리** (정수 또는 소수점 첫째 자리, kcal)
                          → **중요:** `unit`이 `g`일 경우, 100g당 칼로리가 아니라 **1g당 칼로리**를 계산하여 소수로 넣으세요.
                          → 예: 아보카도 `caloriesPerUnit=320`
                          → 예: 돼지고기(100g/250kcal)일 경우 `caloriesPerUnit=2.5`
                          → **이 필드 누락 시 출력 전체 무효**
                    12) **DB에 있는 재료**는 `customPrice`, `caloriesPerUnit` **절대 포함 금지**
                
                    **[새 필드 규칙]**
                    13) **marketPrice**: 레시피 전체 **실제 배달가격** (정수, 원)
                        - 한국 배달 앱(배달의민족, 요기요) 기준 현실 가격 추정
                        - 재료 총 가격 + 요리 유형(프리미엄 시 +20%) + 배송비(3,000원) 고려
                        - 2025년 서울 기준 (인플레이션 반영, 볶음밥 평균 8,000~12,000원)
                        - 예: 아보카도 김치 볶음밥 2인분 → 9,000원 (기본 7,000 + 아보카도 1,500 + 배송 3,000 - 할인 1,500)
                    14) **cookingTips**: **서빙 / 맛 강화 / 재활용 / 보조 재료 대체 팁 3~5개** (한 줄 문자열)
                        - **메인 재료 변형 절대 금지** (예: 돼지고기 → 닭고기 X)
                        - **보조 재료 대체 가능하지만, 요리 본연의 맛과 취지를 해치지 않는 범위에서만**
                          → 예: 고춧가루 → 청양고추 (매운맛 유지) O
                          → 예: 버터 → 올리브 오일 (고소함 유지) O
                          → 예: 와인 → 레몬즙 (산미 유지) O
                          → 예: 양파 → 대파 (아삭함+단맛 유지) O
                          → 예: 설탕 → 꿀 (단맛 유지) O
                        - **절대 빈 문자열 금지**
                        - 각 팁 앞에 번호: "1. ", "2. " / "."으로 구분
                        - 예: "1. 고춧가루 대신 청양고추 1개 다져 넣으면 매운맛 유지. 2. 양파 대신 대파 흰 부분 사용 시 아삭함 유지. 3. 버터 대신 올리브 오일로 고소함 살리기. 4. 와인 대신 레몬즙 1작은술로 산미 조절. 등등"
                
                    **[영양성분 객체 강제]**
                    15) **nutrition**: 레시피 전체의 총 영양성분 정보를 객체 형태로 **반드시** 포함해야 합니다.
                         - **proteinG, carbohydrateG, fatG, sugarG**는 **100g 기준의 영양소 함량**이 아닌, **레시피 전체 기준**의 최종 합산값(BigDecimal)으로 출력하세요. (소수점 2자리까지 허용)
                         - **sodiumMg**는 나트륨 최종 합산값(정수, mg)으로 출력하세요.
                         - 모든 필드는 0이상의 값을 가지며, **절대로 누락 금지** (null 또는 빈 문자열 금지).
                """;

        String titleRule = """
                **[title 강화 규칙]**
                - 제목은 **주재료 + 맛 표현 + 요리명** 형식으로 작성
                - 예: '매콤 돼지고기 김치찌개', '얼큰한 두부 김치찌개'
                - 너무 간결한 '김치찌개' 금지
                - 인분/시간 포함 가능 (예: '2인분 25분 매콤 김치찌개')
                """;

        String strictIngredientRule = """
                **[CRITICAL: 요청된 재료 100% 사용 - 반드시 준수]**
                - 이 재료들은 **모두 ingredients 배열에 반드시 포함**해야 합니다.
                - **누락은 절대 허용되지 않으며**, 누락 시 출력이 무효로 간주됩니다.
                - 예시 JSON은 **형식 참고용일 뿐**, 재료나 제목은 **절대 따라하지 마세요**.
                """;

        String fewShotExample = """
                {
                  "title": "매콤 돼지고기 김치찌개",
                  "dishType": "국/찌개/탕",
                  "description": "매콤한 김치와 돼지고기의 조화로 따뜻한 국물이 일품인 전통 찌개입니다.",
                  "cookingTime": 25,
                  "cookingTools": ["냄비", "숟가락"],
                  "servings": 2.0,
                  "ingredients": [
                    { "name": "김치", "quantity": "200", "unit": "g" },
                    { "name": "돼지고기", "quantity": "150", "unit": "g",
                      "customPrice": 35, "caloriesPerUnit": 2.5 },
                    { "name": "두부", "quantity": "0.5", "unit": "모" },
                    { "name": "양파", "quantity": "1", "unit": "개" },
                    { "name": "마늘", "quantity": "3", "unit": "개" },
                    { "name": "고춧가루", "quantity": "1", "unit": "작은술" },
                    { "name": "진간장", "quantity": "1", "unit": "큰술" },
                    { "name": "물", "quantity": "800", "unit": "ml" },
                    { "name": "식용유", "quantity": "1", "unit": "큰술" }
                  ],
                  "nutrition": {
                      "proteinG": 30.50,
                      "carbohydrateG": 70.20,
                      "fatG": 25.00,
                      "sugarG": 10.00,
                      "sodiumMg": 1500
                  },
                  "steps": [
                    { "stepNumber": 0, "instruction": "돼지고기를 2cm 크기로 썰고, 김치는 3cm 길이로 썰어 준비합니다.", "action": "썰기" },
                    { "stepNumber": 1, "instruction": "냄비에 식용유를 두르고 중불에서 마늘과 돼지고기를 볶아 고기 표면이 하얗게 익을 때까지 2분간 볶습니다.", "action": "볶기" },
                    { "stepNumber": 2, "instruction": "김치와 고춧가루를 넣고 3분간 더 볶아 김치 향이 올라오도록 합니다.", "action": "볶기" },
                    { "stepNumber": 3, "instruction": "물을 붓고 끓인 후 양파와 두부를 넣고 약불로 15분간 끓입니다.", "action": "끓이기" },
                    { "stepNumber": 4, "instruction": "간장을 넣고 간을 맞춘 후 불을 끄고 2분간 뜸 들입니다.", "action": "끓이기" }
                  ],
                  "tags": ["🍲 해장", " 🍽️ 혼밥"],
                  "marketPrice": 10000,
                  "cookingTips": "1. 고춧가루 대신 청양고추 1개 다져 매운맛 유지. 2. 양파 대신 대파 흰 부분 썰어 아삭함 유지. 3. 진간장 대신 국간장 사용 시 짠맛 조절. 4. 식용유 대신 올리브 오일로 고소함 유지. 5. 두부를 마지막에 넣어 부드러움 살리기."
                }
                """;

        return String.format("""
                        %s
                        %s
                        %s
                        **DB에 이미 있는 재료**: [%s]
                        **DB에 없는 재료**: [%s]
                        
                        %s
                        
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
                        2) 요청한 \"tags\" 배열 %s의 순서를 절대로 수정·누락하지 말 것.
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
                systemRole,
                unitTable,
                persona,
                knownList,
                unknownList,
                criticalIngredientRule,
                stepRules,
                dishTypeRule,
                request.getDishType(),
                tagsJson,
                tagsJson,
                allowedUnits,
                unitMapping,
                fieldExtension,
                titleRule,
                strictIngredientRule,
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