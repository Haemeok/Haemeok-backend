package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.service.SurveyService;
import org.springframework.stereotype.Component;

import java.util.Collections;
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
                : null;

        String allergyPref = (survey != null && survey.getAllergy() != null && !survey.getAllergy().isBlank())
                ? survey.getAllergy()
                : null;

        Set<String> themePrefs =
                (survey != null && survey.getTags() != null && !survey.getTags().isEmpty())
                        ? survey.getTags()
                        : Collections.emptySet();

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

        String allowedUnits = unitService.unitsAsString();
        String unitMapping = unitService.mappingAsString();
        String tagsJson = themePrefs.isEmpty()
                ? "[]"
                : "[\"" + String.join("\", \"", themePrefs) + "\"]";

        String ingredientsWithUnits = names.stream()
                .map(name -> name + "(" + unitService.getDefaultUnit(name).orElse("g") + ")")
                .collect(Collectors.joining(", "));


        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 요리 전문가야.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> persona = "너는 재료의 풍미와 조리 원리를 극대화하여 **가장 맛있고 풍부한 맛을 구현하는 전문 요리사야.**";
        }

        String cookingTimePart = (request.getCookingTime() != null && request.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", request.getCookingTime())
                : "- 희망 조리 시간 정보가 제공되지 않았습니다. AI 모델이 자동으로 예상 조리 시간을 추정하세요.";

        String servingsPart = (request.getServings() != null && request.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", request.getServings())
                : "- 인분 수 정보가 제공되지 않았습니다. AI 모델이 적절히 판단하여 작성하세요.";


        String systemAndBaseRules = String.format("""
                [SYSTEM] 너는 요리 전문가 AI로서, 오직 하나의 완전한 JSON만 출력해야 합니다.
                설명, 주석, 마크다운, ```json 등 절대 포함하지 마세요.
                **반드시 아래 규칙을 100%% 준수하여 레시피를 생성하세요.**
                
                %s
                
                **[단위 및 DB 재료 정보]**
                - 허용 단위: [%s]
                - 다음 재료들은 반드시 기본 단위로 작성해야 합니다:
                [%s]
                ※ 'unit' 필드는 위 매핑에서 지정된 단위 외에는 절대 사용 불가합니다.
                
                - DB에 이미 있는 재료: [%s]
                - DB에 없는 재료: [%s]
                
                오직 단 하나의 JSON 객체 형태로만 출력하세요.
                아래 규칙을 반드시 준수하여 레시피를 생성하세요.
                """, persona, allowedUnits, unitMapping, knownList, unknownList);


        String jsonFormatRules = """
                **[JSON 출력 형식 규칙]**
                
                --- [🚨 CRITICAL WARNING: 숫자 필드 NULL/공백 절대 금지 🚨] ---
                - **모든 숫자 필드**(`quantity`, `customPrice`, `caloriesPerUnit`, `marketPrice`, `cookingTime`, `servings`, `protein`, `carbohydrate`, `fat`, `sugar`, `sodium`)는 **0.00 이상의 유효한 숫자만** 허용됩니다.
                - **절대로 빈 문자열("") 또는 null 값을 사용하지 마세요.** 이를 위반하면 JSON 전체가 무효화되고 에러가 발생합니다.
                ---
                
                **[JSON 세부 필드 규칙]**
                아래는 JSON 필드 각각의 세부 규칙입니다. 반드시 지켜주세요.
                
                --- "title" 필드 (제목 강화 규칙) ---
                - 제목은 **주재료 + 맛 표현 + 요리명** 형식으로 작성
                - 예: '매콤 돼지고기 김치찌개', '얼큰한 두부 김치찌개'
                - 너무 간결한 '김치찌개' 금지
                - 인분/시간 포함 가능 (예: '2인분 25분 매콤 김치찌개')
                
                --- "dishType" 필드 (요리 유형 규칙) ---
                - `dishType`은 반드시 요청된 값("%s")을 **그대로 사용**하거나 (요청에 없을 시) 아래 목록에서 하나만 선택하세요:
                  볶음, 국/찌개/탕, 구이, 무침/샐러드, 튀김/부침, 찜/조림, 오븐요리, 생식/회, 절임/피클류, 밥/면/파스타, 디저트/간식류
                - **절대 빈 문자열("")이나 공백으로 출력되어서는 안 됩니다.**
                
                --- "description" 필드 ---
                - 음식에 대한 설명과 후기를 첨부하세요.
                
                --- "ingredients" 필드 (재료 필드 강제 규칙 - 반드시 준수) ---
                - DB에 없는 재료(%s)는 **반드시** 아래 2개 필드 포함:
                  - `customPrice`: 1단위당 가격 (정수, 원)
                  - `caloriesPerUnit`: 1단위당 칼로리 (정수 또는 소수점 첫째 자리, kcal). `unit`이 'g'일 경우 1g당 칼로리를 계산하여 소수로 넣으세요.
                  - 이 필드 누락 시 출력 전체 무효
                - DB에 있는 재료는 `customPrice`, `caloriesPerUnit` **절대 포함 금지**
                - 또한 모든 재료의 quantity는 요청된 인분 수에 맞추어 자동으로 조절해야 하며, 기본 1인분 기준으로 자연스럽게 확장하거나 축소된 값으로 작성해야 합니다. 인분 수가 제공되지 않은 경우 모델이 적절한 기본 인분을 가정하여 일관성 있게 계산하세요.
                - 재료별 기본 단위 매핑: {%s}
              
                --- "steps" 필드 (단계 규칙) ---
                - "steps" 배열의 "action" 필드는 반드시 아래 19개 중 하나만 사용해야 합니다:
                  썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
                - 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값("")이 될 수 없습니다.
                - "steps" 배열 안의 각 객체는 "stepNumber", "instruction", "action" 키를 모두 포함해야 합니다.
                
                --- "tags" 필드 (태그 규칙) ---
                - 요청한 태그 배열 %s의 원소를 절대로 수정·누락하지 말 것.
                - 만약 요청 태그가 []라면, AI는 아래 허용 목록 중 음식 분위기에 맞는 태그를 **최대 3개** 골라서 반환해야 합니다:
                  🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥, 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절, 🍱 도시락, 🔌 에어프라이어, 🍲 해장
                
                [CRITICAL 태그 선택 조건] 섹션 내 전체 규칙
                - **Servings 기준:** Servings가 **1인분일 때만** '🍽️ 혼밥' 태그를 선택 가능합니다.
                - **시간 기준:** '⚡ 초스피드 / 간단 요리' 태그는 CookingTime이 **15분 이내**일 경우에만 선택 가능합니다.
                - **조리 방식 기준:** '🔌 에어프라이어' 태그는 레시피의 **`cookingTools` 필드에 '오븐' 또는 '에어프라이어'가 명시적으로 포함**되어 있거나, `dishType`이 **'구이'** 또는 **'튀김/부침'**에 해당될 경우에만 선택 가능합니다.
                - **건강 기준:** '🥗 다이어트 / 건강식' 태그는 레시피의 지방(`fat`) 및 당분(`sugar`) 함량이 **총 영양 성분의 10%% 미만**인 경우에만 선택 가능합니다.
                - **나머지 태그 (홈파티, 야식, 술안주 등):** 레시피의 분위기나 재료에 따라 AI가 자유롭게 판단하여 선택합니다.
                - **배제 규칙:** Servings가 2인분 초과일 경우 '🍽️ 혼밥' 태그를 절대 선택 불가. 지방/칼로리가 높거나 조리 시간이 20분 초과(오븐/찜 포함)일 경우 '⚡ 초스피드 / 간단 요리' 또는 '🥗 다이어트 / 건강식' 태그를 절대 선택 불가.
                
                --- "marketPrice" 필드 (배달 가격 규칙) ---
                - 레시피 전체 **실제 예상 배달 가격** (정수, 원)을 한국 배달 앱 기준으로 현실적으로 추정하세요.
                - **[CRITICAL PRICE RULE]** 배달 가격은 **원가, 인건비, 포장비, 마진**을 모두 포함해야 하므로, **절대로 저렴한 가격으로 책정해서는 안 됩니다.** 일반적인 **배달 전문점**의 메뉴판 가격(예: 1인분당 최소 9,000원 이상)을 기준으로 **충분히 현실적인 고가**로 설정하세요.
               
                --- "nutrition" 필드 (영양성분 규칙) ---
                - 레시피 전체의 총 영양성분 정보를 객체 형태로 **반드시** 포함해야 합니다.
                - `protein`, `carbohydrate`, `fat`, `sugar`는 **레시피 전체 기준**의 최종 합산값(BigDecimal)으로 출력하세요. (소수점 2자리까지 허용)
                - **[CRITICAL]** `sodium` 수치는 1인분 기준 **1800mg**을 절대 초과할 수 없으며, 요리의 종류와 간의 정도에 따라 현실적으로 추정해야 합니다. **(참고: 일반적인 1인분 나트륨은 500mg ~ 1000mg 선에서 유연하게 결정되어야 합니다.)**
                - **이 필드들(`protein`, `carbohydrate`, `fat`, `sugar`, `sodium`)은 0.00 이상의 숫자로 채워져야 하며, null이나 빈 문자열은 절대 허용되지 않습니다.**
                
                --- "cookingTips" 필드 (팁 규칙) ---
                - **서빙 / 맛 강화 / 재활용 / 보조 재료 대체 팁 3~5개**를 생성하세요.
                - 보조 재료 대체 가능하지만, 요리 본연의 맛과 취지를 해치지 않는 범위에서만 허용됩니다. (예: 고춧가루 → 청양고추 O)
                - 반드시 문장 단위로 이어서 작성하고, 숫자나 목록 표시(1, 2, 3...)는 사용하지 마세요.

                --- 기타 필드 ---
                - `cookingTime`, `cookingTools`, `servings`는 요청 조건과 요리 원리에 맞춰 적절히 작성하세요.
                
                
                [단계 설명 규칙 - 전문 레시피처럼 자연스럽고 품질감 있게]
                - **각 단계는 자연스럽고 논리적인 흐름으로 구성** (재료 손질 → 풍미 베이스 → 본 조리 → 마무리)
                - **[CRITICAL 단계 규칙]** 재료 손질(썰기, 다지기) 단계와 양념장/마리네이드 준비(섞기, 담그기) 단계를 **논리적으로 분리**하여 명확성을 높이세요. 재료 손질 단계를 끝낸 후 다음 단계에서 양념 준비를 시작하세요.
                - **초보자도 바로 따라할 수 있도록 (묘사 강화):** 불 세기, 시간, 재료 상태 변화를 구체적인 형용사나 부사를 사용하여 묘사하세요.
                - **문장 구성:** 2~3개 문장, 최대 150자 이내로 작성하고 끝은 '주세요', '하세요', '합니다' 등 자연스럽게 마무리하세요.
                - **보조 설명:** 조리 과정에 대한 보조 설명이나 팁은 별도로 분리하지 않고, 현재 단계의 instruction 뒤에 자연스러운 다음 문장으로 연결하여 추가하세요.
                
                
                [요리 원리 규칙]
                1. **(핵심)** 찌개·볶음·조림 요리에서는 기름에 주재료나 향신채(마늘·파 등)를 먼저 볶아 풍미의 기초를 다지는 과정을 최우선으로 고려하세요.
                2. 효율적이고 논리적인 순서로 단계를 구성하세요.
                3. 요청에 없더라도 필수 보조 재료를 자유롭게 추가하고 'ingredients'에 포함시키세요.
                4. **[CRITICAL] 각 재료의 양은 요청된 인분 수(servings)에 100%% 비례하여 조정할 것.**
                   - **(최우선 무게 기준)** AI는 모든 주재료(육류, 해산물, 생선)에 대해 **1인분당 최소 180g ~ 200g의 포만감**을 충족하도록 추정해야 합니다.
                   - **[개수-무게 계산 공식 적용]** 주재료가 '마리'나 '개' 단위일 경우, **1마리(개)당 평균 무게를 논리적으로 추정**하고, **1인분 최소 무게(180g)를 해당 무게로 나누어 필요한 개수**를 계산해야 합니다.
                   - 양념 및 부재료도 주재료의 양에 맞춰 풍미를 충분히 낼 수 있도록 비례하여 증가시킬 것.
                5. **[핵심: 맛의 완성도 목표]** 레시피는 **실제 전문 요리사가 만든 것처럼 깊은 맛**을 내야 하며, **맛의 깊이(단짠 균형, 감칠맛)**와 **풍미(향)**를 최우선 목표로 구현하세요.
                   - **(기능적 충족)** 볶음/조림 요리 시, 육류나 해산물의 **잡내를 제거**하는 재료(예: 미림, 청주)를 사용하고, 마지막에 **고소한 마무리 향**을 더하는 재료(예: 참기름, 들기름)를 **요리의 종류에 맞춰 자유롭게 선택**하세요.
                   - **(편향 제거)** AI가 자체적으로 가진 **건강이나 다이어트 편향을 버리고**, **가장 맛있고 풍부한 결과물**을 만들도록 양념을 **충분하고 복합적**으로 설정해야 합니다.
                6. **[새 재료 단위 추론 원칙]** DB에 없는 **새로운 재료**의 단위를 설정할 때, 해당 재료의 **물리적 특성**에 따라 가장 현실적인 단위를 선택해야 합니다.
                    - **액체류 (오일, 우유, 청주 등):** 무게(`g`) 대신 **부피 단위(`ml`, `큰술`, `작은술`)**를 우선 사용합니다.
                    - **분말류 (가루, 향신료):** 무게(`g`) 또는 **용량 단위(`큰술`, `작은술`)**를 사용합니다.
                    - **고형물 (채소, 고기):** 무게(`g`)를 기본으로 하되, 크기가 일정하면 **개수(`개`, `모`, `마리`)**를 사용할 수 있습니다.
                """.formatted(
                request.getDishType() != null && !request.getDishType().isBlank() ? request.getDishType() : "",
                unknownList,
                unitMapping,
                tagsJson
        );


        String fewShotExample = """
                {
                  "title": "샘플 요리 제목",
                  "dishType": "볶음",
                  "description": "이것은 JSON 구조를 보여주기 위한 샘플 레시피입니다. 내용을 복사하지 마세요.",
                  "cookingTime": 20,
                  "cookingTools": ["팬", "주걱"],
                  "servings": 2.0,
                  "ingredients": [
                    { "name": "주재료A", "quantity": "100", "unit": "g", "customPrice": 50, "caloriesPerUnit": 3.0 },
                    { "name": "주재료B", "quantity": "1", "unit": "개" },
                    { "name": "보조재료C", "quantity": "1", "unit": "작은술" }
                  ],
                  "steps": [
                    { "stepNumber": 0, "instruction": "재료를 손질합니다.", "action": "손질하기" },
                    { "stepNumber": 1, "instruction": "팬에 재료를 볶습니다.", "action": "볶기" }
                  ],
                  "tags": ["🍽️ 혼밥"],
                  "marketPrice": 8000,
                   "cookingTips": "팁1. 팁2. 팁3.",
                   "nutrition": {
                     "protein": 15.25,
                     "carbohydrate": 45.50,
                     "fat": 10.75,
                     "sugar": 12.80,
                     "sodium": 500
                   }
                }
                """;

        String requestContext = """
                요청 조건:
                - 요리 유형: %s
                %s
                %s
                - 매운맛 선호도: %s/5
                - 알레르기 정보: %s
                - 요리 테마 선호 태그: %s
                - 주요 재료: %s
                - 태그: %s
                """.formatted(
                request.getDishType() != null && !request.getDishType().isBlank() ? request.getDishType() : "AI 자동 선택",
                cookingTimePart,
                servingsPart,
                spicePref != null ? spicePref : "기본",
                allergyPref != null && !allergyPref.isBlank() ? allergyPref : "없음",
                tagsJson,
                ingredientsWithUnits,
                tagsJson
        );

        return systemAndBaseRules
                + "\n\n--- 예시 JSON ---\n"
                + fewShotExample
                + "\n--- 예시 끝 ---\n"
                + "\n\n" + requestContext
                + "\n\n" + jsonFormatRules;
    }
}