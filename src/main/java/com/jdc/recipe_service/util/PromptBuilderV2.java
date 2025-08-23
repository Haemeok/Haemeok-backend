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
public class PromptBuilderV2 {

    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final SurveyService surveyService;

    public PromptBuilderV2(UnitService unitService, IngredientRepository ingredientRepo, SurveyService surveyService) {
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
        String knownList = known.isEmpty() ? "없음" : String.join(", ", known);


        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가야.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> persona = "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사야.";
        }

        String userRequestXml = String.format("""
                <user_request>
                  <dishType>%s</dishType>
                  <mainIngredients>%s</mainIngredients>
                  <cookingTime>%s</cookingTime>
                  <servings>%s</servings>
                  <spiceLevel>%s/5</spiceLevel>
                  <allergyInfo>%s</allergyInfo>
                  <themes>%s</themes>
                  <knownIngredients>%s</knownIngredients>
                </user_request>""",
                request.getDishType(),
                String.join(", ", request.getIngredients()),
                (request.getCookingTime() != null && request.getCookingTime() > 0) ? request.getCookingTime() + "분 이내" : "AI가 적절히 판단",
                (request.getServings() != null && request.getServings() > 0) ? request.getServings() + "인분" : "AI가 적절히 판단",
                spicePref != null ? spicePref : "기본",
                allergyPref != null && !allergyPref.isBlank() ? allergyPref : "없음",
                themePrefs.isEmpty() ? "없음" : String.join(", ", themePrefs),
                knownList
        );

        String fewShotExampleXml = """
                <example>
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
                </example>
                """;

        String rulesXml = String.format("""
                <rules>
                  **[최상위 규칙]**
                  1.  당신은 <user_request>를 분석하고, 아래의 모든 규칙을 준수하여 **단 하나의 JSON 객체만**을 생성해야 합니다.
                  2.  JSON 외에 어떤 설명, 주석, 마커(예: ```json)도 절대 포함해서는 안 됩니다.

                  **[콘텐츠 생성 규칙]**
                  1.  **요리 원리 준수**: 찌개·볶음 등에서는 기름에 주재료나 향신채를 먼저 볶아 풍미의 기초를 다지는 과정을 최우선으로 고려하세요.
                  2.  **재료 추가**: 요청에 없더라도 맛을 내기 위해 필수적인 보조 재료(기름, 맛술, 설탕 등)는 자유롭게 추가하고 'ingredients' 목록에 포함하세요.
                  3.  **인분 수 비례 조정**: 'ingredients'의 `quantity`는 <example>의 양을 기준으로 사용자의 요청 인분 수에 비례하여 조정하세요. (예: 요청이 4인분이면 예시의 2배)
                  4.  **알레르기 정보 반영**: 사용자의 알레르기 유발 재료는 반드시 제외하거나 안전한 재료로 대체하세요.
                  5.  **단계별 설명**: 각 단계는 핵심 행동 위주로 간결하고 명확하게 작성하고, 불 세기, 순서, 시간 등 구체적인 지시를 포함하세요.

                  **[JSON 필드 규칙]**
                  1.  `dishType`: <user_request>의 `dishType` 값('%s')을 그대로 사용해야 합니다.
                  2.  `tagNames`: <user_request>의 `themes`가 비어있지 않다면, 그 값을 순서대로 사용하세요. 비어있다면, 음식과 어울리는 태그를 아래 목록에서 최대 3개 선택하세요.
                      - 허용 태그 목록: 🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥, 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절, 🍱 도시락, 🔌 에어프라이어, 🍲 해장
                  3.  `unit`: 재료의 단위는 반드시 아래 목록 중 하나여야 합니다. [%s]
                  4.  `action`: `steps`의 `action`은 반드시 아래 목록 중 하나여야 합니다.
                      - 허용 동사 목록: 썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
                  5.  `customPrice`, `caloriesPerUnit`: <user_request>의 `knownIngredients` 목록에 없는 재료에 대해서만 이 두 필드를 추정하여 포함하세요. DB에 이미 있는 재료에는 절대 포함하면 안 됩니다.
                  6.  모든 필드는 의미 있는 한글 값이어야 하며, 빈 값("")이나 null이 될 수 없습니다.
                </rules>
                """, request.getDishType(), unitService.unitsAsString());

        return String.format("""
                        %s
                        
                        당신은 지금부터 아래의 지시사항에 따라 사용자 요청에 맞는 레시피 JSON을 생성해야 합니다.
                        
                        %s
                        
                        %s
                        
                        %s
                        
                        위 규칙과 예시를 참고하여 <user_request>에 대한 레시피 JSON을 생성하세요.
                        """,
                persona,
                rulesXml,
                userRequestXml,
                fewShotExampleXml
        );
    }
}