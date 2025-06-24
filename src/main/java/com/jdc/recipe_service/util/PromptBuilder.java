package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;
import org.springframework.stereotype.Component;

@Component
public class PromptBuilder {

    private final UnitService unitService;

    public PromptBuilder(UnitService unitService) {
        this.unitService = unitService;
    }

    public String buildPrompt(AiRecipeRequestDto request, RobotType type) {

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
                 { "name": "신김치",   "quantity": "200", "unit": "g" },
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
                {
                  "stepNumber": 0,
                  "instruction": "돼지고기는 한입 크기로, 김치는 2cm 폭으로 썰고, 양파는 채썰고 대파는 어슷썹니다. 두부는 1.5cm 두께로 준비합니다.",
                  "action": "손질하기"
                },
                {
                  "stepNumber": 1,
                  "instruction": "중불로 달군 냄비에 들기름 1큰술을 두르고 돼지고기를 넣어 겉면이 익을 때까지 볶습니다.",
                  "action": "볶기"
                },
                {
                  "stepNumber": 2,
                  "instruction": "김치를 넣고 3~5분간 충분히 볶아 신맛을 부드럽게 만들고 풍미를 끌어올립니다.",
                  "action": "볶기"
                },
                {
                  "stepNumber": 3,
                  "instruction": "멸치육수 500ml와 김치국물 0.5컵을 붓고, 고춧가루·다진마늘·설탕을 넣어 10분간 끓입니다.",
                  "action": "끓이기"
                },
                {
                  "stepNumber": 4,
                  "instruction": "양파와 두부를 넣고 5분 더 끓인 뒤, 마지막에 대파를 넣어 한소끔 더 끓여 마무리합니다.",
                  "action": "끓이기"
                }
              ],
              "tagNames": ["🍲 해장", "🍽️ 혼밥"]
            }
            """;

        String ingredients = String.join(", ", request.getIngredients());

        String tags;
        if (request.getTagNames() == null || request.getTagNames().isEmpty()) {
            tags = "[]";
        } else {
            tags = "\"" + String.join("\", \"", request.getTagNames()) + "\"";
            tags = "[" + tags + "]";
        }

        String cookingTimePart = (request.getCookingTime() != null && request.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", request.getCookingTime())
                : "- 희망 조리 시간 정보가 제공되지 않았습니다. AI 모델은 자동으로 예상 조리 시간을 추정하세요.";

        String servingsPart = (request.getServings() != null && request.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", request.getServings())
                : "- 인분 수 정보가 제공되지 않았습니다. AI 모델이 적절히 판단하여 작성하세요.";

        String preferencePart = String.format("""
            - 매운맛 선호도: %s%s
            - 짠맛 선호도: %s
            - 알레르기 정보: %s
            - 식이 제한: %s""",
                request.getSpiceLevel()    != null ? request.getSpiceLevel() : "기본",
                request.getSpiceLevel()    != null ? "/5"             : "",
                request.getSaltiness()     != null ? request.getSaltiness().name() : "기본",
                (request.getAllergy()      != null && !request.getAllergy().isBlank()) ? request.getAllergy() : "없음",
                (request.getDietType()     != null && !request.getDietType().isBlank()) ? request.getDietType() : "없음"
        );

        String persona = switch (type) {
            case CREATIVE  -> "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가야.";
            case HEALTHY   -> "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case INDULGENT -> "너는 사람들의 입맛을 확 사로잡는 자극적이고 화려한 요리를 추구하는 미식가야.";
            default        -> "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사야.";
        };

        String allowedUnits   = unitService.unitsAsString();
        String unitMapping    = unitService.mappingAsString();

        return String.format("""
            %s
            **오직 단 하나의 JSON 객체 형태로만 출력하세요. 다른 텍스트나 설명은 일절 허용되지 않습니다.**

            **아래 규칙을 반드시 준수하여, 맛의 깊이와 요리 원리를 고려한 최상의 레시피를 생성하세요.**

            **[요리 원리 규칙]**
            1. **(핵심)** 찌개·볶음·조림 요리에서는 기름에 주재료나 향신채(마늘·파 등)를 먼저 볶아 풍미의 기초를 다지는 과정을 최우선으로 고려하세요.
            2. 효율적이고 논리적인 순서로 단계를 구성하세요. (예: 모든 재료 손질 후 조리 시작)
            3. 요청에 없더라도 필수 보조 재료(기름·맛술·설탕 등)를 자유롭게 추가하고 'ingredients'에 포함시키세요.
            4. **예시 JSON은 2인분 기준이며, 각 재료의 quantity는 “예시 양 × (요청 인분 수 ÷ 2)” 공식을 적용해 비례 조정할 것.**
            5. **알레르기 및 식이 제한(예: 견과류 알레르기 시 견과류 완전 배제, 락토-오보 식단 시 버터 대신 들기름 사용) 에 맞춰 부적합 재료는 반드시 제외하거나 대체 재료로 변경하세요.**
            
            **[출력 형식 규칙]**
            1) 요청한 "dishType"(%s)을 절대로 수정·누락하지 말 것.
            2) 요청한 "tagNames" 배열 %s의 순서를 절대로 수정·누락하지 말 것.
               - 만약 %s가 `[]`라면, AI는 아래 허용 목록 중 음식 분위기에 맞는 태그를 최대 3개 골라서 반환해야 합니다.
               - 허용 목록 (최대 3개 선택):
                 🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥,
                 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절,
                 🍱 도시락, 🔌 에어프라이어, 🍲 해장
            3) "steps" 배열의 "action" 필드는 반드시 아래 19개 중 하나만 사용해야 합니다:
               썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기,
               구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기,
               로스팅, 캐러멜라이즈, 부치기
            4) 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값("")이 될 수 없습니다.
            5) "steps" 배열 안의 각 객체는 "stepNumber", "instruction", "action" 키를 모두 포함해야 합니다.
            6) JSON 외에 어떤 텍스트(설명·주석·마커 등)도 절대로 포함하지 마세요.
            7) "unit" 필드는 다음 허용 단위 중 하나만 사용해야 합니다: [%s]
            8) 재료별 기본 단위 매핑: {%s}

            --- 예시 JSON (이 구조와 요리 원리를 참고하여 생성) ---
            %s
            --- 예시 끝 ---

            요청 조건:
            - 요리 유형: %s
            %s
            %s
            %s
            - 주요 재료: %s
            - 태그: %s

            출력 형식: JSON 객체 하나만
            """,
                persona,
                request.getDishType(), tags, tags,
                allowedUnits,
                unitMapping,
                fewShotExample,
                request.getDishType(),
                cookingTimePart,
                servingsPart,
                preferencePart,
                ingredients,
                tags
        );
    }
}