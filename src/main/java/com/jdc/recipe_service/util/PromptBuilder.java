package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;

public class PromptBuilder {

    public static String buildPrompt(AiRecipeRequestDto request, RobotType type) {
        String fewShotExample = """
            {
              "title": "돼지고기 김치찌개",
              "dishType": "국/찌개/탕",
              "description": "얼큰하고 시원한 김치찌개입니다.",
              "cookingTime": 30,
              "cookingTools": ["냄비", "도마", "칼"],
              "servings": 2.0,
              "ingredients": [
                 { "name": "돼지고기",  "quantity": "150", "unit": "g" },
                 { "name": "신김치",   "quantity": "200", "unit": "g" },
                 { "name": "두부",     "quantity": "0.5", "unit": "모" },
                 { "name": "대파",     "quantity": "0.5", "unit": "대" },
                 { "name": "양파",     "quantity": "0.25", "unit": "개" },
                 { "name": "청양고추",  "quantity": "1",    "unit": "개" },
                 { "name": "고춧가루",  "quantity": "1",    "unit": "큰술" },
                 { "name": "다진 마늘", "quantity": "0.5",  "unit": "큰술" },
                 { "name": "국간장",    "quantity": "0.5",  "unit": "큰술" },
                 { "name": "멸치육수",   "quantity": "500",  "unit": "ml" }
              ],
              "steps": [
                {
                  "stepNumber": 0,
                  "instruction": "돼지고기 목살 150g은 한입 크기로 깍둑썰기한다.",
                  "action": "손질하기"
                },
                {
                  "stepNumber": 1,
                  "instruction": "냄비에 물 500ml를 붓고 강불에서 끓이다가, 돼지고기와 신김치를 넣고 중불로 줄여 10분간 끓인다.",
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

        String persona = switch (type) {
            case CREATIVE -> "너는 창의적인 한국 요리 전문가야.";
            case HEALTHY   -> "너는 건강식을 잘 만드는 전문가야.";
            case INDULGENT -> "너는 자극적인 요리를 추구하는 전문가야.";
            default        -> "너는 꼼꼼하고 정확한 한국 요리 전문가야.";
        };

        return String.format("""
            %s
            **오직 단 하나의 JSON 객체 형태로만 출력하세요. 다른 JSON, 다른 텍스트, 추가 출력은 일절 허용되지 않습니다. {…} 블럭을 한 번 닫으면 절대로 이어서 다른 JSON을 출력하지 마세요.**

            **아래 규칙을 절대 어기지 말고, 출력은 반드시 “{”로 시작해서 “}”로 끝나는 단 하나의 JSON 객체만 내보내세요.**

            1) 요청한 "dishType"(%s)을 **절대로 수정·누락하지 말 것**.
            2) 요청한 "tagNames" 배열 %s의 순서를 **절대로 수정·누락하지 말 것**.
               - 만약 %s가 `[]`라면, AI는 **아래 허용 목록** 중 음식 분위기에 맞는 태그를 최대 3개 골라서 반환해야 합니다.
               - 허용 목록 (최대 3개 선택):
                 🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥,
                 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절,
                 🍱 도시락, 🔌 에어프라이어, 🍲 해장
            3) 각 단계의 "steps[].action" 필드는 요청에서 준 단어 그대로 사용하고, **다른 단어로 바꾸면 안 됩니다**.
            4) “title”, “dishType”, “description” 등 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값("")이 되어서는 안 됩니다.
            5) 채소(감자·당근·양파·호박 등)는 “개”로, 두부는 “모”로, 대파는 “단”으로 표시하세요.
            6) 육류(돼지고기·소고기 등)는 “g”로, 양념류(고춧가루·설탕·소금 등)는 “작은술”·“큰술”로 표시하세요.
            7) "steps" 배열 안의 각 객체는 반드시
               - "stepNumber" (0부터 순서대로 증가)
               - "instruction" (한 문장 이상의 구체적 조리 방법)
               - "action" (요약 키워드)
               세 가지 키를 모두 포함해야 합니다.
               예: `"instruction": "볼에 손질한 상추 1개, 오이 0.5개, 방울토마토 10g을 넣고 올리브유 10ml, 소금 0.1큰술, 후추 0.1작은술, 발사믹 식초 1작은술을 넣어 30초간 골고루 버무린다."`
            8) JSON 외에 어떤 텍스트(설명·주석·마커 등)도 절대로 포함하지 마세요.
            9) **"action" 필드는 반드시 아래 19개 중 하나만 사용해야 해:**
             ```
             썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기,
             구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기,
             로스팅, 캐러멜라이즈, 부치기
             ```
             따옴표 없이, 정확히 저 문자열 중 하나로만 기입)
            --- 예시 JSON (구조 참고용) ---
            %s
            --- 예시 끝 ---

            요청 조건:
            - 요리 유형: %s
            - 희망 조리 시간: %d분 이내
            - 주요 재료: %s
            - 태그: %s

            출력 형식: JSON 객체 하나만
            """,
                persona,
                request.getDishType(),
                tags,
                tags,
                fewShotExample,
                request.getDishType(),
                request.getCookingTime(),
                ingredients,
                tags
        );
    }
}
