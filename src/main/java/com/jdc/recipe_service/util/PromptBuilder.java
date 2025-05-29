package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.type.RobotType;

public class PromptBuilder {

    public static String buildPrompt(AiRecipeRequestDto request, RobotType type) {
        // Few‐shot 예시 JSON
        String fewShotExample = """
            {
              "title": "돼지고기 김치찌개",
              "dishType": "국/찌개/탕",
              "description": "잘 익은 김치와 돼지고기를 사용하여 만드는 모두가 사랑하는 한국인의 대표적인 찌개입니다. 얼큰하면서도 깊은 맛이 일품이며, 밥도둑이 따로 없습니다.",
              "cookingTime": 30,
              "cookingTools": ["냄비", "칼", "도마", "국자", "계량스푼"],
              "servings": 2.0,
              "ingredients": [
                {"name": "돼지고기 목살", "quantity": "150", "unit": "g"},
                {"name": "신김치", "quantity": "200", "unit": "g"},
                {"name": "두부", "quantity": "0.5", "unit": "모"},
                {"name": "대파", "quantity": "0.5", "unit": "대"},
                {"name": "양파", "quantity": "0.25", "unit": "개"},
                {"name": "청양고추", "quantity": "1", "unit": "개"},
                {"name": "고춧가루", "quantity": "1", "unit": "큰술"},
                {"name": "다진 마늘", "quantity": "0.5", "unit": "큰술"},
                {"name": "국간장", "quantity": "0.5", "unit": "큰술"},
                {"name": "멸치 다시마 육수", "quantity": "500", "unit": "ml"}
              ],
              "steps": [
                {"stepNumber": 0, "instruction": "돼지고기와 김치를 먹기 좋은 크기로 썰고, 양파와 대파, 고추를 손질합니다.", "action": "손질하기"},
                {"stepNumber": 1, "instruction": "냄비에 돼지고기를 넣고 중불에 달달 볶다가, 김치를 넣어 함께 볶아 고춧가루를 뿌립니다.", "action": "볶기"},
                {"stepNumber": 2, "instruction": "멸치 다시마 육수를 붓고 다진 마늘, 국간장을 넣어 끓이다가 중약불로 줄여 15분간 끓입니다.", "action": "끓이기"},
                {"stepNumber": 3, "instruction": "두부와 양파를 넣고 5분 더 끓인 뒤, 대파와 고추를 넣어 한소끔 더 끓입니다.", "action": "끓이기"}
              ],
              "tagNames": ["얼큰한", "밥도둑", "한국인 최애", "김치요리"]
            }
            """;

        // User‐provided params
        String ingredients = (request.getIngredients() == null || request.getIngredients().isEmpty())
                ? "없음"
                : String.join(", ", request.getIngredients());
        String tags = (request.getTagNames() == null || request.getTagNames().isEmpty())
                ? "없음"
                : String.join(", ", request.getTagNames());
        String persona = switch(type) {
            case CREATIVE -> "너는 창의력을 최우선으로 하는 한국 요리 전문가(창의적요리사)야.";
            case HEALTHY   -> "너는 건강과 다이어트를 중시하는 한국 요리 전문가(건강요리사)야.";
            case INDULGENT -> "너는 치팅데이를 위한 자극적이고 맛있는 요리를 추구하는 전문가(자극적요리사)야.";
            default        -> "너는 매우 꼼꼼하고 정확한 한국 요리 전문가(정석요리사)야.";
        };

        // 최종 프롬프트
        return String.format("""
            %s
            너는 지금부터 **매우 꼼꼼하고 경험이 풍부한 한국 요리 전문가**로서, 다음 지침과 예시를 **반드시** 준수하여 요청 조건에 맞는 레시피를 JSON 형식으로 제공해야 한다.

            **[가장 중요한 지침]**
            1.  최종 응답은 **오직 단 하나의 완결된 JSON 객체**여야 한다. 다른 어떤 텍스트, 설명, 주석, 마크다운 코드 블록도 포함해서는 안 된다.
            2.  생성될 JSON 객체는 최상위에 다음 키들을 **반드시 모두 포함**해야 한다: "title", "dishType", "description", "cookingTime", "cookingTools", "servings", "ingredients", "steps", "tagNames".
            3.  **"title" (요리 제목) 필드는 절대로 생략해서는 안 되며, 의미 있는 한글 요리 이름이어야 한다.** 이 지침을 어기면 안 된다.

            다음은 네가 따라야 할 매우 우수한 품질의 레시피 JSON 예시이다. 이 예시의 전체적인 형식, 필드 구성, 상세함의 수준을 주의 깊게 학습하고 그대로 재현하라.
            --- 레시피 JSON 예시 시작 ---
            %s
            --- 레시피 JSON 예시 끝 ---
            
            이제, 다음 "요청 조건"과 위의 모든 지침, 그리고 "레시피 JSON 예시"를 종합적으로 고려하여, 최고의 레시피를 JSON 형식으로 생성하라.

            요청 조건:
            - 요리 유형: %s
            - 희망 조리 시간: %d분 이내
            - 주요 재료: %s
            - 관련 태그: %s
            
            **[JSON 상세 출력 지침]**
            1.  (가장 중요한 지침 1번과 동일) 출력은 반드시 요청된 모든 정보를 포함하는 단일 JSON 객체여야 하며, 다른 설명이나 코드블럭 없이 JSON 데이터만 반환해야 해.
            2.  (가장 중요한 지침 2번과 동일) JSON 객체에는 다음 키들이 **반드시 포함**되어야 해: "title", "dishType", "description", "cookingTime", "cookingTools", "servings", "ingredients", "steps", "tagNames".
            3.  (가장 중요한 지침 3번과 동일) **"title" (요리 제목)과 "dishType" (요리 유형) 필드는 의미 있는 한글 내용을 포함해야 하며, 절대 빈 문자열("")이 아니어야 해.**
            4.  "cookingTime"은 실제 예상 조리 시간을 나타내는 숫자여야 하고, "servings"는 인분 수를 나타내는 숫자(예: 2 또는 2.0, 예시 참고)여야 해. "cookingTools"는 주요 요리 도구들의 문자열 배열이어야 해 (예시의 ["냄비", "칼", "도마", "국자", "계량스푼"]처럼 최소 2-3개 이상 실제 필요한 도구를 나열해줘). "tagNames"는 태그 이름의 문자열 배열이어야 해.
            5.  "ingredients" 배열의 각 재료 객체는 반드시 "name"(재료명 문자열), "quantity"(수량 문자열), "unit"(단위 문자열) 세 가지 키를 모두 가져야 해. (모델이 학습한 DB 기준으로 정확한 단위를 사용하되, 제공된 예시의 단위 표현 방식을 참고해줘. 특히 "대파"와 같은 채소는 "단"과 같이 너무 큰 단위 대신 "대", "줄기", "뿌리" 등 더 적절하고 현실적인 단위를 사용하고, 그에 맞는 수량을 제시해야 해.)
            6.  "quantity" 필드에는 해당 재료에 필요한 구체적인 양을 반드시 기입해야 하며, 절대 빈 문자열("")이 되어서는 안 돼. 예를 들어, "1", "0.5", "1/2", "200" 과 같이 명확한 값을 입력해줘. (모델이 인분 수를 스스로 판단한다면, 그 판단된 인분 수에 맞는 양을 기입하되, 특히 "%s" 같은 국물 요리에서는 국물이 너무 적지 않도록 물이나 육수의 양을 위 예시(예: 2인분 기준 약 400-500ml)처럼 현실적으로 조절해줘.)
            7.  "steps" 배열의 각 단계 객체는 반드시 "stepNumber"(단계 번호, 0부터 순서대로 증가하는 숫자), "instruction"(단계별 상세 설명 문자열), "action"(요약 행동 문자열, 예: "썰기", "볶기", "끓이기") 키를 가져야 해.
            8.  모든 정보는 주어진 "요청 조건"에 최대한 부합하게 작성하고, 전반적인 내용은 위에 제공된 "레시피 JSON 예시"의 스타일과 상세함 수준을 참고하여 생성해줘.

            생성 시작:
            {
            """,
                persona,
                fewShotExample,
                request.getDishType(),
                request.getCookingTime(),
                ingredients,
                tags,
                request.getDishType() // 이 마지막 인자는 String.format에서 %s에 해당하는데, 이전 코드에서 중복으로 보입니다. 필요한지 확인 필요.
                // 만약 6번 지침의 "%s"를 채우기 위함이었다면, 해당 위치에 request.getDishType()을 직접 넣는 것이 아니라,
                // String.format의 인자 순서에 맞게 추가하거나, 해당 %s를 다른 값으로 대체해야 합니다.
                // 여기서는 일단 기존 구조를 유지하되, 이 부분을 점검해보시길 권합니다.
        );
    }
}