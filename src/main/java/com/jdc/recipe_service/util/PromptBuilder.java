package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;

public class PromptBuilder {
    public static String buildPrompt(AiRecipeRequestDto request) {
        String ingredientsString = String.join(", ", request.getIngredients());
        String tagsString = String.join(", ", request.getTagNames());

        // AI에게 보여줄 좋은 품질의 레시피 JSON 예시 (김치찌개)
        // 이 예시에는 현실적인 재료의 양 (특히 국물의 양과 대파의 단위/양)이 포함되어 있습니다.
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
                {"stepNumber": 1, "instruction": "돼지고기는 먹기 좋은 크기로 썰고, 신김치도 적당한 크기로 썹니다. 양파는 채 썰고, 대파와 청양고추는 어슷하게 썹니다. 두부는 큼직하게 썹니다.", "action": "손질/썰기"},
                {"stepNumber": 2, "instruction": "냄비에 돼지고기와 김치를 넣고 중간 불에서 달달 볶다가, 고춧가루를 넣어 함께 볶아 풍미를 더합니다.", "action": "볶기"},
                {"stepNumber": 3, "instruction": "멸치 다시마 육수를 붓고 다진 마늘, 국간장을 넣어 간을 맞춘 후, 센 불에서 끓이기 시작합니다. 끓어오르면 중약불로 줄여 15분 정도 충분히 끓여 맛을 냅니다.", "action": "끓이기"},
                {"stepNumber": 4, "instruction": "채 썬 양파와 두부를 넣고 5분 정도 더 끓입니다.", "action": "끓이기"},
                {"stepNumber": 5, "instruction": "마지막으로 대파와 청양고추를 넣고 한소끔 더 끓여내면 완성입니다. 필요하다면 소금으로 최종 간을 맞춥니다.", "action": "완성하기"}
              ],
              "tagNames": ["얼큰한", "밥도둑", "한국인 최애", "김치요리"]
            }
            """;

        return String.format("""
                        너는 지금부터 **매우 꼼꼼하고 경험이 풍부한 한국 요리 전문가**야. 너의 목표는 사용자가 제공하는 조건에 맞춰, 누구나 쉽게 따라 할 수 있으면서도 맛이 보장되는, 아주 상세하고 정확한 레시피를 JSON 형식으로 제공하는 것이야. 특히 재료의 양과 단위, 조리 시간, 단계별 설명은 실제 요리에 바로 사용할 수 있을 만큼 현실적이고 정확해야 해. "영상에서는" 같은 불필요한 정보는 절대 포함하지 마.

                        다음은 사용자가 매우 만족한 고품질 레시피 JSON의 한 예시입니다. 이 예시의 전체적인 형식, 상세함의 수준, 특히 "ingredients" 항목 내 각 재료의 "quantity"와 "unit"이 어떻게 현실적으로 작성되었는지 (예: "대파"는 "0.5", "대" / "멸치 다시마 육수"는 "500", "ml"), 그리고 "steps"의 구체성과 "cookingTools"의 구성을 주의 깊게 참고하여, 아래 "요청 조건"에 맞는 새로운 레시피를 생성해주세요.
                        --- 레시피 JSON 예시 시작 ---
                        %s
                        --- 레시피 JSON 예시 끝 ---

                        이제, 다음 "요청 조건"에 따라, 위 예시와 너의 전문 지식을 총동원하여 최고의 레시피를 JSON 형식으로 생성해줘.

                        요청 조건:
                        - 요리 유형: %s
                        - 희망 조리 시간: %d분 이내
                        - 주요 재료: %s
                        - 관련 태그: %s

                        JSON 출력 지침:
                        1. 출력은 반드시 요청된 모든 정보를 포함하는 단일 JSON 객체여야 하며, 다른 설명이나 코드블럭 없이 JSON 데이터만 반환해야 해.
                        2. JSON 객체에는 다음 키들이 **반드시 포함**되어야 해: "title", "dishType", "description", "cookingTime", "cookingTools", "servings", "ingredients", "steps", "tagNames".
                        3. **"title" (요리 제목)과 "dishType" (요리 유형) 필드는 의미 있는 한글 내용을 포함해야 하며, 절대 빈 문자열("")이 아니어야 해.**
                        4. "cookingTime"은 실제 예상 조리 시간을 나타내는 숫자여야 하고, "servings"는 인분 수를 나타내는 숫자(예: 2 또는 2.0, 예시 참고)여야 해. **"cookingTools"는 주요 요리 도구들의 문자열 배열이어야 해 (예시의 ["냄비", "칼", "도마", "국자", "계량스푼"]처럼 최소 2-3개 이상 실제 필요한 도구를 나열해줘).** "tagNames"는 태그 이름의 문자열 배열이어야 해.
                        5. "ingredients" 배열의 각 재료 객체는 반드시 "name"(재료명 문자열), "quantity"(수량 문자열), "unit"(단위 문자열) 세 가지 키를 모두 가져야 해. (모델이 학습한 DB 기준으로 정확한 단위를 사용하되, 제공된 예시의 단위 표현 방식을 참고해줘. **특히 "대파"와 같은 채소는 "단"과 같이 너무 큰 단위 대신 "대", "줄기", "뿌리" 등 더 적절하고 현실적인 단위를 사용하고, 그에 맞는 수량을 제시해야 해.**)
                        6. **"quantity" 필드에는 해당 재료에 필요한 구체적인 양을 반드시 기입해야 하며, 절대 빈 문자열("")이 되어서는 안 돼. 예를 들어, "1", "0.5", "1/2", "200" 과 같이 명확한 값을 입력해줘.** (모델이 인분 수를 스스로 판단한다면, 그 판단된 인분 수에 맞는 양을 기입하되, 특히 "%s" 같은 국물 요리에서는 국물이 너무 적지 않도록 물이나 육수의 양을 위 예시(예: 2인분 기준 약 400-500ml)처럼 현실적으로 조절해줘.)
                        7. **"steps" 배열의 각 단계 객체는 반드시 "stepNumber"(단계 번호, 0부터 순서대로 증가하는 숫자), "instruction"(단계별 상세 설명 문자열), "action"(요약 행동 문자열, 예: "썰기", "볶기", "끓이기") 키를 가져야 해.** (AI가 생성할 필요 없는 "imageKey", "stepImageKeys", 단계별 "ingredients" 필드는 "steps" 객체 내에 포함하지 않아도 괜찮아.)
                        8. 모든 정보는 주어진 "요청 조건"에 최대한 부합하게 작성하고, 전반적인 내용은 위에 제공된 "레시피 JSON 예시"의 스타일과 상세함 수준을 참고하여 생성해줘.

                        생성 시작:
                        """,
                fewShotExample, // 예시 JSON 전달
                request.getDishType(),
                request.getCookingTime(),
                ingredientsString,
                tagsString,
                request.getDishType() // JSON 출력 지침 6번의 %s 에 해당 (국물 요리 시 물 양 조절 힌트)
        );
    }
}