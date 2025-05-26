package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;

public class PromptBuilder {

    public static String buildPrompt(AiRecipeRequestDto request) {
        String ingredients = request.getIngredients() != null && !request.getIngredients().isEmpty()
                ? String.join(", ", request.getIngredients())
                : "없음";
        String tags = request.getTagNames() != null && !request.getTagNames().isEmpty()
                ? String.join(", ", request.getTagNames())
                : "없음";

        return String.format("""
                너는 지금부터 **매우 꼼꼼하고 정확한 한국 요리 전문가**야. 사용자가 제공하는 조건에 맞춰 현실적인 요리 레시피를 JSON 형식으로 생성해줘.

                📌 반드시 아래 조건만을 참고해서 JSON만 반환해야 해. 다른 문장이나 설명은 절대 출력하지 마.
                - 출력은 반드시 `{`로 시작해서 `}`로 끝나는 **하나의 JSON 객체**만 생성해야 함.
                - **절대 마크다운, 코드블럭, 해설, 설명을 추가하지 마.**
                - 오직 JSON 오브젝트 하나만 생성해. (예: `{ "title": ..., ... }`)

                요청 조건:
                - 요리 유형: %s
                - 조리 시간: %d분 이내
                - 주요 재료: %s
                - 관련 태그: %s

                JSON 스키마 (모든 키 필수):
                {
                  "title": String (요리 이름),
                  "dishType": String ("국/찌개/탕", "볶음", ...),
                  "description": String (요리 설명),
                  "cookingTime": Integer (예: 30),
                  "cookingTools": [String],
                  "servings": Number (예: 2.0),
                  "ingredients": [
                    {
                      "name": String,
                      "quantity": String (예: "1", "0.5", "200"),
                      "unit": String (예: "개", "큰술", "ml")
                    }
                  ],
                  "steps": [
                    {
                      "stepNumber": Integer (0부터 시작),
                      "instruction": String,
                      "action": String (예: "썰기", "볶기", "끓이기" 등 허용된 범위 내)
                    }
                  ],
                  "tagNames": [String] (예: ["🍽️ 혼밥", "⚡ 초스피드 / 간단 요리"])
                }

                생성 시작:
                {
                """,
                request.getDishType(),
                request.getCookingTime(),
                ingredients,
                tags
        );
    }
}
