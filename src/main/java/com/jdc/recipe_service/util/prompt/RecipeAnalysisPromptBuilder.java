package com.jdc.recipe_service.util.prompt;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class RecipeAnalysisPromptBuilder {

    public String buildAnalysisPrompt(Recipe recipe) {
        String ingredientsStr = (recipe.getIngredients() != null)
                ? recipe.getIngredients().stream()
                .map(ri -> {
                    String name = (ri.getIngredient() != null) ? ri.getIngredient().getName() : ri.getCustomName();
                    String quantity = ri.getQuantity() != null ? ri.getQuantity() : "";
                    String unit = ri.getUnit() != null ? ri.getUnit() : "";

                    return String.format("%s(%s %s)", name, quantity, unit).trim().replace("  ", " ");
                })
                .collect(Collectors.joining(", "))
                : "재료 정보 없음";

        String stepsStr = (recipe.getSteps() != null)
                ? recipe.getSteps().stream()
                .sorted(Comparator.comparing(RecipeStep::getStepNumber))
                .map(step -> step.getStepNumber() + ". " + step.getInstruction())
                .collect(Collectors.joining("\n"))
                : "조리 과정 정보 없음";

        Set<String> tools = recipe.getCookingTools();
        String toolsStr = (tools != null && !tools.isEmpty()) ? String.join(", ", tools) : "일반 조리 도구";

        String description = recipe.getDescription() != null ? recipe.getDescription() : "설명 없음";
        String servings = recipe.getServings() != null ? String.valueOf(recipe.getServings()) : "1";
        String dishTypeStr = recipe.getDishType() != null ? recipe.getDishType().name() : "기타";
        int totalCost = recipe.getTotalIngredientCost() != null ? recipe.getTotalIngredientCost() : 0;

        return """
                [ROLE]
                당신은 **[수석 콘텐츠 안전 관리자]**이자 **[한국 배달 플랫폼 수익성 분석 전문가]**입니다.
                당신의 임무는 사용자가 업로드한 레시피의 유해성을 판단하고, 복잡한 배달 수수료 구조를 고려하여 **점주가 손해 보지 않는 최적의 판매가**를 산출하는 것입니다.
                        
                [INPUT DATA: 분석 대상 레시피]
                - 요리명: %s
                - 설명: %s
                - 종류: %s
                - 인분: %s인분
                - 조리 도구: %s
                - **식재료 원가(COGS): %d원**
                        
                [INGREDIENTS]
                %s
                        
                [STEPS]
                %s
                        
                ---------------------------------------------------

                [SAFETY POLICY: 유해 콘텐츠 판단 기준]
                다음 카테고리에 해당하면 즉시 `isAbusive: true`로 판정하고, 해당 `abuseType`을 반환하십시오.
                
                1. SAFETY_HAZARD (안전 위험):
                   - 식용 불가능한 재료(세제, 락스, 붕산, 표백제 등) 사용.
                   - 신체적 상해 위험 조리법 (밀폐 용기 가열, 폭발/화재 위험).
                   - 치명적 식중독 유발 지침 (생 닭고기/돼지고기 섭취 권장 등).
                   - 섭식 장애(거식증 등) 조장 및 극단적 단식 유도.
                             
                2. HATE_HARASSMENT (혐오 및 괴롭힘):
                   - 욕설, 비속어, 인종/성별/종교/지역 비하 발언.
                   - 특정인 저격 및 사이버 불링.
               
                3. SPAM_AD (스팸 및 광고):
                   - 도박, 성인 사이트 등 외부 링크.
                   - 상업적 홍보 문구 반복, 봇(Bot)이 작성한 무의미한 텍스트(ㄹㄹㄹ 등).
                                
                4. OFF_TOPIC (주제 이탈):
                   - 요리와 무관한 정치 선전, 개인 일기, 장난성 글.

                [GUIDELINES: 분석 지침]
                - **맥락 파악(Context Awareness):** '마약 김밥', '폭탄 세일', '미친 매운맛' 처럼 한국의 관용적 표현은 마약/폭력으로 분류하지 마십시오. (SAFE 판정)
                - **숨겨진 위험 탐지:** 텍스트가 정상적으로 보여도, 화학적으로 위험한 조합(예: 락스+세제 혼합)이 있다면 즉시 차단하십시오.
                - **비속어 허용 기준 (Slang Tolerance):** - **맛이나 감정을 강조하기 위한 가벼운 비속어**는 허용하십시오.
                - (허용 예시): "존나 맛있다", "개꿀맛", "미친 맛", "개쩐다" -> **false (SAFE)**

                ---------------------------------------------------   
                                     
                [OUTPUT RULES: JSON 출력 규칙]
                오직 아래 JSON 형식으로만 응답하십시오. (설명 금지)
                        
                1. "marketPrice" (Integer):
                   - 한국 배달 시장(배민1플러스 등)의 고비용 구조를 반영하여 **'역마진 방지 공식'**으로 가격을 계산하십시오.       
                              
                   **[가격 산정 공식 (Reverse Margin Formula)]**
                   $$ P = (C_{food} + C_{pack} + F_{del} + M) / (1 - R_{var}) $$

                   * **변수 값:**
                     - $C_{food}$ (식재료비): **%d원** (위에서 주어진 값)
                     - $C_{pack}$ (포장비): **1,000원** (기본값)
                     - $F_{del}$ (점주 부담 배달비): **3,200원** (평균값)
                     - $R_{var}$ (플랫폼 수수료+결제수수료+VAT): **0.1408 (14.08%%)**
                     - $M$ (목표 순이익): **최소 2,500원** 또는 **식재료비의 30%%** 중 더 큰 금액으로 설정.
                     
                   * **계산 지침:** 
                     - 위 공식을 적용하여 계산된 금액을 **100원 단위로 반올림**하여 정수로 반환하십시오. 
                     - **[중요]** 계산된 가격은 반드시 **원가(%d원)보다 높아야 합니다.**
                     
                2. "cookingTips" (String):
                   - 현재 사용 가능한 조리 도구(%s)나 재료 상황을 고려하여 팁을 작성해.
                   - 요리의 맛을 한 층 더 올릴 수 있는 비법(마이야르, 불조절 등)이나 재료 대체 팁을 **2~3문장의 줄글**로 작성해.
                   - **[중요] 재료를 대체할 것을 추천한다면, 반드시 구체적인 양을 명시해야 해.**
                     (나쁜 예: "우유 대신 두유를 쓰세요.")
                     (좋은 예: "우유가 없다면 두유 100ml로 대체하여 더 고소한 맛을 낼 수 있습니다.")
                   - 문체는 친절하고 전문적인 '사장님 팁' 말투로 작성해 (~해요, ~합니다).
                   
                3. "isAbusive" (Boolean) 및 "abuseType" (String):
                   - 위 **[SAFETY POLICY]**와 **[GUIDELINES]**를 종합적으로 고려하여 위반 여부를 판단하십시오.
                   - 위반 사항이 없다면 "isAbusive": false, "abuseType": "SAFE" 입니다.
                   - 위반 사항이 있다면 "isAbusive": true 이며, "abuseType"은 해당 위반 카테고리 코드(예: SAFETY_HAZARD)를 반환하십시오.
                                  
                        
                [JSON 출력 예시]
                {
                  "marketPrice": 14500,
                  "cookingTips": "마지막에 트러플 오일을 두 방울 떨어뜨리면 풍미가 살아납니다. 생크림이 없다면 우유 150ml와 버터 10g을 섞어 사용해도 좋습니다.",
                  "isAbusive": false,
                  "abuseType": "SAFE"
                }
                """.formatted(
                recipe.getTitle(),
                description,
                dishTypeStr,
                servings,
                toolsStr,
                totalCost,
                ingredientsStr,
                stepsStr,
                totalCost,
                totalCost,
                toolsStr
        );
    }
}
