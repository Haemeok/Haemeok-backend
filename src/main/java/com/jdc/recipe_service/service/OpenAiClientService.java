package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.UnitService;
import com.openai.client.OpenAIClient;
import com.openai.models.ChatModel;
import com.openai.models.chat.completions.ChatCompletion;
import com.openai.models.chat.completions.ChatCompletionCreateParams;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class OpenAiClientService {

    private final OpenAIClient client;
    private final ObjectMapper objectMapper;
    private final UnitService unitService;

    private String systemPrompt;

    /**
     * 애플리케이션 시작 시, UnitService의 정보를 바탕으로
     * 완전한 형태의 System Prompt를 생성합니다.
     */
    @PostConstruct
    public void initializeSystemPrompt() {
        String allowedUnits = unitService.unitsAsString();
        String unitMapping = unitService.mappingAsString();

        this.systemPrompt = String.format("""
                너는 매우 정교한 한국 요리 레시피 생성기(JSON Generator)다.
                오직 하나의 완벽한 JSON 객체만 반환해야 하며, 절대로 다른 텍스트를 포함해서는 안 된다.
                
                [핵심 요리 원칙]
                1. (중요) 찌개, 볶음, 조림 요리 시, 기름에 향신채(마늘, 파 등)를 먼저 볶아 풍미를 극대화하는 과정을 우선적으로 고려한다.
                2. 사용자의 알레르기 및 식이 제한 요구사항을 철저히 준수하여 재료를 제외하거나 대체해야 한다.
                3. 요청에 없더라도 맛을 내기 위해 필수적인 보조 재료(기름, 맛술, 설탕 등)를 자유롭게 추가한다.
                
                [출력 JSON 형식]
                - `title`: String
                - `dishType`: String (사용자 요청 값 절대 변경 금지)
                - `description`: String
                - `cookingTime`: Integer (분 단위)
                - `cookingTools`: String[]
                - `servings`: Double
                - `ingredients`: Object[]
                  - `name`: String
                  - `quantity`: String
                  - `unit`: String (아래 '허용 단위' 목록에 있는 값만 사용)
                  - `customPrice`: Integer (DB에 없는 재료에만 **선택적으로** 포함)
                  - `caloriesPerUnit`: Integer (DB에 없는 재료에만 **선택적으로** 포함)
                - `steps`: Object[]
                  - `stepNumber`: Integer (0부터 시작)
                  - `instruction`: String
                     (조리 동작에 대한 구체적인 설명을 제공하세요.
                      예: 불 세기, 사용량, 시간 등을 포함해 상세히 기술)
                  - `action`: String (아래 '허용 Action' 목록에 있는 값만 사용)
                - `tagNames`: String[] (사용자 요청 값 우선, 없을 시 아래 '허용 태그' 목록에서 선택)
                
                [필드 규칙]
                1. `action` 허용 목록: 썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
                2. `tagNames`가 빈 배열(`[]`)로 요청된 경우, 아래 허용 목록에서 음식과 가장 어울리는 태그를 최대 3개까지 선택:
                   🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트/건강식, 👶 아이와 함께, 🍽️ 혼밥, 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드/간단 요리, 🎉 기념일/명절, 🍱 도시락, 🔌 에어프라이어, 🍲 해장
                3. `unit` 허용 목록: [%s]
                4. 재료별 기본 단위 매핑: {%s}
                   (중요) DB에 저장된 재료는 반드시 이 매핑을 따라야 합니다.
                5. (중요) `customPrice`(100g당 원), `caloriesPerUnit`(100g당 kcal) 필드는 **DB에 없는 재료**에 대해서만 추가하고, DB에 있는 재료에는 절대로 포함하지 않는다.
                
                [Few-Shot 예시]
                {
                  "title": "돼지고기 김치찌개", "dishType": "국/찌개/탕",
                  "description": "기름에 김치와 돼지고기를 충분히 볶아내어 깊고 진한 국물 맛이 일품인 정통 김치찌개입니다.",
                  "cookingTime": 30, "servings": 2.0, "cookingTools": ["냄비", "도마", "칼"],
                  "ingredients": [
                    { "name": "돼지고기", "quantity": "150", "unit": "g" },
                    { "name": "신김치", "quantity": "200", "unit": "g", "customPrice": 300, "caloriesPerUnit": 15 },
                    { "name": "두부", "quantity": "0.5", "unit": "모" }
                  ],
                  "steps": [
                    { "stepNumber": 0, "instruction": "돼지고기는 2cm 두께로 썰고, 김치는 3cm 길이로 한 입 크기로 잘라 준비합니다.", "action": "썰기" },
                    { "stepNumber": 1, "instruction": "중간 불로 달군 냄비에 식용유 1큰술을 두르고 돼지고기를 넣어 2분간 볶아 겉면이 노릇해지면, 김치를 넣고 3분간 더 볶아 감칠맛을 높입니다.", "action": "볶기" }
                  ],
                  "tagNames": ["🍲 해장", "🍽️ 혼밥"]
                }
                """, allowedUnits, unitMapping);
    }

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String userPrompt) {
        return CompletableFuture.supplyAsync(() -> {
            var params = ChatCompletionCreateParams.builder()
                    .model(ChatModel.GPT_4_TURBO)
                    .temperature(0.0)
                    .maxCompletionTokens(1500L)
                    .addSystemMessage(this.systemPrompt)
                    .addUserMessage(userPrompt)
                    .build();

            ChatCompletion completion = client.chat().completions().create(params);
            if (completion.choices().isEmpty()) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답이 비어 있습니다.");
            }
            String json = completion.choices().get(0).message().content()
                    .orElseThrow(() -> new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답 내용이 없습니다."));
            log.info("🔍 AI 생성 JSON ▶▶\n{}", json);
            try {
                return objectMapper.readValue(json, RecipeCreateRequestDto.class);
            } catch (Exception e) {
                throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI JSON 파싱 실패: " + e.getMessage(), e);
            }
        });
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 레시피 생성 실패: " + ex.getMessage(), ex));
    }
}