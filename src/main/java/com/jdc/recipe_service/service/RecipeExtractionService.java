package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.media.YtDlpService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;

@Service
@Slf4j
public class RecipeExtractionService {

    private static final int MAX_CONTEXT_CHARS = 18_000;
    private static final int MAX_SCRIPT_CHARS  = 14_000;
    private static final int MAX_DESC_CHARS    = 4_000;
    private static final int MAX_CMT_CHARS     = 1_000;

    private final YtDlpService ytDlpService;
    private final GrokClientService grokClientService;
    private final GeminiMultimodalService geminiMultimodalService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;

    private final TransactionTemplate transactionTemplate;

    private static final Pattern UNIT_PATTERN = Pattern.compile(
            "(?i)(큰술|작은술|spoon|tbs|tsp|cup|\\b[0-9.]+\\s?g\\b|\\b[0-9.]+\\s?ml\\b|\\b[0-9.]+\\s?oz\\b|한\\s?꼬집|약간)"
    );
    private static final Pattern INGREDIENT_KEYWORD_PATTERN = Pattern.compile(
            "(?i)(재료|ingredient|준비물|필요한\\s?것)"
    );
    private static final Pattern STEP_ACTION_PATTERN = Pattern.compile(
            "(?i)(만드는|방법|recipe|step|direction|넣고|볶|끓|굽|튀기|섞|다지|채썰|chop|mix|boil|fry|bake|roast)"
    );

    public RecipeExtractionService(
            YtDlpService ytDlpService,
            GrokClientService grokClientService,
            GeminiMultimodalService geminiMultimodalService,
            RecipeService recipeService,
            DailyQuotaService dailyQuotaService,
            TransactionTemplate transactionTemplate
    ) {
        this.ytDlpService = ytDlpService;
        this.grokClientService = grokClientService;
        this.geminiMultimodalService = geminiMultimodalService;
        this.recipeService = recipeService;
        this.dailyQuotaService = dailyQuotaService;
        this.transactionTemplate = transactionTemplate;
    }

    private String getExtractionPrompt() {
        return """
            [SYSTEM]
            너는 요리 전문가 AI로서, 오직 하나의 완전한 JSON만 출력해야 합니다.
            설명, 주석, 마크다운, ```json 등 절대 포함하지 마세요.
            **반드시 아래 규칙을 100% 준수하여 레시피를 생성하세요.**
            
            [1단계: 레시피 영상 판별 (Critical Step)]
            제공된 텍스트/오디오가 '요리 조리법(Recipe)'을 설명하는지 판단하세요.
            단순 먹방(Mukbang), 맛집 탐방, 뉴스, 게임, 일상 브이로그 등 **직접 요리하는 과정과 재료 설명이 없는 영상**이라면,
            **아래의 '실패 JSON'만 출력하고 즉시 종료하세요.** 다른 필드는 절대 생성하지 마세요.
            
            --- 실패 시 출력 JSON ---
            {
              "isRecipe": false,
              "nonRecipeReason": "먹방/리뷰 영상입니다."
            }
            -----------------------
            
            [2단계: 레시피 추출 (성공 시)]
            레시피가 확실한 경우("isRecipe": true)에만, 아래 규칙을 100% 준수하여 상세 정보를 추출하세요.
            
            [근거 우선순위]
            - 자막(Script) > 영상 설명(Description) > 제목(Title) > 댓글(Comments) 순으로 신뢰합니다.
            - 댓글은 레시피 근거로 사용하지 말고, 자막/설명과 일치할 때만 보조로 참고하세요.
            - 링크/광고/쿠폰/구매유도/잡담은 레시피 추출 근거에서 제외하세요.
            
            **[보편적 조리 원리 (Universal Culinary Principles)]**
            1. **암묵적 재료(Implicit Ingredients) 포착:**
               - 자막이나 설명에 명시되지 않았더라도, 영상 속 조리 행위(예: 간 맞추기, 볶기, 농도 조절 등)를 수행하기 위해 **물리/화학적으로 투입된 모든 재료**를 시각적 단서로 유추하여 포함하세요.
               - 암묵적 재료/중간 단계 추가는 영상/자막/시각 단서로 '거의 확실'할 때만 허용합니다. 애매하면 절대 추가하지 말고, 기존 단계의 묘사를 더 구체화하세요.
            
            2. **상태 변화와 인과관계(Causality) 완결성:**
               - 조리 과정은 '원인(행동)'과 '결과(상태 변화)'의 연속입니다.
               - 영상 편집으로 인해 중간 과정이 생략되었더라도, **이전 단계의 재료 상태에서 다음 단계로 넘어가기 위해 논리적으로 반드시 수행되어야 하는 행동(중간 단계)**이 있다면, 이를 전문가적 지식으로 추론하여 채워 넣으세요.
               - 특정 요리법에 국한되지 말고, 모든 요리에 통용되는 물리적 법칙을 따르세요.
            
            3. **감각적 상태 묘사:**
               - 단순한 행동 동사(볶는다, 끓인다)만 나열하지 말고, **사용자가 행동을 멈추거나 다음 단계로 넘어가야 할 '타이밍'을 알 수 있도록** 재료의 시각적/청각적 상태 변화(색깔, 질감, 소리 등)를 구체적으로 묘사하세요.
            
            오직 단 하나의 JSON 객체 형태로만 출력하세요.
            
            [성공 시 JSON 출력 형식]
            {
              "isRecipe": true,
              "nonRecipeReason": null,
              "title": "샘플 요리 제목",
              "dishType": "볶음",
              "description": "이것은 JSON 구조를 보여주기 위한 샘플 레시피입니다. 내용을 복사하지 마세요.",
              "cookingTime": 20,
              "cookingTools": ["팬", "주걱"],
              "servings": 2,
              "ingredients": [
                { "name": "주재료A", "quantity": "100", "unit": "g" },
                { "name": "주재료B", "quantity": "1", "unit": "개" },
                { "name": "보조재료C", "quantity": "1", "unit": "작은술" }
              ],
              "steps": [
                { "stepNumber": 0, "instruction": "재료를 손질합니다.", "action": "손질하기", "timeline": "00:10" },
                { "stepNumber": 1, "instruction": "팬에 재료를 볶습니다.", "action": "볶기", "timeline": null }
              ],
              "tags": ["🍽️ 혼밥"],
              "marketPrice": 8000,
               "cookingTips": "팁1. 팁2. 팁3."
            }
            --- 예시 끝 ---
            
            **[JSON 출력 형식 규칙]**

            --- [🚨 CRITICAL WARNING: 숫자 필드 NULL/공백 절대 금지 🚨] ---
            - **모든 숫자 필드** `quantity`, `marketPrice`, `cookingTime`는 **0.00 이상의 유효한 숫자만** 허용됩니다.
            - **`servings`(인분)는 반드시 '정수(Integer)'로 반올림하여 출력하세요.** (예: 2.5 -> 3, 1.5 -> 2). 소수점을 포함하면 안 됩니다.
            - **절대로 빈 문자열("") 또는 null 값을 사용하지 마세요.** 이를 위반하면 JSON 전체가 무효화되고 에러가 발생합니다.
            
            **[JSON 세부 필드 규칙]**
            아래는 JSON 필드 각각의 세부 규칙입니다. 반드시 지켜주세요.

            --- "dishType" 필드 (요리 유형 규칙) ---
            - `dishType`은 반드시 아래 목록에서 하나만 선택하세요:
              "볶음", "국/찌개/탕", "구이", "무침/샐러드", "튀김/부침", "찜/조림", "오븐요리", "생식/회", "절임/피클류", "밥/면/파스타", "디저트/간식류"
            - **절대 빈 문자열("")이나 공백으로 출력되어서는 안 됩니다.**

            --- "description" 필드 ---
            - 유튜브 영상에서 말하는 톤처럼 레시피 소개문 1~2문장으로 작성
            - 맛/식감 포인트 1개 + 핵심 특징(간단/매콤/바삭 등) 1개는 반드시 포함
            -“후기/따라함/원레시피 고지”는 영상에 그런 맥락이 있을 때만 사용

            --- "ingredients" 필드 (재료 필드 강제 규칙 - 반드시 준수) ---
            - 영상에 언급되었거나, 영상/자막/시각 단서로 **거의 확실한 경우에만** 재료를 포함하세요.
            - 예: { "name": "양파", "quantity": "0.5", "unit": "개" }
            - 또한 모든 재료의 quantity는 요청된 인분 수에 맞추어 자동으로 조절해야 하며, 인분 수가 제공되지 않은 경우 모델이 적절한 기본 인분을 가정하여 일관성 있게 계산하세요.

            --- "steps" 필드 (단계 규칙) ---
            --**최대 개수** : step의 최대 개수는 8단계 ~ 10단계. 하나의 스텝에 적당히 2~3문장의 과정을 거치도록.
            - "steps" 배열의 "action" 필드는 반드시 아래 20개 중 하나만 사용해야 합니다:
              썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
            - 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값("")이 될 수 없습니다.
            - "steps" 배열 안의 각 객체는 "stepNumber", "instruction", "action" 키를 모두 포함해야 합니다.
            
            --- "timeline" 필드 (타임라인 규칙) ---
            - 각 조리 단계("steps")가 영상의 **몇 분 몇 초**에 시작하는지 찾아 "MM:SS" 형식으로 추출하세요. (예: "04:12")
            - 제공된 자막(Script) 텍스트에 `[04:12]`와 같은 시간 정보가 있다면 이를 우선적으로 매핑하세요.
            - 만약 자막이나 설명글에서 특정 단계의 시간을 정확히 찾을 수 없다면, 억지로 추측하지 말고 `null`로 두세요.
            - **중요:** timeline은 반드시 해당 action(동작)이 시작되는 시점이어야 합니다.
            
            --- "tags" 필드 (태그 규칙) ---
            - AI는 아래 허용 목록 중 음식 분위기에 맞는 태그를 **최대 3개** 골라서 반환해야 합니다:
              🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥, 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절, 🍱 도시락, 🔌 에어프라이어, 🍲 해장, 👨‍🍳 셰프 레시피

            [CRITICAL 태그 선택 조건] 섹션 내 전체 규칙
            - **Servings 기준:** Servings가 **1인분일 때만** '🍽️ 혼밥' 태그를 선택 가능합니다.
            - **시간 기준:** '⚡ 초스피드 / 간단 요리' 태그는 CookingTime이 **15분 이내**일 경우에만 선택 가능합니다.
            - **조리 방식 기준:** '🔌 에어프라이어' 태그는 레시피의 `cookingTools` 필드에 '오븐' 또는 '에어프라이어'가 명시적으로 포함되어 있거나, `dishType`이 **'구이'** 또는 **'튀김/부침'**에 해당될 경우에만 선택 가능합니다.
            - **건강 기준:** '🥗 다이어트 / 건강식' 태그는 **설탕, 튀김류, 가공육(햄/소시지)**이 주재료로 사용되지 않고, **채소나 단백질 위주**의 식단일 경우에만 선택 가능합니다.
            - **나머지 태그 (홈파티, 야식, 술안주 등):** 레시피의 분위기나 재료에 따라 AI가 자유롭게 판단하여 선택합니다.
            - **배제 규칙:** Servings가 2인분 초과일 경우 '🍽️ 혼밥' 태그를 절대 선택 불가. 지방/칼로리가 높거나 조리 시간이 20분 초과(오븐/찜 포함)일 경우 '⚡ 초스피드 / 간단 요리' 또는 '🥗 다이어트 / 건강식' 태그를 절대 선택 불가.

            --- "marketPrice" 필드 (배달앱 현실가 추정) ---
            - marketPrice는 한국 배달앱 기준 "메뉴 1개 주문 가격(원)" 정수입니다. (배달비/포장비/수수료/마진 포함)
            - 임의로 싸게 잡지 말고, 배달 전문점 메뉴판 수준으로 현실적으로 책정하세요.
            - 하한 규칙:
              - servings가 1이면 최소 9000원
              - servings가 2 이상이면 최소 (9000 * servings) 원
            - 조리 난이도/시간/재료가 풍부할수록 가격을 더 높게 책정하세요.
            - 100원 단위 올림으로 출력하세요.

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
            """;
    }

    @Async("recipeExtractionExecutor")
    public CompletableFuture<PresignedUrlResponse> extractAndCreateRecipe(String videoUrl, Long userId) {
        log.info("🚀 유튜브 레시피 추출 요청: URL={}", videoUrl);

        dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.YOUTUBE_EXTRACTION);

        String canonicalUrl = videoUrl;
        String title = "제목 미상";
        String description = "";
        String comments = "";
        String scriptPlain = "";

        boolean useUrlFallback = false;

        try {
            YtDlpService.YoutubeFullDataDto videoData = ytDlpService.getVideoDataFull(videoUrl);

            canonicalUrl = nullToEmpty(videoData.canonicalUrl());
            title = nullToEmpty(videoData.title());
            description = cap(nullToEmpty(videoData.description()), MAX_DESC_CHARS);
            comments = cap(nullToEmpty(videoData.comments()), MAX_CMT_CHARS);
            scriptPlain = cap(nullToEmpty(videoData.scriptTimecoded()), MAX_SCRIPT_CHARS);

        } catch (Exception e) {
            log.warn("⚠️ yt-dlp 추출 실패 (YouTube 차단/오류). Gemini 영상 분석으로 즉시 전환합니다. Error: {}", safeMsg(e));
            useUrlFallback = true;
        }

        try {
            String fullContext = cap(("""
                영상 URL: %s
                영상 제목: %s
                영상 설명: %s
                고정/인기 댓글: %s
                자막: %s
                """).formatted(
                    canonicalUrl,
                    title,
                    emptyToPlaceholder(description, "(없음)"),
                    emptyToPlaceholder(comments, "(없음)"),
                    emptyToPlaceholder(scriptPlain, "(없음)")
            ), MAX_CONTEXT_CHARS);

            RecipeCreateRequestDto recipeDto = null;

            if (!useUrlFallback && isTextSufficient(description, comments, scriptPlain)) {
                log.info("✅ [텍스트 모드] 자막/설명이 충분함. 1차 분석 시도.");
                try {
                    RecipeCreateRequestDto rawRecipe = grokClientService.generateRecipeStep1(getExtractionPrompt(), fullContext).join();

                    if (rawRecipe == null) {
                        useUrlFallback = true;
                    } else {
                        Boolean isRecipe = rawRecipe.getIsRecipe();

                        if (Boolean.FALSE.equals(isRecipe)) {
                            log.warn("🚫 Grok 확정 판정: 레시피 아님. 사유: {}", rawRecipe.getNonRecipeReason());
                            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                                    "레시피 영상이 아닙니다: " + rawRecipe.getNonRecipeReason());
                        }

                        if (!Boolean.TRUE.equals(isRecipe)) {
                            log.info("⚠️ Grok 판단 모호(null). Gemini 분석으로 전환합니다.");
                            useUrlFallback = true;
                        }
                    }

                    if (!useUrlFallback) {
                        log.info("🔨 [텍스트 모드] 2차 가공(가격/영양소 계산) 시작");
                        String refineSystemPrompt =
                                "너는 JSON 데이터 검증 AI다. 창의성을 배제하고 오직 규격 준수에만 집중하라. " +
                                        "입력 JSON의 isRecipe, nonRecipeReason 값은 절대 변경하지 마라.";

                        recipeDto = grokClientService.refineRecipeToStandard(refineSystemPrompt, rawRecipe).join();

                        if (recipeDto == null) {
                            useUrlFallback = true;
                        } else {
                            if (!Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
                                log.warn("⚠️ refine가 isRecipe를 변경함(위반). fallback 전환. isRecipe={}, reason={}",
                                        recipeDto.getIsRecipe(), recipeDto.getNonRecipeReason());
                                useUrlFallback = true;
                                recipeDto = null;
                            }
                        }
                    }
                } catch (CustomException ce) {
                    throw ce;
                } catch (Exception e) {
                    log.warn("⚠️ 텍스트 분석 실패. URL 분석으로 전환합니다. 이유: {}", safeMsg(e));
                    useUrlFallback = true;
                }
            } else if (!useUrlFallback) {
                log.info("ℹ️ 텍스트 정보 부족. 바로 URL 분석으로 진입합니다.");
                useUrlFallback = true;
            }

            if (useUrlFallback || recipeDto == null) {
                log.info("🎥 [멀티모달 모드] Gemini 3.0 Flash에게 영상 URL 직접 전송");

                RecipeCreateRequestDto geminiRecipe = geminiMultimodalService
                        .generateRecipeFromYoutubeUrl(getExtractionPrompt(), title, canonicalUrl)
                        .join();

                if (geminiRecipe == null) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "레시피 생성에 실패했습니다.");
                }
                if (!Boolean.TRUE.equals(geminiRecipe.getIsRecipe())) {
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                            "레시피 영상이 아닙니다: " + geminiRecipe.getNonRecipeReason());
                }

                if (geminiRecipe != null) {
                    log.info("🔨 [멀티모달 모드] 2차 가공(가격/영양소 계산) 시작");
                    String refineSystemPrompt =
                            "너는 JSON 데이터 검증 AI다. 창의성을 배제하고 오직 규격 준수에만 집중하라. " +
                                    "입력 JSON의 isRecipe, nonRecipeReason 값은 절대 변경하지 마라.";
                    recipeDto = grokClientService
                            .refineRecipeToStandard(refineSystemPrompt, geminiRecipe)
                            .join();

                    if (recipeDto != null) {
                        recipeDto.setIsRecipe(true);
                        recipeDto.setNonRecipeReason(null);
                    }
                }
            }

            if (recipeDto == null) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "레시피 생성에 실패했습니다.");
            }

            if (Boolean.FALSE.equals(recipeDto.getIsRecipe())) {
                String reason = recipeDto.getNonRecipeReason();
                log.warn("🚫 레시피 아님: {}", reason);
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 영상이 아닙니다: " + (reason == null ? "" : reason));
            }

            if (recipeDto.getTitle() == null || recipeDto.getTitle().isBlank() || "제목 미상".equals(title)) {
                recipeDto.setTitle(recipeDto.getTitle() != null && !recipeDto.getTitle().isBlank() ? recipeDto.getTitle() : title);
            }
            recipeDto.setYoutubeUrl(canonicalUrl);

            mergeDuplicateIngredientsByNameAndUnit(recipeDto);

            PresignedUrlResponse response = saveRecipeTransactional(recipeDto, userId);

            log.info("💾 레시피 저장 완료: ID={}", response.getRecipeId());
            return CompletableFuture.completedFuture(response);

        } catch (CustomException e) {
            if (e.getErrorCode() == ErrorCode.INVALID_INPUT_VALUE) {
                log.warn("🚫 레시피 아님 판정으로 쿼터 환불 없이 종료: userId={}", userId);
            } else {
                log.warn("❌ 추출 실패(System/AI Error). 쿼터 환불: userId={}", userId);
                dailyQuotaService.refundIfPolicyAllows(userId, QuotaType.YOUTUBE_EXTRACTION);
            }
            throw e;
        }
    }

    private PresignedUrlResponse saveRecipeTransactional(RecipeCreateRequestDto recipeDto, Long userId) {
        return transactionTemplate.execute(status -> {
            RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
            request.setRecipe(recipeDto);
            return recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.AI);
        });
    }

    private boolean isTextSufficient(String description, String comments, String scriptPlain) {
        if (scriptPlain != null && scriptPlain.length() >= 50) return true;

        String bodyText = (nullToEmpty(description) + " " + nullToEmpty(comments));
        if (bodyText.length() < 50) return false;

        boolean hasUnit = UNIT_PATTERN.matcher(bodyText).find();
        boolean hasIngredientKeyword = INGREDIENT_KEYWORD_PATTERN.matcher(bodyText).find();
        boolean hasAction = STEP_ACTION_PATTERN.matcher(bodyText).find();

        return hasUnit || (hasIngredientKeyword && hasAction);
    }

    private void mergeDuplicateIngredientsByNameAndUnit(RecipeCreateRequestDto recipeDto) {
        if (recipeDto.getIngredients() == null || recipeDto.getIngredients().isEmpty()) return;

        Map<String, RecipeIngredientRequestDto> merged = new LinkedHashMap<>();

        for (RecipeIngredientRequestDto cur : recipeDto.getIngredients()) {
            if (cur == null || cur.getName() == null) continue;

            String name = cur.getName().trim();
            String unit = cur.getCustomUnit() == null ? "" : cur.getCustomUnit().trim();
            String key = (name + "|" + unit).toLowerCase();

            if (merged.containsKey(key)) {
                RecipeIngredientRequestDto exist = merged.get(key);
                double q1 = parseQuantitySafe(exist.getQuantity());
                double q2 = parseQuantitySafe(cur.getQuantity());
                exist.setQuantity(formatQuantity(q1 + q2));
            } else {
                merged.put(key, cur);
            }
        }
        recipeDto.setIngredients(new ArrayList<>(merged.values()));
    }

    private double parseQuantitySafe(String quantity) {
        if (quantity == null || quantity.isBlank()) return 0.0;
        try {
            return Double.parseDouble(quantity.trim());
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }

    private String formatQuantity(double value) {
        return (value == (long) value) ? String.format("%d", (long) value) : String.valueOf(value);
    }

    private String nullToEmpty(String s) { return s == null ? "" : s; }
    private String emptyToPlaceholder(String s, String placeholder) { return (s == null || s.isBlank()) ? placeholder : s; }

    private String cap(String s, int max) {
        if (s == null) return "";
        if (s.length() <= max) return s;
        int head = (int) (max * 0.7);
        int tail = max - head;
        return s.substring(0, head) + "\n...(truncated)...\n" + s.substring(s.length() - tail);
    }

    private String safeMsg(Throwable t) {
        if (t == null) return "";
        return t.getMessage() != null ? t.getMessage() : t.getClass().getSimpleName();
    }
}