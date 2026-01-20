package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.YoutubeRecommendation;
import com.jdc.recipe_service.domain.entity.YoutubeTargetChannel;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.media.YtDlpService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@Slf4j
public class RecipeExtractionService {

    private static final int MAX_CONTEXT_CHARS = 100_000;
    private static final int MAX_SCRIPT_CHARS  = 80_000;
    private static final int MAX_DESC_CHARS    = 10_000;
    private static final int MAX_CMT_CHARS     = 1_000;
    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;
    private static final Set<String> SPECIAL_QTY = Set.of("약간");

    private static final List<String> NOISE_KEYWORDS = List.of(
            // 1. 기존 먹방/브이로그
            "먹방", "mukbang", "asmr", "이팅사운드",
            "리뷰", "후기", "탐방", "review", "맛집", "맛있게 먹는",
            "브이로그", "vlog", "일상", "grwm", "what i eat",
            "식단일기", "장보기", "언박싱",
            "소분", "정리", "살림", "청소", "룸투어",
            "costco", "코스트코", "이마트", "trader joe",

            // 2. 예능/방송/상황극
            "예능", "방송", "출연", "개그", "성대모사",
            "ㅋㅋ", "ㅎㅎ", "ㅠㅠ", "웃긴", "대박", "참교육",
            "반응", "결말", "충격", "근황", "논란", "해명",
            "몰카", "prank", "챌린지", "challenge",
            "유형", "사람 특징", "공감", "상황극", "꽁트",
            "비하인드", "ng", "하이라이트", "모음", "zip",

            // 3. 공지/홍보/예고
            "공지", "이벤트", "나눔", "구독자", "q&a", "qna", "질문",
            "예고", "티저", "teaser", "trailer", "미리보기",
            "라이브", "live", "다시보기", "full ver"
    );

    private static final Pattern YOUTUBE_URL_PATTERN = Pattern.compile(
            "(?i)^(https?://)?(www\\.)?(youtube\\.com|youtu\\.be)/.+$"
    );

    private static final Pattern UNIT_PATTERN = Pattern.compile(
            "(?i)(큰술|작은술|spoon|tbs|tsp|cup|\\b[0-9.]+\\s?g\\b|\\b[0-9.]+\\s?ml\\b|\\b[0-9.]+\\s?oz\\b|한\\s?꼬집|약간)"
    );
    private static final Pattern INGREDIENT_KEYWORD_PATTERN = Pattern.compile(
            "(?i)(재료|ingredient|준비물|필요한\\s?것)"
    );
    private static final Pattern STEP_ACTION_PATTERN = Pattern.compile(
            "(?i)(만드는|방법|recipe|step|direction|넣고|볶|끓|굽|튀기|섞|다지|채썰|chop|mix|boil|fry|bake|roast)"
    );

    private final YtDlpService ytDlpService;
    private final GrokClientService grokClientService;
    private final GeminiMultimodalService geminiMultimodalService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;
    private final RecipeFavoriteService recipeFavoriteService;
    private final RecipeActivityService recipeActivityService;

    private final RecipeRepository recipeRepository;
    private final YoutubeTargetChannelRepository youtubeTargetChannelRepository;
    private final YoutubeRecommendationRepository youtubeRecommendationRepository;

    private final TransactionTemplate transactionTemplate;
    private final Executor extractionExecutor;

    private final AtomicBoolean isRefreshing = new AtomicBoolean(false);
    private final ConcurrentHashMap<String, CompletableFuture<PresignedUrlResponse>> extractionTasks = new ConcurrentHashMap<>();

    public RecipeExtractionService(
            YtDlpService ytDlpService,
            GrokClientService grokClientService,
            GeminiMultimodalService geminiMultimodalService,
            RecipeService recipeService,
            DailyQuotaService dailyQuotaService, RecipeActivityService recipeActivityService,
            RecipeRepository recipeRepository,
            RecipeFavoriteService recipeFavoriteService, YoutubeTargetChannelRepository youtubeTargetChannelRepository, YoutubeRecommendationRepository youtubeRecommendationRepository,
            TransactionTemplate transactionTemplate,
            @Qualifier("recipeExtractionExecutor") Executor extractionExecutor
    ) {
        this.ytDlpService = ytDlpService;
        this.grokClientService = grokClientService;
        this.geminiMultimodalService = geminiMultimodalService;
        this.recipeService = recipeService;
        this.dailyQuotaService = dailyQuotaService;
        this.recipeActivityService = recipeActivityService;
        this.recipeRepository = recipeRepository;
        this.recipeFavoriteService = recipeFavoriteService;
        this.youtubeTargetChannelRepository = youtubeTargetChannelRepository;
        this.youtubeRecommendationRepository = youtubeRecommendationRepository;
        this.transactionTemplate = transactionTemplate;
        this.extractionExecutor = extractionExecutor;
    }

    private String getExtractionPrompt() {
        return """
            [SYSTEM]
            너는 다양한 요리 영상(집밥, 셰프, 초보 레시피 등)을 분석하는 전문 AI다. 출력은 반드시 "단 하나의 JSON 객체"만 허용한다.
            설명/주석/마크다운/코드펜스(```)/여분 텍스트를 절대 출력하지 마라.
            
            ==============================
            0) OUTPUT CONTRACT (최우선)
            - JSON 1개만 출력
            - 키 이름 변경/추가 금지
            - 문자열 필드는 ""(빈값) 금지
            - 숫자 필드는 null/"" 금지
            - timeline, nonRecipeReason(isRecipe=true일 때)만 null 허용 (그 외 null 금지)
            ==============================
            
            1) 레시피 영상 판별 (Critical)
            입력(제목/설명/자막/댓글)이 "요리 조리법"이 아니면, 아래 JSON만 그대로 출력하고 종료:
            {
              "isRecipe": false,
              "nonRecipeReason": "먹방/리뷰/브이로그 등 조리법이 아닙니다."
            }
            
            2) 레시피 추출 (isRecipe=true일 때만)
            [근거 우선순위] Script > Description > Title > Comments
            - 광고/링크/쿠폰/인사/웃음/잡담은 전부 무시
            - 댓글은 자막/설명과 100% 일치할 때만 보조로 참고
            - **영상에 명확한 근거 없는 정보는 절대 추측/추가/창의적으로 채우지 마라**
            
            [Universal Culinary Principles]
            - 암묵적 재료: 시각/조리 행위로 "거의 확실"할 때만 포함
            - 다양한 요리 스타일(이탈리아, 한국, 디저트 등)에 맞춰 유연하게 분석. 영상 톤(캐주얼/전문)을 반영하되, 일반적으로 적용 가능하게.
            
            ==============================
            3) 성공 JSON 스키마 (반드시 이 형태)
            {
              "isRecipe": true,
              "nonRecipeReason": null,
              "title": "요리 제목",
              "dishType": "볶음",
              "description": "영상 톤의 1~2문장 소개(맛/식감 1개 + 핵심특징 1개 포함)",
              "cookingTime": 15,
              "cookingTools": ["도구1","도구2"],
              "servings": 1,
              "ingredients": [
                { "name": "재료명", "quantity": "수량문자열", "unit": "단위" }
              ],
              "steps": [
                { "stepNumber": 0, "instruction": "지시문", "action": "액션", "timeline": "MM:SS" }
              ],
              "tags": ["태그1","태그2","태그3"],
              "marketPrice": 1500,
              "cookingTips": "문장으로만 3~5개 팁을 이어서 작성"
            }
            ==============================
            
            4) 필드 규칙 (위반 시 전체 실패)
            [dishType]
            - dishType은 아래 중 정확히 1개만 선택: "볶음", "국/찌개/탕", "구이", "무침/샐러드", "튀김/부침", "찜/조림", "오븐요리", "생식/회", "절임/피클류", "밥/면/파스타", "디저트/간식류"
            - 빈 문자열/공백 금지
            
            [숫자 필드]
            - cookingTime: 0 이상의 정수(분)
            - servings: 정수(반올림하여 출력), 소수 금지
            - marketPrice: 정수, 100원 단위 올림(ceil)
            - quantity: 아래 형식만 허용(문자열이지만 수치로 해석 가능해야 함)
              - 정수: "2"
              - 소수: "0.5"
              - 분수: "1/2"  (혼합분수 "1 1/2" 금지, 공백 금지)
              - 단, 추정 불가(영상에서 수량 단서 없음)인 경우에만 quantity="약간" 허용
            - quantity/marketPrice/cookingTime/servings는 null/"" 절대 금지
            
            [timeline]
            - "MM:SS" 문자열 또는 null만 허용
            - 자막에 [04:12] 또는 0:02 형태가 있으면 우선 매핑
            - 시간을 확실히 못 찾으면 억지로 추측하지 말고 null
            
            [ingredients] (DB 매칭을 위한 핵심 규칙)
            - **[중요] 단일 명사 원칙:** '또는', 'or', '/', '대체', '취향껏' 같은 표현 금지. 영상에서 실제로 사용한 **가장 메인이 되는 재료 하나**만 적어라.
            - quantity: 단위와 수량을 명확히 분리하고, null 금지.
            - **[부재료 포착]:** 파, 깨, 참기름, 후추 등 셰프가 조리 중간에 "향"이나 "마무리"를 위해 소량 첨가하는 재료도 놓치지 말고 포함하라.
            - **[소스 분석]:** 영상에서 별도의 소스(양념장)를 배합하는 과정이 나온다면, 그 배합에 들어가는 재료(간장, 설탕, 식초 등)를 모두 분리하여 적어라.
            
            [steps] (영상 순서 최우선, '극도로 상세한' 서술형 작성)
            - stepNumber는 0부터 1씩 증가
            - **[Hyper-Detailing Instruction Rule - 6대 필수 요소]:**
              각 단계는 **2~3문장**으로 작성하되, 문장 수를 줄이려고 핵심 정보를 생략하지 마라.
              6대 요소는 영상/자막에 근거가 있는 것만 포함. 근거 없으면 생략. 절대 지어내지 마라.
              아래 항목 중 해당되는 것은 빠짐없이 문장에 녹여내라:
             1. **무엇을 (Specifics):** 재료의 상태나 도구. (Bad: "파를 넣고" -> Good: "파의 흰 대 부분만 송송 썰어 예열된 팬에 넣고")
             2. **어떻게 (Action):** 구체적 동작. (Bad: "볶는다" -> Good: "기름이 튀지 않게 조심하며 저어가며 볶습니다")
             3. **불/온도 (Heat):** 강불/중불/약불, 잔열, "연기가 날 정도로 달궈지면", "끓기 시작하면 약불로 줄여"
             4. **시간/횟수 (Time):** "3분간", "30초 정도", "3번에 나눠서"
             5. **멈춤 타이밍 (Visual Cue & State):** 시간보다 **'상태'**가 더 중요하다. (예: "양파가 투명해질 때까지", "가장자리가 갈색이 돌면", "소스가 걸쭉해질 때까지")
             6. **이유/팁 (Why & Insight):** 단순 조리 순서를 넘어, **셰프가 강조하는 이유나 철학**을 반드시 포함하라. (예: "그래야 잡내가 날아갑니다", "지금 간을 해야 재료에 맛이 뱁니다")
            
            - **[금지어]:** "적당히", "알맞게", "잘". -> 반드시 "어떤 상태가 될 때까지"라고 풀어서 써라.
            - **[Flow]:** 같은 단계 안에서는 '행동 → 관찰(상태) → 이유/다음행동' 순으로 자연스럽게 이어 써라.
            - **[순서 규칙: 타임라인 오름차순]:** 요리의 논리적 순서(재료손질->조리)보다 **'영상의 편집/진행 순서'**를 최우선으로 따르라.
            - 사용자가 영상을 보며 따라 할 수 있도록, `step 0` -> `step 1`으로 갈수록 `timeline` 시간도 반드시 커져야 한다. (시간 역전 금지)
            - timeline: 해당 동작이 시작되는 정확한 시간 (MM:SS)
            - action: "썰기","다지기","볶기","튀기기","끓이기","찌기","데치기","구이","조림","무치기","섞기","부치기" 중 택1
            
            [Chef Insight Capture - 누락 금지]
            - 영상에서 조리의 "이유/원리/선택 기준"을 설명하면 절대 누락하지 마라.
            - 아래 유형은 반드시 결과에 포함:
              1) 기술/과정의 이유(왜 이런 순서/불/상태를 고집하는지)
              2) 재료/제품 선택 기준(면/오일/재료 선택 논리, 가성비/등급/보관 포인트)
              3) 향/풍미 보강 팁(향을 옮기는 방법, 마무리 포인트)
            - 배치 규칙:
              - “행동과 직결된 이유”는 해당 step instruction 안에 1문장으로 포함(Why & Insight).
              - “제품/재료 선택 팁(가성비/등급/구매 요령)”은 cookingTips에 1~2문장으로 포함.
            - 제외 규칙:
              - 인사, 근황, 농담, 협찬 멘트 등 조리와 무관한 대화는 steps/cookingTips 모두에서 제외
            
            [tags] (허용 목록에서 최대 3개)
            "🏠 홈파티","🌼 피크닉","🏕️ 캠핑","🥗 다이어트 / 건강식","👶 아이와 함께","🍽️ 혼밥","🍶 술안주","🥐 브런치","🌙 야식","⚡ 초스피드 / 간단 요리","🎉 기념일 / 명절","🍱 도시락","🔌 에어프라이어","🍲 해장","👨‍🍳 셰프 레시피"
            - 🍽️ 혼밥: servings==1일 때만
            - ⚡ 초스피드 / 간단 요리: cookingTime<=15일 때만
            - 🔌 에어프라이어: cookingTools에 오븐/에어프라이어 포함 OR dishType이 구이/튀김/부침일 때만
            - 🥗 다이어트 / 건강식: 설탕/튀김/가공육이 주재료가 아니고 채소·단백질 위주일 때만
            - 👨‍🍳 셰프 레시피: 제목/설명/자막에 셰프/대가/명장/호텔 등 명확 근거가 있을 때만
            - servings>2이면 🍽️ 혼밥 금지
            - cookingTime>20(오븐/찜 포함)이면 ⚡, 🥗 금지
            
            [marketPrice] (배달앱 메뉴판 감각, 선형곱 금지)
            - 비싼 재료 TOP3만 반영(기본양념/물/소금/설탕/간장/마늘 등은 무시)
            - 등급 1개 선택: A(SIDE) / B(MEAL) / C(PREMIUM)
              - A: 2,000~7,500 (A이면서 1인분이면 8,000 초과 금지)
              - B: 9,000~15,900
              - C: 17,900~45,900
            - 공유형(전골/탕/찜/떡볶이 등): 1인×1.0, 2인×1.4, 3인×1.7, 4인+×2.0
            - 개별형(1인 1그릇): 1인×1.0, 2인×1.9, 3인×2.8, 4인+×(servings*0.9)
            - 극소 메뉴(공기밥/후라이/소스/단무지 등): servings가 커도 개당 2,500원 초과 금지
            - 전체 범위: 1,500~150,000
            - 100원 단위 올림 정수 출력
            
            [cookingTips]
            - 일반적인 요리 상식이 아니라, **이 영상에서 요리사가 강조한 꿀팁** 3~5가지를 문장으로 적어라.
            - 숫자/목록표시/접두어("팁:") 금지
            """;
    }

    private String getExtractionPromptV2() {
        return """
            당신은 레시피 추출 AI입니다. 오직 유효한 JSON만 출력하세요.
            
            ## 최우선 규칙
            - 단일 JSON 객체만 출력 (마크다운, 코드펜스, 설명 절대 금지)
            - timeline과 nonRecipeReason만 null 허용
            - 모든 숫자 필드는 0 이상 (빈 문자열, null 금지)
            - 영상에 없는 정보는 절대 창작 금지
            
            ## 1단계: 레시피 판별
            조리법이 아니면 즉시 반환:
            {
              "isRecipe": false,
              "nonRecipeReason": "먹방/리뷰/브이로그 - 조리법 없음"
            }
            
            제외 키워드: 먹방, mukbang, ASMR, 리뷰, 브이로그, vlog, 장보기, 언박싱, 예능, 챌린지, 공지, 라이브
            
            ## 2단계: 데이터 추출 (isRecipe=true일 때만)
            
            ### [CRITICAL] 다중 레시피 처리 규칙
            영상에 두 가지 이상의 레시피(예: 버전1 vs 버전2, 매운맛 vs 순한맛)가 나올 경우:
            1. **단일 선택:** 가장 비중 있게 다뤄지거나, 제목과 가장 일치하거나, 일반 사용자가 따라 하기 쉬운 **'메인 레시피 1개'**만 선택하라.
            2. **혼합 금지:** 선택하지 않은 버전의 재료나 조리법을 절대 섞지 마라. (예: 버전1의 재료와 버전2의 소스를 섞으면 안 됨)
            3. **[중요] 경계 설정(Boundary):**
               - 메인 레시피의 조리가 끝나고 **새로운 버전(Recipe 2)이나 다른 요리가 시작되는 시점**에서 `steps` 추출을 멈춰라.
               - 이후에 나오는 내용은 `steps`가 아니라 `cookingTips`에 '참고 정보'로만 적어야 한다.
            
            근거 우선순위: Script(자막) > Description > Title > Comments
            
            ### 출력 구조
            {
              "isRecipe": true,
              "nonRecipeReason": null,
              "title": "영상의 요리명",
              "dishType": "11개 중 정확히 1개",
              "description": "1-2문장: 맛/식감 + 핵심특징",
              "cookingTime": 15,
              "cookingTools": ["도구1"],
              "servings": 1,
              "ingredients": [...],
              "steps": [...],
              "tags": ["태그1","태그2","태그3"],
              "marketPrice": 1500,
              "cookingTips": "문장으로 3-5개 팁 (불릿 금지)"
            }
            
            ### 필드별 규칙
            
            **cookingTime** - [필수 추론]:
            - **[절대 0 금지]:** 조리 시간이 0분인 요리는 없다.
            - 영상에 시간이 명시되지 않았다면, **재료의 양과 조리 과정(끓이기, 굽기 등)을 종합적으로 분석하여 현실적인 소요 시간을 스스로 추론하여 기입하라.**
            
            **dishType** - 정확히 1개만 선택:
            "볶음", "국/찌개/탕", "구이", "무침/샐러드", "튀김/부침", "찜/조림", "오븐요리", "생식/회", "절임/피클류", "밥/면/파스타", "디저트/간식류"
            
            **ingredients** - DB 매칭을 위한 핵심 규칙:
            1. 단일 명사 원칙: "또는", "/", "대체" 표현 금지
            2. 실제 사용한 메인 재료 1개만
            3. quantity 형식: "2", "0.5", "1/2" (혼합분수 금지)
            4. quantity="약간"은 정말 추정 불가능할 때만
            5. 소스 분해: 양념장 만드는 장면 있으면 간장/설탕/식초 등 모두 분리
            6. 부재료 포착: 파/깨/참기름/후추 등 조리 중 추가하는 것 누락 금지
            
            예시:
            [
              { "name": "돼지고기", "quantity": "300", "unit": "g" },
              { "name": "간장", "quantity": "2", "unit": "큰술" },
              { "name": "깨", "quantity": "약간", "unit": "약간" }
            ]
            
            **steps** - 극도로 상세한 지시문 작성:
            
            기본 구조:
            - stepNumber: 0부터 시작
            - timeline: "MM:SS" 형식 또는 null (확실하지 않으면 null)
            - timeline은 반드시 오름차순 (시간 역전 금지)
            - instruction: 2-3문장으로 상세 작성
            - action: "썰기","다지기","볶기","튀기기","끓이기","찌기","데치기","굽기","조림","무치기","씻기","부치기" 중 1개
            
            instruction 작성시 6대 요소 (영상에 근거 있을 때만 포함):
            1. 무엇을 (Specifics): "양파 1개를 0.5cm 두께로 채썰어"
            2. 어떻게 (Action): "나무 주걱으로 저어가며 볶습니다"
            3. 불/온도 (Heat): "중불", "강불로 올려", "연기가 날 정도로"
            4. 시간/횟수 (Time): "3분간", "30초 정도", "2번 뒤집어"
            5. 멈춤 타이밍 (Visual Cue): "양파가 투명해질 때까지", "소스가 걸쭉해지면"
            6. 이유/팁 (Why): "그래야 식감이 살아납니다", "지금 간을 해야 맛이 뱁니다"
            
            금지 표현: "적당히", "알맞게", "잘" → 구체적 상태/조건으로 변경
            
            Flow 패턴: 행동 → 관찰(상태) → 이유/다음행동
            
            예시:
            {
              "stepNumber": 0,
              "instruction": "돼지고기는 한입 크기로 썰어 키친타월로 핏물을 제거합니다. 이렇게 해야 누린내가 나지 않습니다. 준비된 고기에 간장 1큰술, 설탕 0.5큰술을 넣고 10분간 재워둡니다.",
              "action": "썰기",
              "timeline": "00:45"
            }
            
            **Chef Insight 포착** - 영상에서 조리 이유/원리 설명시 절대 누락 금지:
            
            3가지 유형 (영상에 있을 때만 포함):
            1. 기술/과정의 이유
               → step instruction에 1문장 포함
           
            2. 재료/도구 선택 기준
               → cookingTips에 포함
               (브랜드/등급/품질 언급, 왜 이 재료인지 설명)
            
            3. 향/풍미 보강 팁
               → cookingTips에 포함
               (부재료 활용, 타이밍, 온도 등)
            
            제외: 인사, 근황, 농담, 광고 등 조리 무관 내용
            
            **tags** - 조건부 허용 (최대 3개):
            "🏠 홈파티","🌼 피크닉","🏕️ 캠핑","🥗 다이어트 / 건강식","👶 아이와 함께","🍽️ 혼밥","🍶 술안주","🥐 브런치","🌙 야식","⚡ 초스피드 / 간단 요리","🎉 기념일 / 명절","🍱 도시락","📌 에어프라이어","🍲 해장","👨‍🍳 셰프 레시피"
            
            조건:
            - 🍽️ 혼밥: servings==1일 때만
            - ⚡ 초스피드: cookingTime<=15일 때만
            - 📌 에어프라이어: cookingTools에 오븐/에어프라이어 포함시만
            - 🥗 다이어트: 튀김/가공육 아니고 채소·단백질 위주일 때만
            - 👨‍🍳 셰프: 제목/설명에 셰프/대가/명장 등 명확 근거 있을 때만
            
            [marketPrice] (2026년 대한민국 외식/반찬가게 판매가 기준):
            - **[중요] 식당/반찬가게의 '소비자 가격'을 예측하라.** (단, 메뉴의 급에 맞는 현실적 가격 책정 필수)
            
            **[카테고리별 가격 가이드라인 (1인분/1팩 기준)]**:
            0. **초간단/사이드/반찬** (계란후라이, 공기밥, 간단 나물 1종, 소스, 피클): **1,000 ~ 4,500원**
               - (주의: 메인 식사가 안 되는 단순 반찬은 절대 5,000원을 넘기지 마라.)
            1. **저가형/분식/간식** (김밥, 라면, 떡볶이, 토스트, 샌드위치): 4,500 ~ 8,500원
            2. **일반 식사/한식** (김치찌개, 덮밥, 볶음밥, 국밥): 9,000 ~ 13,000원
            3. **양식/일품/브런치** (파스타, 리조또, 샐러드볼): **14,000 ~ 22,000원**
            4. **메인 요리/안주** (치킨, 족발, 전골, 탕수육): 22,000 ~ 35,000원
            5. **프리미엄** (스테이크, 장어, 회, 갈비찜): 40,000원 이상
            
            **[조정 규칙]**:
            - **인분 계산:**
              - 개별 메뉴: 1인분 가격 × servings
              - 공유 메뉴(전골, 찜): 2인(x1.5), 3인(x2.0) 감경 적용.
            - 100원 단위 반올림.
            
            **cookingTips**:
            - 일반 상식 아닌, 이 영상에서 셰프가 강조한 팁 3-5가지
            - 영상에 있을 때만: **steps에 쓴 내용을 제외하고**, 재료팁/대체법/수습법(재료 선택 이유, 기술의 원리, 타이밍 팁) 위주로 작성.
            - 숫자/목록/접두어 금지
            - **[필수]** 만약 영상에 다른 버전의 레시피가 소개되었다면, 여기서 "영상에서는 ~하는 방법도 소개하고 있습니다"라고 한 줄로 언급하라.
            - 자연스러운 문장으로 이어서 작성
            
            ## 실행 순서
            1. 레시피 영상 판별
            2. isRecipe=false면 즉시 종료
            3. 근거 우선순위로 데이터 추출
            4. ingredients: 단일 명사, 소스 분해, 부재료 포함
            5. steps: 6대 요소 기반 2-3문장 상세 작성, timeline 오름차순
            6. Chef Insight 누락 금지
            7. 모든 숫자 필드 0 이상 확인
            8. 단일 JSON 출력 (코드펜스/설명 제거)
            
            ## 절대 금지
            - ```json ``` 코드펜스
            - "이 레시피는..." 같은 설명
            - 근거 없는 추측
            - 빈 문자열/null (허용 필드 제외)
            - 중복 재료
            - "적당히", "알맞게" 모호한 표현
            - steps의 timeline 시간 역전
            """;
    }

    public CompletableFuture<PresignedUrlResponse> extractAndCreateRecipe(String videoUrl, Long userId, String nickname) {
        log.info("🚀 유튜브 레시피 추출 요청: URL={}, UserID={}", videoUrl, userId);

        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }
        String videoId = extractVideoId(videoUrl);
        if (videoId == null) throw new CustomException(ErrorCode.INVALID_URL_FORMAT);

        CompletableFuture<PresignedUrlResponse> sharedTask = extractionTasks.computeIfAbsent(videoId, key -> {
            log.info("🚌 [버스 출발] 새로운 추출 작업 시작 (운전자: {}). VideoID: {}", userId, key);
            return CompletableFuture.supplyAsync(() -> {
                try {
                    return processActualExtractionLogic(videoUrl, userId, key, nickname);
                } finally {
                    extractionTasks.remove(key);
                    log.info("🏁 [종점 도착] 작업 종료 및 맵에서 제거. VideoID: {}", key);
                }
            }, extractionExecutor).orTimeout(5, TimeUnit.MINUTES);
        });

        sharedTask.whenComplete((res, ex) -> {
            if (extractionTasks.remove(videoId) != null) {
                log.info("🏁 [종점 도착] 맵에서 Key 제거 완료: {}", videoId);
            }
        });

        return sharedTask.handle((response, ex) -> {
            if (ex != null) {
                Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                if (cause instanceof RuntimeException re) throw re;
                throw new RuntimeException(cause);
            }

            try {
                log.info("⭐ 유저 {}에게 레시피 {} 즐겨찾기/로그 추가", userId, response.getRecipeId());
                addFavoriteToUser(userId, response.getRecipeId());
                recipeActivityService.saveLog(userId, nickname, ActivityLogType.YOUTUBE_EXTRACT);
            } catch (Exception e) {
                log.warn("⚠️ 후속 처리(즐겨찾기 등) 실패: userId={}, error={}", userId, e.getMessage());
            }
            return response;
        });
    }

    private PresignedUrlResponse processActualExtractionLogic(String videoUrl, Long userId, String videoId, String nickname) {
        boolean shorts = isShortsUrl(videoUrl);
        String storageUrl = buildStorageYoutubeUrl(videoId, shorts);
        String watchUrl  = buildStorageYoutubeUrl(videoId, false);
        String shortsUrl = buildStorageYoutubeUrl(videoId, true);

        Optional<Recipe> existingRecipe = recipeRepository.findByYoutubeUrl(watchUrl)
                .or(() -> recipeRepository.findByYoutubeUrl(shortsUrl));

        if (existingRecipe.isPresent()) {
            log.info("♻️ 이미 존재하는 레시피 발견. 생성 건너뜀.");
            return handleExistingRecipe(existingRecipe.get()).join();
        }

        dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.YOUTUBE_EXTRACTION);

        String title = "제목 미상";
        String description = "";
        String comments = "";
        String scriptPlain = "";
        String channelName = "";
        String channelId = "";
        String originalVideoTitle = "";
        String thumbnailUrl = "";
        String channelProfileUrl = "";
        Long subscriberCount = 0L;
        boolean useUrlFallback = false;

        try {
            YtDlpService.YoutubeFullDataDto videoData = ytDlpService.getVideoDataFull(videoUrl);

            title = nullToEmpty(videoData.title());
            description = cap(nullToEmpty(videoData.description()), MAX_DESC_CHARS);
            comments = cap(nullToEmpty(videoData.comments()), MAX_CMT_CHARS);
            scriptPlain = cap(nullToEmpty(videoData.scriptTimecoded()), MAX_SCRIPT_CHARS);
            channelName = nullToEmpty(videoData.channelName());
            channelId = nullToEmpty(videoData.channelId());
            originalVideoTitle = nullToEmpty(videoData.title());
            thumbnailUrl = nullToEmpty(videoData.thumbnailUrl());
            channelProfileUrl = nullToEmpty(videoData.channelProfileUrl());
            subscriberCount = videoData.youtubeSubscriberCount();

            String canonicalUrl = nullToEmpty(videoData.canonicalUrl());
            Optional<Recipe> existingRecipeCanonical = recipeRepository.findByYoutubeUrl(canonicalUrl);
            if (existingRecipeCanonical.isPresent()) {
                log.info("♻️ 이미 존재하는 레시피 발견 (Canonical URL). 쿼터 환불 및 연결: ID={}", existingRecipeCanonical.get().getId());
                dailyQuotaService.refundIfPolicyAllows(userId, QuotaType.YOUTUBE_EXTRACTION);
                return handleExistingRecipe(existingRecipeCanonical.get()).join();
            }

        } catch (Exception e) {
            log.warn("⚠️ yt-dlp 실패 -> Gemini 모드 전환: {}", safeMsg(e));
            useUrlFallback = true;
        }

        try {
            String fullContext = cap(("""
            영상 URL: %s
            영상 제목: %s
            영상 설명: %s
            고정/인기 댓글: %s
            자막: %s
            """).formatted(storageUrl, title,
                    emptyToPlaceholder(description, "(없음)"),
                    emptyToPlaceholder(comments, "(없음)"),
                    emptyToPlaceholder(scriptPlain, "(없음)")
            ), MAX_CONTEXT_CHARS);

            RecipeCreateRequestDto recipeDto = null;

            if (!useUrlFallback && isTextSufficient(description, comments, scriptPlain)) {
                log.info("✅ [텍스트 모드] 자막/설명이 충분함. 1차 분석 시도.");
                try {
                    RecipeCreateRequestDto rawRecipe = grokClientService.generateRecipeStep1(getExtractionPromptV2(), fullContext).join();

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
                        .generateRecipeFromYoutubeUrl(getExtractionPromptV2(), title, storageUrl)
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
            recipeDto.setYoutubeUrl(storageUrl);
            recipeDto.setYoutubeChannelName(channelName);
            recipeDto.setYoutubeChannelId(channelId);
            recipeDto.setYoutubeVideoTitle(originalVideoTitle);
            recipeDto.setYoutubeThumbnailUrl(thumbnailUrl);
            recipeDto.setYoutubeChannelProfileUrl(channelProfileUrl);
            recipeDto.setYoutubeSubscriberCount(subscriberCount);

            mergeDuplicateIngredientsByNameAndUnit(recipeDto);

            PresignedUrlResponse response = saveRecipeTransactional(recipeDto, OFFICIAL_RECIPE_USER_ID);

            log.info("💾 신규 생성 및 즐겨찾기 추가 완료: ID={}, UserID={}", response.getRecipeId(), userId);
            return response;

        } catch (CustomException e) {
            if (e.getErrorCode() == ErrorCode.INVALID_INPUT_VALUE) {
                log.warn("🚫 레시피 아님 판정으로 쿼터 환불 없이 종료: userId={}", userId);
            } else {
                log.warn("❌ 추출 실패(System/AI Error). 쿼터 환불: userId={}", userId);
                dailyQuotaService.refundIfPolicyAllows(userId, QuotaType.YOUTUBE_EXTRACTION);
            }
            throw e;
        } catch (Exception e) {
            log.warn("❌ 알 수 없는 오류. 쿼터 환불: userId={}", userId);
            dailyQuotaService.refundIfPolicyAllows(userId, QuotaType.YOUTUBE_EXTRACTION);
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED);
        }
    }

    @Scheduled(cron = "0 0 4 * * *")
    @Transactional
    public void refreshRecommendedRecipes() {
        if (!isRefreshing.compareAndSet(false, true)) return;

        log.info("🔄 [스케줄러] 타겟 채널 레시피 갱신 및 DB 저장 시작...");

        try {
            List<YoutubeTargetChannel> allChannels = youtubeTargetChannelRepository.findAllByIsActiveTrue();

            if (allChannels.isEmpty()) {
                log.warn("⚠️ 수집할 타겟 채널이 DB에 없습니다.");
                return;
            }

            Collections.shuffle(allChannels);
            List<YoutubeTargetChannel> selectedChannels = allChannels.subList(0, Math.min(allChannels.size(), 6));

            List<YtDlpService.YoutubeSearchDto> rawCandidates = new ArrayList<>();

            int fetchPerChannel = 10;

            for (YoutubeTargetChannel channel : selectedChannels) {
                try {
                    List<YtDlpService.YoutubeSearchDto> results = ytDlpService.getLatestVideosFromChannel(channel.getChannelUrl(), fetchPerChannel);
                    rawCandidates.addAll(results);
                } catch (Exception e) {
                    log.warn("⚠️ 채널 수집 실패 [{}]: {}", channel.getChannelName(), e.getMessage());
                }
            }

            List<YtDlpService.YoutubeSearchDto> keywordFiltered = rawCandidates.stream()
                    .filter(dto -> dto != null && dto.videoId() != null)
                    .filter(dto -> !isNoiseVideo(dto.title()))
                    .distinct()
                    .toList();

            log.info("1차 키워드 필터: {}개 -> {}개", rawCandidates.size(), keywordFiltered.size());

            if (keywordFiltered.isEmpty()) return;

            List<Map<String, String>> aiInput = keywordFiltered.stream()
                    .map(dto -> {
                        Map<String, String> map = new HashMap<>();
                        map.put("id", dto.videoId());
                        map.put("title", dto.title());
                        map.put("channel", dto.channelName());
                        return map;
                    })
                    .toList();

            List<String> validIds = grokClientService.filterRecipeVideos(aiInput).join();

            if (validIds.isEmpty()) {
                log.warn("⚠️ AI 필터링 결과 없음(0건). 키워드 필터링 결과를 그대로 사용합니다.");
                validIds = keywordFiltered.stream()
                        .map(YtDlpService.YoutubeSearchDto::videoId)
                        .toList();
            }

            List<String> finalValidIds = validIds;

            List<YtDlpService.YoutubeSearchDto> finalResults = keywordFiltered.stream()
                    .filter(dto -> finalValidIds.contains(dto.videoId()))
                    .sorted(Comparator.comparingLong(YtDlpService.YoutubeSearchDto::viewCount).reversed())
                    .limit(40)
                    .toList();

            if (!finalResults.isEmpty()) {
                youtubeRecommendationRepository.deleteAll();
                youtubeRecommendationRepository.flush();
                List<YoutubeRecommendation> entities = finalResults.stream()
                        .map(dto -> YoutubeRecommendation.builder()
                                .videoId(dto.videoId())
                                .title(dto.title())
                                .thumbnail(dto.thumbnailUrl())
                                .channelName(dto.channelName())
                                .viewCount(dto.viewCount())
                                .publishedAt("")
                                .collectedAt(LocalDateTime.now())
                                .build())
                        .toList();

                youtubeRecommendationRepository.saveAll(entities);
                log.info("✅ AI 정제 및 저장 완료: {}개 (API 노출 시 랜덤 20개)", entities.size());
            }

        } catch (Exception e) {
            log.error("❌ 갱신 실패", e);
        } finally {
            isRefreshing.set(false);
        }
    }

    public List<YtDlpService.YoutubeSearchDto> getRecommendedRecipes() {
        List<YoutubeRecommendation> entities = youtubeRecommendationRepository.findAll();

        if (!entities.isEmpty()) {
            List<YtDlpService.YoutubeSearchDto> dtos = entities.stream()
                    .map(e -> new YtDlpService.YoutubeSearchDto(
                            e.getTitle(),
                            e.getVideoId(),
                            e.getChannelName(),
                            e.getThumbnail(),
                            e.getViewCount()
                    ))
                    .toList();
            return getRandomizedList(dtos);
        }

        log.info("⚠️ 추천 DB가 비어있습니다. 유튜브 수집을 시작합니다. (빈 목록 반환)");
        CompletableFuture.runAsync(this::refreshRecommendedRecipes);

        return Collections.emptyList();
    }

    private List<YtDlpService.YoutubeSearchDto> getRandomizedList(List<YtDlpService.YoutubeSearchDto> list) {
        if (list.isEmpty()) return Collections.emptyList();

        List<YtDlpService.YoutubeSearchDto> shuffledList = new ArrayList<>(list);
        Collections.shuffle(shuffledList);

        int limit = Math.min(shuffledList.size(), 20);
        return shuffledList.subList(0, limit);
    }

    @Transactional(readOnly = true)
    public Long checkUrlExistence(String videoUrl) {
        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }
        String videoId = extractVideoId(videoUrl);
        if (videoId == null) throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        String canonicalUrl = convertToCanonical(videoId);

        Optional<Recipe> existingRecipe = recipeRepository.findByYoutubeUrl(canonicalUrl);

        return existingRecipe.map(Recipe::getId).orElse(null);
    }

    private CompletableFuture<PresignedUrlResponse> handleExistingRecipe(Recipe recipe) {
        PresignedUrlResponse response = PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(Collections.emptyList())
                .created(false)
                .build();
        return CompletableFuture.completedFuture(response);
    }

    private void addFavoriteToUser(Long userId, Long recipeId) {
        transactionTemplate.executeWithoutResult(status -> {
            recipeFavoriteService.addFavoriteIfNotExists(userId, recipeId);
        });
    }

    private PresignedUrlResponse saveRecipeTransactional(RecipeCreateRequestDto recipeDto, Long userId) {
        return transactionTemplate.execute(status -> {
            RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
            request.setRecipe(recipeDto);

            PresignedUrlResponse originalRes = recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.YOUTUBE,null);

            return PresignedUrlResponse.builder()
                    .recipeId(originalRes.getRecipeId())
                    .uploads(originalRes.getUploads())
                    .created(true)
                    .build();
        });
    }
    private boolean isTextSufficient(String description, String comments, String scriptPlain) {
        String combinedText = (nullToEmpty(description) + " "
                + nullToEmpty(comments) + " "
                + nullToEmpty(scriptPlain)).toLowerCase();

        if (combinedText.length() < 50) return false;

        boolean hasUnit = UNIT_PATTERN.matcher(combinedText).find();
        boolean hasIngredient = INGREDIENT_KEYWORD_PATTERN.matcher(combinedText).find();
        boolean hasAction = STEP_ACTION_PATTERN.matcher(combinedText).find();

        return (hasUnit || hasIngredient) && hasAction;
    }

    private boolean isSpecialQty(String q) {
        return q != null && SPECIAL_QTY.contains(q.trim());
    }

    private Double tryParseNumericQty(String q) {
        if (q == null) return null;
        q = q.trim();
        if (q.isEmpty() || isSpecialQty(q)) return null;

        String clean = q.replaceAll("[^0-9./]", "");
        if (clean.isBlank()) return null;

        try {
            if (clean.contains("/")) {
                String[] parts = clean.split("/");
                if (parts.length != 2) return null;
                double num = Double.parseDouble(parts[0]);
                double den = Double.parseDouble(parts[1]);
                if (den == 0) return null;
                return num / den;
            }
            return Double.parseDouble(clean);
        } catch (Exception e) {
            return null;
        }
    }

    private void mergeDuplicateIngredientsByNameAndUnit(RecipeCreateRequestDto recipeDto) {
        if (recipeDto.getIngredients() == null || recipeDto.getIngredients().isEmpty()) return;

        Map<String, RecipeIngredientRequestDto> merged = new LinkedHashMap<>();

        for (RecipeIngredientRequestDto cur : recipeDto.getIngredients()) {
            if (cur == null || cur.getName() == null) continue;

            String name = cur.getName().trim();
            String unit = cur.getCustomUnit() == null ? "" : cur.getCustomUnit().trim();
            String key = (name + "|" + unit).toLowerCase();

            RecipeIngredientRequestDto exist = merged.get(key);
            if (exist == null) {
                Double q = tryParseNumericQty(cur.getQuantity());
                if (q != null) cur.setQuantity(formatQuantity(q));
                merged.put(key, cur);
                continue;
            }

            boolean existSpecial = isSpecialQty(exist.getQuantity());
            boolean curSpecial   = isSpecialQty(cur.getQuantity());

            Double q1 = tryParseNumericQty(exist.getQuantity());
            Double q2 = tryParseNumericQty(cur.getQuantity());

            if (q1 != null && q2 != null) {
                exist.setQuantity(formatQuantity(q1 + q2));
            } else if (q1 != null) {
            } else if (q2 != null) {
                exist.setQuantity(formatQuantity(q2));
            } else {
                if (existSpecial || curSpecial) {
                    exist.setQuantity("약간");
                    if (exist.getCustomUnit() == null || exist.getCustomUnit().isBlank()) {
                        exist.setCustomUnit("약간");
                    }
                }
            }
        }

        recipeDto.setIngredients(new ArrayList<>(merged.values()));
    }

    private double parseQuantitySafe(String quantity) {
        if (quantity == null || quantity.isBlank()) return 0.0;

        String clean = quantity.replaceAll("[^0-9./]", "");
        if (clean.isBlank()) return 0.0;

        try {
            if (clean.contains("/")) {
                String[] parts = clean.split("/");
                if (parts.length == 2) {
                    double num = Double.parseDouble(parts[0]);
                    double den = Double.parseDouble(parts[1]);
                    if (den == 0) return 0.0;
                    return num / den;
                }
                return 0.0;
            }
            return Double.parseDouble(clean);
        } catch (Exception e) {
            return 0.0;
        }
    }


    private String formatQuantity(double value) {
        double rounded = Math.round(value * 10.0) / 10.0;

        if (rounded == (long) rounded) {
            return String.format("%d", (long) rounded);
        }

        return String.valueOf(rounded);
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

    private String extractVideoId(String url) {
        String pattern = "(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%5C%2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*";

        Pattern compiledPattern = Pattern.compile(pattern);
        Matcher matcher = compiledPattern.matcher(url);

        if (matcher.find()) {
            return matcher.group();
        }
        return null;
    }

    private String convertToCanonical(String videoId) {
        return "https://www.youtube.com/watch?v=" + videoId;
    }

    private boolean isNoiseVideo(String title) {
        if (title == null || title.isBlank()) return true;
        String lowerTitle = title.toLowerCase();

        for (String noise : NOISE_KEYWORDS) {
            if (lowerTitle.contains(noise)) {
                return true;
            }
        }
        return false;
    }

    private boolean isShortsUrl(String url) {
        if (url == null) return false;
        return url.contains("youtube.com/shorts/") || url.contains("/shorts/");
    }

    private String buildStorageYoutubeUrl(String videoId, boolean shorts) {
        if (shorts) return "https://www.youtube.com/shorts/" + videoId;
        return "https://www.youtube.com/watch?v=" + videoId;
    }

}

