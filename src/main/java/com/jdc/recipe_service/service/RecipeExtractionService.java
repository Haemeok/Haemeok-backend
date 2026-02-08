package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeGenerationJob;
import com.jdc.recipe_service.domain.entity.YoutubeRecommendation;
import com.jdc.recipe_service.domain.entity.YoutubeTargetChannel;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeRecommendationRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.media.YtDlpService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.context.request.async.DeferredResult;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.*;
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
    private static final long MAX_VIDEO_DURATION_SECONDS = 70 * 60;

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

    private static final Pattern TIMESTAMP_PATTERN = Pattern.compile(
            "(?i)(\\d{1,2}:\\d{2}|\\d{1,2}\\s?(분|min)|\\d{1,2}\\s?(초|sec))"
    );

    private static final Pattern YOUTUBE_URL_PATTERN = Pattern.compile(
            "(?i)^(https?://)?(www\\.)?(youtube\\.com|youtu\\.be)/.+$"
    );

    private static final Pattern UNIT_PATTERN = Pattern.compile(
            "(?i)(큰술|작은술|밥숟가락|티스푼|종이컵|국자|주걱|꼬집|약간|적당량|" +
                    "spoon|tbs|tbsp|tsp|cup|oz|lb|kg|ml|l|cc|liter|" +
                    "개|마리|모|단|통|알|쪽|줌|봉|봉지|팩|장|copy|ea|" +
                    "\\b[0-9]+/[0-9]+\\b|" +
                    "\\b[0-9.]+\\s?(g|kg|ml|l|cc)\\b)"
    );

    private static final Pattern INGREDIENT_KEYWORD_PATTERN = Pattern.compile(
            "(?i)(재료|ingredient|준비물|필요|양념|소스|드레싱|시즈닝|seasoning|sauce|dressing|materials|shopping list)"
    );

    private static final Pattern STEP_ACTION_PATTERN = Pattern.compile(
            "(?i)(만드는|방법|순서|조리|과정|레시피|recipe|step|direction|how to|" +
                    "넣|볶|끓|굽|튀기|섞|다지|채썰|썰|자르|데치|삶|찌|무치|부치|재우|간하|손질|씻|헹구|" +
                    "chop|mix|boil|fry|stir|bake|roast|grill|simmer|poach|slice|mince|dice)"
    );

    private final YtDlpService ytDlpService;
    private final GrokClientService grokClientService;
    private final GeminiMultimodalService geminiMultimodalService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;
    private final RecipeFavoriteService recipeFavoriteService;
    private final RecipeActivityService recipeActivityService;

    private final RecipeRepository recipeRepository;
    private final RecipeGenerationJobRepository jobRepository;
    private final YoutubeTargetChannelRepository youtubeTargetChannelRepository;
    private final YoutubeRecommendationRepository youtubeRecommendationRepository;

    private final TransactionTemplate transactionTemplate;
    private final Executor extractionExecutor;

    private final AsyncImageService asyncImageService;
    private final ObjectMapper objectMapper;

    private final ConcurrentHashMap<String, CompletableFuture<PresignedUrlResponse>> extractionTasks = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, CompletableFuture<Long>> processingTasks = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, Set<Long>> passengersMap = new ConcurrentHashMap<>();

    private final AtomicBoolean isRefreshing = new AtomicBoolean(false);

    public RecipeExtractionService(
            YtDlpService ytDlpService,
            GrokClientService grokClientService,
            GeminiMultimodalService geminiMultimodalService,
            RecipeService recipeService,
            DailyQuotaService dailyQuotaService, RecipeActivityService recipeActivityService,
            RecipeRepository recipeRepository,
            RecipeFavoriteService recipeFavoriteService, RecipeGenerationJobRepository jobRepository,
            YoutubeTargetChannelRepository youtubeTargetChannelRepository,
            YoutubeRecommendationRepository youtubeRecommendationRepository,
            TransactionTemplate transactionTemplate,
            @Qualifier("recipeExtractionExecutor") Executor extractionExecutor,
            AsyncImageService asyncImageService,
            ObjectMapper objectMapper
    ) {
        this.ytDlpService = ytDlpService;
        this.grokClientService = grokClientService;
        this.geminiMultimodalService = geminiMultimodalService;
        this.recipeService = recipeService;
        this.dailyQuotaService = dailyQuotaService;
        this.recipeActivityService = recipeActivityService;
        this.recipeRepository = recipeRepository;
        this.recipeFavoriteService = recipeFavoriteService;
        this.jobRepository = jobRepository;
        this.youtubeTargetChannelRepository = youtubeTargetChannelRepository;
        this.youtubeRecommendationRepository = youtubeRecommendationRepository;
        this.transactionTemplate = transactionTemplate;
        this.extractionExecutor = extractionExecutor;
        this.asyncImageService = asyncImageService;
        this.objectMapper = objectMapper;
    }

    private String getExtractionPrompt() {
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
            **5. [단위 보존]: 영상에서 '국자', '컵', '개', '봉지', '줌' 등으로 표현된 단위는 무리하게 '큰술'이나 'g'으로 바꾸지 말고 들리는 그대로(예: "1 개", "1 국자") 적어라. (정확한 환산은 다음 단계에서 진행함)**
            6. 소스 분해: 양념장 만드는 장면 있으면 간장/설탕/식초 등 모두 분리
            7. 부재료 포착: 파/깨/참기름/후추 등 조리 중 추가하는 것 누락 금지
            **8. [중요] 총 합계 작성: 조리 과정 중 재료를 여러 번 나눠 넣더라도, ingredients 리스트에는 요리 전체에 사용된 '총 합계량'을 계산하여 적어라. (예: 고기 밑간에 1스푼, 소스에 2스푼을 썼다면 ingredients에는 3스푼으로 기재)**
            
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
            "🏠 홈파티","🌼 피크닉","🏕️ 캠핑","🥗 다이어트 / 건강식","👶 아이와 함께","🍽️ 혼밥","🍶 술안주","🥐 브런치","🌙 야식","⚡ 초스피드 / 간단 요리","🎉 기념일 / 명절","🍱 도시락","🔌 에어프라이어","🍲 해장","👨‍🍳 셰프 레시피"
            
            조건:
            - 🍽️ 혼밥: servings==1일 때만
            - ⚡ 초스피드: cookingTime<=15일 때만
            - 🔌 에어프라이어: cookingTools에 오븐/에어프라이어 포함시만
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

    private PresignedUrlResponse processActualExtractionLogic(String videoUrl, Long userId, String videoId, String nickname) {
        boolean shorts = isShortsUrl(videoUrl);
        String storageUrl = buildStorageYoutubeUrl(videoId, shorts);
        String watchUrl  = buildStorageYoutubeUrl(videoId, false);
        String shortsUrl = buildStorageYoutubeUrl(videoId, true);

        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(watchUrl)
                .or(() -> recipeRepository.findFirstByYoutubeUrl(shortsUrl));

        if (existingRecipe.isPresent()) {
            return handleExistingRecipe(existingRecipe.get()).join();
        }

        boolean usedToken = false;

        try {
            usedToken = dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.YOUTUBE_EXTRACTION);
        } catch (Exception e) {
            throw e;
        }

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
        Long videoViewCount = 0L;
        Long videoDuration = 0L;
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
            videoViewCount = videoData.viewCount();
            videoDuration = videoData.duration();

            String canonicalUrl = nullToEmpty(videoData.canonicalUrl());
            Optional<Recipe> existingRecipeCanonical = recipeRepository.findFirstByYoutubeUrl(canonicalUrl);
            if (existingRecipeCanonical.isPresent()) {
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
                log.info("✅ [텍스트 모드] Step 1: 초안 생성 시작");
                try {
                    recipeDto = grokClientService.generateRecipeStep1(getExtractionPrompt(), fullContext).join();

                    if (isRecipeResultGarbage(recipeDto)) {
                        log.warn("📉 Grok 분석 실패: 결과물 품질 미달 (재료 부족/모호). Gemini로 재시도합니다.");
                        useUrlFallback = true;
                        recipeDto = null;
                    }

                    if (recipeDto != null && Boolean.FALSE.equals(recipeDto.getIsRecipe())) {
                        throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 아님: " + recipeDto.getNonRecipeReason());
                    }
                    if (recipeDto == null || !Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
                        useUrlFallback = true;
                        recipeDto = null;
                    }
                } catch (Exception e) {
                    useUrlFallback = true;
                }
            } else {
                useUrlFallback = true;
            }

            if (useUrlFallback || recipeDto == null) {

                if (videoDuration > MAX_VIDEO_DURATION_SECONDS) {
                    log.warn("🚫 영상 길이 초과 ({}초). 텍스트 정보 부족 및 Gemini 영상 분석 불가로 중단.", videoDuration);
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                            "텍스트 정보가 부족하며, 영상이 너무 길어(20분 초과) AI 심층 분석을 진행할 수 없습니다.");
                }
                log.info("🎥 [멀티모달 모드] Step 1: Gemini 초안 생성 시작");

                String promptWithHint = getExtractionPrompt() + "\n\n" +
                        "## [참고용 텍스트 데이터]\n" +
                        "아래 텍스트는 영상의 설명, 댓글, 자막입니다. " +
                        "영상을 분석할 때 이 내용을 '강력한 힌트'로 참고하되, " +
                        "타임라인(Timeline)은 반드시 영상 화면을 보고 실제 조리 시점을 기준으로 작성하세요.\n\n" +
                        fullContext;

                recipeDto = geminiMultimodalService.generateRecipeFromYoutubeUrl(promptWithHint, title, storageUrl).join();

                if (recipeDto == null || !Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 아님/생성실패");
                }
            }

            logJson("STEP 1: Draft Recipe Created (Before Refinement)", recipeDto);


            log.info("⚡ [병렬 처리 시작] 2단계 재료 정제(Grok) + 이미지 생성(AsyncImage) 동시 실행");

            String refineSystemPrompt = "너는 식재료 데이터 정제 AI다. 창의성을 배제하고 오직 규격 준수에만 집중하라.";
            CompletableFuture<List<RecipeIngredientRequestDto>> ingredientTask =
                    grokClientService.refineIngredientsOnly(refineSystemPrompt, recipeDto.getIngredients());

            CompletableFuture<String> imageTask = asyncImageService.generateImageFromDto(recipeDto, OFFICIAL_RECIPE_USER_ID)
                    .exceptionally(ex -> {
                        log.warn("⚠️ 이미지 생성 실패 (병렬 처리 중): {}", ex.getMessage());
                        return null;
                    });

            CompletableFuture.allOf(ingredientTask, imageTask).join();

            List<RecipeIngredientRequestDto> refinedIngredients = ingredientTask.join();
            String generatedImageUrl = imageTask.join();

            logJson("STEP 2: Refined Ingredients", refinedIngredients);
            if (generatedImageUrl != null && !generatedImageUrl.isBlank()) {
                log.info("🎨 생성된 이미지 URL: {}", generatedImageUrl);

                String s3Key = extractS3Key(generatedImageUrl);
                recipeDto.setImageKey(s3Key);

                recipeDto.setImageStatus(com.jdc.recipe_service.domain.type.RecipeImageStatus.READY);
            }

            recipeDto.setIngredients(refinedIngredients);

            recipeDto.setYoutubeUrl(storageUrl);
            recipeDto.setYoutubeChannelName(channelName);
            recipeDto.setYoutubeChannelId(channelId);
            recipeDto.setYoutubeVideoTitle(originalVideoTitle);
            recipeDto.setYoutubeThumbnailUrl(thumbnailUrl);
            recipeDto.setYoutubeChannelProfileUrl(channelProfileUrl);
            recipeDto.setYoutubeSubscriberCount(subscriberCount);
            recipeDto.setYoutubeVideoViewCount(videoViewCount);

            mergeDuplicateIngredientsByNameAndUnit(recipeDto);

            PresignedUrlResponse response = saveRecipeTransactional(recipeDto, OFFICIAL_RECIPE_USER_ID, userId);

            log.info("💾 신규 생성 및 즐겨찾기 추가 완료: ID={}, OwnerID={}, ExtractorID={}",
                    response.getRecipeId(), OFFICIAL_RECIPE_USER_ID, userId);
            return response;

        } catch (CustomException e) {
            if (e.getErrorCode() == ErrorCode.INVALID_INPUT_VALUE) {
                log.warn("🚫 레시피 아님 판정: {}", e.getMessage());
            } else {
                dailyQuotaService.refund(userId, QuotaType.YOUTUBE_EXTRACTION, usedToken);
            }
            throw e;
        } catch (Exception e) {
            log.warn("❌ 알 수 없는 오류. 쿼터 환불: userId={}", userId);
            dailyQuotaService.refund(userId, QuotaType.YOUTUBE_EXTRACTION, usedToken);
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED);
        }
    }

    public DeferredResult<ResponseEntity<PresignedUrlResponse>> extractAndCreateRecipe(String videoUrl, Long userId, String nickname) {
        String safeVideoUrl = videoUrl != null ? videoUrl.trim() : "";
        log.info("🚀 유튜브 레시피 추출 요청: URL={}, UserID={}", safeVideoUrl, userId);

        if (!YOUTUBE_URL_PATTERN.matcher(safeVideoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }

        String videoId = extractVideoId(safeVideoUrl);
        if (videoId == null) throw new CustomException(ErrorCode.INVALID_URL_FORMAT);

        String canonicalUrl = convertToCanonical(videoId);
        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(canonicalUrl)
                .or(() -> recipeRepository.findFirstByYoutubeUrl(buildStorageYoutubeUrl(videoId, true)))
                .or(() -> recipeRepository.findFirstByYoutubeUrl(buildStorageYoutubeUrl(videoId, false)));

        if (existingRecipe.isPresent()) {
            log.info("♻️ 이미 존재하는 레시피 발견. 생성 건너뜀. ID={}", existingRecipe.get().getId());
            DeferredResult<ResponseEntity<PresignedUrlResponse>> result = new DeferredResult<>();

            PresignedUrlResponse response = PresignedUrlResponse.builder()
                    .recipeId(existingRecipe.get().getId())
                    .uploads(new ArrayList<>())
                    .created(false)
                    .build();
            result.setResult(ResponseEntity.ok(response));
            return result;
        }

        CompletableFuture<PresignedUrlResponse> sharedTask = extractionTasks.computeIfAbsent(videoId, key -> {
            log.info("🚌 [버스 출발] 새로운 추출 작업 시작 (운전자: {}). VideoID: {}", userId, key);

            return CompletableFuture.supplyAsync(() -> {
                try {
                    return processActualExtractionLogic(safeVideoUrl, userId, key, nickname);
                } finally {
                }
            }, extractionExecutor).orTimeout(10, TimeUnit.MINUTES);
        });

        long timeout = 900000L;
        DeferredResult<ResponseEntity<PresignedUrlResponse>> deferredResult = new DeferredResult<>(timeout);

        deferredResult.onTimeout(() -> {
            log.warn("⏳ 추출 요청 타임아웃: UserID={}, VideoID={}", userId, videoId);
            deferredResult.setErrorResult(new CustomException(ErrorCode.TIMEOUT_ERROR));
        });

        sharedTask.whenComplete((response, ex) -> {
            if (extractionTasks.remove(videoId) != null) {
                log.info("🏁 [종점 도착] 작업 종료 및 맵에서 Key 제거 완료: {}", videoId);
            }

            if (ex != null) {
                Throwable cause = ex.getCause() != null ? ex.getCause() : ex;

                if (cause instanceof DailyQuotaService.DailyQuotaExceededException) {
                    log.warn("🚫 쿼터 소진으로 인한 차단: {}", cause.getMessage());
                    deferredResult.setErrorResult(cause);
                    return;
                }

                log.error("❌ 추출 작업 실패: {}", cause.getMessage());
                deferredResult.setErrorResult(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED));
                return;
            }

            try {
                log.info("⭐ 유저 {}에게 레시피 {} 즐겨찾기/로그 추가", userId, response.getRecipeId());
                addFavoriteToUser(userId, response.getRecipeId());
                recipeActivityService.saveLog(userId, nickname, ActivityLogType.YOUTUBE_EXTRACT);
            } catch (Exception e) {
                log.warn("⚠️ 후속 처리 실패: {}", e.getMessage());
            }

            deferredResult.setResult(ResponseEntity.ok(response));
        });

        return deferredResult;
    }

    @Transactional
    public Long createYoutubeExtractionJobV2(String videoUrl, Long userId, String nickname, String idempotencyKey) {
        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }

        Optional<RecipeGenerationJob> existingJob = jobRepository.findByIdempotencyKey(idempotencyKey);
        if (existingJob.isPresent()) {
            log.info("♻️ [Youtube V2] 기존 작업 재사용 - Key: {}, JobID: {}", idempotencyKey, existingJob.get().getId());
            return existingJob.get().getId();
        }

        dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.YOUTUBE_EXTRACTION);

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.YOUTUBE_EXTRACTION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(idempotencyKey)
                .build();

        jobRepository.save(job);
        log.info("🆕 [Youtube V2] 신규 추출 작업 생성 - JobID: {}", job.getId());

        return job.getId();
    }

    /**
     * [Phase 2] 비동기 처리 (백그라운드 실행)
     */
    @Async("recipeExtractionExecutor")
    public void processYoutubeExtractionAsyncV2(Long jobId, String videoUrl, Long userId, String nickname) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        String videoId = extractVideoId(videoUrl);
        if (videoId == null || videoId.isBlank()) {
            handleAsyncError(job, userId, new CustomException(ErrorCode.INVALID_URL_FORMAT));
            return;
        }

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 5);

            passengersMap.computeIfAbsent(videoId, k -> ConcurrentHashMap.newKeySet()).add(jobId);

            CompletableFuture<Long> sharedTask = processingTasks.computeIfAbsent(videoId, key -> {
                log.info("🚌 [버스 출발] 운전사(Job: {})가 AI 작업을 시작합니다. VideoID: {}", jobId, key);

                return CompletableFuture.supplyAsync(() -> {
                    try {
                        PresignedUrlResponse response = processActualExtractionLogicV2(videoUrl, userId, key, nickname, job);
                        return response.getRecipeId();
                    } finally {
                        processingTasks.remove(key);
                        passengersMap.remove(key);
                        log.info("🏁 [종점 도착] 작업 완료. 맵 정리: {}", key);
                    }
                }, extractionExecutor);
            });

            Long resultRecipeId = sharedTask.join();

            completeJobInTransaction(jobId, resultRecipeId);

            addFavoriteToUser(userId, resultRecipeId);

            try {
                recipeActivityService.saveLog(userId, nickname, ActivityLogType.YOUTUBE_EXTRACT);
            } catch (Exception e) {
                log.warn("⚠️ 후속 처리 실패: {}", e.getMessage());
            }

        } catch (Exception e) {
            Throwable cause = (e instanceof CompletionException) ? e.getCause() : e;
            handleAsyncError(job, userId, (Exception) cause);
        } finally {
            Set<Long> passengers = passengersMap.get(videoId);
            if (passengers != null) passengers.remove(jobId);
        }
    }

    private void handleAsyncError(RecipeGenerationJob job, Long userId, Exception e) {
        log.error("❌ [Youtube V2] 추출 실패 JobID: {} - 원인: {}", job.getId(), e.getMessage(), e);

        ErrorCode errorCode = resolveErrorCode(e);
        String clientMsg = resolveClientErrorMessage(e, errorCode);

        job.setErrorMessage(errorCode.getCode() + "::" + clientMsg);
        updateProgress(job, JobStatus.FAILED, 0);

        if (errorCode != ErrorCode.INVALID_INPUT_VALUE) {
            log.info("💸 시스템 오류로 쿼터 환불 (UserID: {})", userId);
            dailyQuotaService.refund(userId, QuotaType.YOUTUBE_EXTRACTION, true);
        } else {
            log.info("🚫 '레시피 아님' 판정이므로 쿼터 환불 X (UserID: {})", userId);
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void broadcastProgress(String videoId, JobStatus status, int progress) {
        Set<Long> passengers = passengersMap.get(videoId);

        if (passengers != null && !passengers.isEmpty()) {
            log.info("📢 [방송] 승객 {}명에게 진행률 {}% 전파 (IDs: {})", passengers.size(), progress, passengers);

            List<RecipeGenerationJob> jobs = jobRepository.findAllById(passengers);
            for (RecipeGenerationJob pJob : jobs) {
                // 이미 끝난 Job은 건드리지 않음
                if (pJob.getStatus() != JobStatus.COMPLETED && pJob.getStatus() != JobStatus.FAILED) {
                    pJob.updateProgress(status, progress);
                }
            }
            jobRepository.saveAll(jobs);
            jobRepository.flush();
        }
    }

    /**
     * [Phase 3] 상태 조회 (Polling)
     */
    @Transactional(readOnly = true)
    public JobStatusDto getJobStatus(Long jobId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        Long recipeId = job.getResultRecipeId();
        if (job.getStatus() == JobStatus.COMPLETED && recipeId != null) {
            if (!recipeRepository.existsById(recipeId)) recipeId = null;
        }

        String fullError = job.getErrorMessage();
        String code = null;
        String msg = fullError;

        if (job.getStatus() == JobStatus.FAILED && fullError != null && fullError.contains("::")) {
            String[] parts = fullError.split("::", 2);
            code = parts[0];
            msg = parts[1];
        }

        return JobStatusDto.builder()
                .jobId(job.getId())
                .status(job.getStatus())
                .resultRecipeId(recipeId)
                .code(code)
                .message(msg)
                .progress(job.getProgress())
                .build();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void updateProgress(RecipeGenerationJob job, JobStatus status, int progress) {
        job.updateProgress(status, progress);
        jobRepository.saveAndFlush(job);
    }

    /**
     * V2 전용 내부 로직: 쿼터 차감 없이 수행 + 중복 시 성공(환불) 처리
     */
    private PresignedUrlResponse processActualExtractionLogicV2(String videoUrl, Long userId, String videoId, String nickname, RecipeGenerationJob job) {
        long startTime = System.currentTimeMillis();
        broadcastProgress(videoId, JobStatus.IN_PROGRESS, 10);

        if (videoId == null || videoId.isBlank() || "null".equals(videoId)) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT, "유튜브 영상 ID를 추출할 수 없습니다.");
        }

        String storageUrl = buildStorageYoutubeUrl(videoId, false);
        String watchUrl  = buildStorageYoutubeUrl(videoId, false);
        String shortsUrl = buildStorageYoutubeUrl(videoId, true);

        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(watchUrl)
                .or(() -> recipeRepository.findFirstByYoutubeUrl(shortsUrl));

        if (existingRecipe.isPresent()) {
            log.info("♻️ [V2] 기존 레시피 발견 (URL). 쿼터 환불 및 즉시 완료.");
            dailyQuotaService.refund(userId, QuotaType.YOUTUBE_EXTRACTION, true);
            return handleExistingRecipe(existingRecipe.get()).join();
        }

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
        Long videoViewCount = 0L;
        Long videoDuration = 0L;
        boolean useUrlFallback = false;

        long metadataStart = System.currentTimeMillis();
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
            videoViewCount = videoData.viewCount();
            videoDuration = videoData.duration();

            String canonicalUrl = nullToEmpty(videoData.canonicalUrl());

            Optional<Recipe> existingRecipeCanonical = recipeRepository.findFirstByYoutubeUrl(canonicalUrl);
            if (existingRecipeCanonical.isPresent()) {
                log.info("♻️ [V2] 기존 레시피 발견 (Canonical). 쿼터 환불 및 즉시 완료.");
                dailyQuotaService.refund(userId, QuotaType.YOUTUBE_EXTRACTION, true);
                return handleExistingRecipe(existingRecipeCanonical.get()).join();
            }
        } catch (Exception e) {
            log.warn("⚠️ yt-dlp 실패 -> Gemini 모드 전환: {}", safeMsg(e));
            useUrlFallback = true;
        }
        long metadataEnd = System.currentTimeMillis();
        log.info("⏱️ [Performance] 영상 메타데이터 추출(yt-dlp) 소요 시간: {}ms", (metadataEnd - metadataStart));

        broadcastProgress(videoId, JobStatus.IN_PROGRESS, 30);

        RecipeCreateRequestDto recipeDto = null;

        long textGenStart = System.currentTimeMillis();
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

            if (!useUrlFallback && isTextSufficient(description, comments, scriptPlain)) {
                log.info("✅ [텍스트 모드] Step 1: 초안 생성 시작");
                try {
                    recipeDto = grokClientService.generateRecipeStep1(getExtractionPrompt(), fullContext).join();

                    if (isRecipeResultGarbage(recipeDto)) {
                        log.warn("📉 Grok 분석 실패: 결과물 품질 미달. Gemini로 재시도.");
                        useUrlFallback = true;
                        recipeDto = null;
                    } else if (recipeDto != null && Boolean.FALSE.equals(recipeDto.getIsRecipe())) {
                        throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 아님: " + recipeDto.getNonRecipeReason());
                    } else if (recipeDto == null || !Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
                        useUrlFallback = true;
                        recipeDto = null;
                    }
                } catch (Exception e) {
                    if (e instanceof CustomException) throw e;
                    useUrlFallback = true;
                }
            } else {
                useUrlFallback = true;
            }

            broadcastProgress(videoId, JobStatus.IN_PROGRESS, 50);

            if (useUrlFallback || recipeDto == null) {
                if (videoDuration > MAX_VIDEO_DURATION_SECONDS) {
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                            "텍스트 정보가 부족하며, 영상이 너무 길어(60분 초과) AI 심층 분석을 진행할 수 없습니다.");
                }
                log.info("🎥 [멀티모달 모드] Step 1: Gemini 초안 생성 시작");

                String promptWithHint = getExtractionPrompt() + "\n\n" +
                        "## [참고용 텍스트 데이터]\n" +
                        "아래 텍스트는 영상의 설명, 댓글, 자막입니다. " +
                        "영상을 분석할 때 이 내용을 '강력한 힌트'로 참고하되, " +
                        "타임라인(Timeline)은 반드시 영상 화면을 보고 실제 조리 시점을 기준으로 작성하세요.\n\n" +
                        fullContext;

                recipeDto = geminiMultimodalService.generateRecipeFromYoutubeUrl(promptWithHint, title, storageUrl).join();

                if (recipeDto == null || !Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 아님/생성실패");
                }
            }
            long textGenEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] AI 텍스트 분석 및 생성 소요 시간: {}ms", (textGenEnd - textGenStart));

            broadcastProgress(videoId, JobStatus.IN_PROGRESS, 60);

            logJson("STEP 1: Draft Recipe Created", recipeDto);
            log.info("⚡ [병렬 처리 시작] 재료 정제 + 이미지 생성");
            long parallelStart = System.currentTimeMillis();

            String refineSystemPrompt = "너는 식재료 데이터 정제 AI다. 창의성을 배제하고 오직 규격 준수에만 집중하라.";
            CompletableFuture<List<RecipeIngredientRequestDto>> ingredientTask =
                    grokClientService.refineIngredientsOnly(refineSystemPrompt, recipeDto.getIngredients());

            CompletableFuture<String> imageTask = asyncImageService.generateImageFromDto(recipeDto, OFFICIAL_RECIPE_USER_ID)
                    .exceptionally(ex -> {
                        log.warn("⚠️ 이미지 생성 실패 (병렬 처리 중): {}", ex.getMessage());
                        return null;
                    });

            CompletableFuture.allOf(ingredientTask, imageTask).join();

            List<RecipeIngredientRequestDto> refinedIngredients = ingredientTask.join();
            String generatedImageUrl = imageTask.join();

            long parallelEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] 병렬 작업(이미지+재료정제) 소요 시간: {}ms", (parallelEnd - parallelStart));
            broadcastProgress(videoId, JobStatus.IN_PROGRESS, 85);

            if (generatedImageUrl != null && !generatedImageUrl.isBlank()) {
                String s3Key = extractS3Key(generatedImageUrl);
                recipeDto.setImageKey(s3Key);
                recipeDto.setImageStatus(com.jdc.recipe_service.domain.type.RecipeImageStatus.READY);
            }

            recipeDto.setIngredients(refinedIngredients);
            recipeDto.setYoutubeUrl(storageUrl);
            recipeDto.setYoutubeChannelName(channelName);
            recipeDto.setYoutubeChannelId(channelId);
            recipeDto.setYoutubeVideoTitle(originalVideoTitle);
            recipeDto.setYoutubeThumbnailUrl(thumbnailUrl);
            recipeDto.setYoutubeChannelProfileUrl(channelProfileUrl);
            recipeDto.setYoutubeSubscriberCount(subscriberCount);
            recipeDto.setYoutubeVideoViewCount(videoViewCount);

            mergeDuplicateIngredientsByNameAndUnit(recipeDto);

            broadcastProgress(videoId, JobStatus.IN_PROGRESS, 90);

            long saveStart = System.currentTimeMillis();
            PresignedUrlResponse response = saveRecipeTransactional(recipeDto, OFFICIAL_RECIPE_USER_ID, userId);
            long saveEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] DB 저장 소요 시간: {}ms", (saveEnd - saveStart));

            log.info("✅ [Performance] 전체 유튜브 추출 총 소요 시간: {}ms", (System.currentTimeMillis() - startTime));
            log.info("💾 [V2] 신규 생성 완료: ID={}, UserID={}", response.getRecipeId(), userId);
            return response;

        } catch (CustomException e) {
            if (e.getErrorCode() == ErrorCode.INVALID_INPUT_VALUE) {
                log.warn("🚫 레시피 아님 판정: {}", e.getMessage());
            }
            throw e;
        } catch (Exception e) {
            log.warn("❌ V2 로직 내부 오류: {}", safeMsg(e));
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

        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(canonicalUrl);

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
        int maxRetries = 5;

        for (int i = 1; i <= maxRetries; i++) {
            try {
                transactionTemplate.executeWithoutResult(status -> {
                    recipeFavoriteService.addFavoriteIfNotExists(userId, recipeId);
                });
                return;

            } catch (Exception e) {
                log.warn("⚠️ 즐겨찾기 추가 충돌(시도 {}/{}): UserID={}, RecipeID={}, Msg={}",
                        i, maxRetries, userId, recipeId, e.getMessage());

                if (i == maxRetries) {
                    log.error("❌ 즐겨찾기 추가 최종 실패: DB 경합이 너무 심함.");
                } else {
                    try {
                        Thread.sleep(100 + (i * 50));
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
        }
    }

    private PresignedUrlResponse saveRecipeTransactional(RecipeCreateRequestDto recipeDto, Long userId, Long extractorId) {
        return transactionTemplate.execute(status -> {
            RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();

            recipeDto.setExtractorId(extractorId);

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

        boolean hasIngredient = INGREDIENT_KEYWORD_PATTERN.matcher(combinedText).find()
                || UNIT_PATTERN.matcher(combinedText).find();
        boolean hasAction = STEP_ACTION_PATTERN.matcher(combinedText).find();

        boolean hasSubtitleData = scriptPlain != null && scriptPlain.length() > 50;

        if (hasIngredient && hasAction) {
            if (hasSubtitleData) return true;

            if (TIMESTAMP_PATTERN.matcher(combinedText).find()) return true;
        }

        return false;
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

    private ErrorCode resolveErrorCode(Exception e) {
        if (e instanceof CustomException ce) return ce.getErrorCode();
        if (e instanceof java.util.concurrent.TimeoutException ||
                (e.getMessage() != null && (e.getMessage().contains("TimeOut") || e.getMessage().contains("504")))) {
            return ErrorCode.INTERNAL_SERVER_ERROR;
        }
        return ErrorCode.INTERNAL_SERVER_ERROR;
    }

    private String resolveClientErrorMessage(Exception e, ErrorCode code) {
        if (e instanceof CustomException) return e.getMessage();
        if (code == ErrorCode.INTERNAL_SERVER_ERROR) return "AI 응답 시간이 초과되었습니다. 잠시 후 다시 시도해 주세요.";
        return "일시적인 시스템 오류가 발생했습니다.";
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
            return matcher.group().trim();
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

    private void logJson(String title, Object object) {
        try {
            String json = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(object);
            log.info("\n==================================================\n" +
                    "👀 {} (Debugging Log)\n" +
                    "--------------------------------------------------\n" +
                    "{}\n" +
                    "==================================================", title, json);
        } catch (Exception e) {
            log.error("Failed to convert object to JSON for logging", e);
        }
    }

    private String extractS3Key(String fullUrl) {
        if (fullUrl == null || fullUrl.isBlank()) return null;

        try {
            java.net.URI uri = new java.net.URI(fullUrl);
            String path = uri.getPath();

            if (path != null && path.startsWith("/")) {
                return path.substring(1);
            }
            return path;

        } catch (Exception e) {
            log.warn("⚠️ URI 파싱 실패, 문자열 처리로 대체: {}", fullUrl);

            int imgIdx = fullUrl.indexOf("images/");
            if (imgIdx != -1) {
                return fullUrl.substring(imgIdx);
            }
            return fullUrl;
        }
    }

    private boolean isRecipeResultGarbage(RecipeCreateRequestDto dto) {
        if (dto == null) return true;
        if (!Boolean.TRUE.equals(dto.getIsRecipe())) return false;

        List<RecipeIngredientRequestDto> ings = dto.getIngredients();

        if (ings == null || ings.size() < 2) return true;

        long badQuantityCount = ings.stream()
                .filter(i -> {
                    String q = i.getQuantity();
                    String u = i.getCustomUnit();
                    return q == null || q.equals("0") || q.contains("약간") ||
                            (u != null && u.contains("약간"));
                })
                .count();

        return (double) badQuantityCount / ings.size() > 0.5;
    }

    private void completeJobInTransaction(Long jobId, Long resultRecipeId) {
        transactionTemplate.executeWithoutResult(status -> {
            RecipeGenerationJob job = jobRepository.findById(jobId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

            job.setResultRecipeId(resultRecipeId);
            job.updateProgress(JobStatus.COMPLETED, 100);

            jobRepository.saveAndFlush(job);
        });
    }
}

