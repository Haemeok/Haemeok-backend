package com.jdc.recipe_service.service.media;

import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.url.YoutubeExtractionResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.credit.CreditCostEntity;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.entity.recipe.RecipeAccess;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.repository.credit.CreditCostRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeInfoRepository;
import com.jdc.recipe_service.domain.repository.recipe.RecipeAccessRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.domain.type.credit.CreditCost;
import com.jdc.recipe_service.domain.type.recipe.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeActivityService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.RecipeImageMatchingService;
import com.jdc.recipe_service.service.user.UserCreditService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@Slf4j
public class YoutubeRecipeExtractionService {

    private static final int DEFAULT_COST_TEXT = 3;
    private static final int DEFAULT_COST_IMAGE = 5;

    private static final int MAX_CONTEXT_CHARS = 100_000;
    private static final int MAX_SCRIPT_CHARS  = 80_000;
    private static final int MAX_DESC_CHARS    = 10_000;
    private static final int MAX_CMT_CHARS     = 1_000;
    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;
    private static final Set<String> SPECIAL_QTY = Set.of("약간");
    private static final long MAX_VIDEO_DURATION_SECONDS = 70 * 60;

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
    private final RecipeActivityService recipeActivityService;
    private final UserCreditService userCreditService;
    private final RecipeImageMatchingService recipeImageMatchingService;

    private final RecipeRepository recipeRepository;
    private final RecipeAccessRepository recipeAccessRepository;
    private final RecipeYoutubeInfoRepository recipeYoutubeInfoRepository;
    private final UserRepository userRepository;
    private final RecipeGenerationJobRepository jobRepository;
    private final CreditCostRepository creditCostRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;

    private final TransactionTemplate transactionTemplate;
    private final Executor recipeExtractionExecutor;
    private final AsyncImageService asyncImageService;

    private final ConcurrentHashMap<String, CompletableFuture<Long>> processingTasks = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Set<Long>> passengersMap = new ConcurrentHashMap<>();

    public YoutubeRecipeExtractionService(
            YtDlpService ytDlpService,
            GrokClientService grokClientService,
            GeminiMultimodalService geminiMultimodalService,
            RecipeService recipeService,
            RecipeActivityService recipeActivityService, UserCreditService userCreditService, RecipeImageMatchingService recipeImageMatchingService,
            RecipeRepository recipeRepository,
            RecipeAccessRepository recipeAccessRepository, RecipeYoutubeInfoRepository recipeYoutubeInfoRepository,
            UserRepository userRepository,
            RecipeGenerationJobRepository jobRepository, CreditCostRepository creditCostRepository, RecipeFavoriteRepository recipeFavoriteRepository,
            TransactionTemplate transactionTemplate,
            @Qualifier("recipeExtractionExecutor") Executor recipeExtractionExecutor,
            AsyncImageService asyncImageService
    ) {
        this.ytDlpService = ytDlpService;
        this.grokClientService = grokClientService;
        this.geminiMultimodalService = geminiMultimodalService;
        this.recipeService = recipeService;
        this.recipeActivityService = recipeActivityService;
        this.userCreditService = userCreditService;
        this.recipeImageMatchingService = recipeImageMatchingService;
        this.recipeRepository = recipeRepository;
        this.recipeAccessRepository = recipeAccessRepository;
        this.recipeYoutubeInfoRepository = recipeYoutubeInfoRepository;
        this.userRepository = userRepository;
        this.jobRepository = jobRepository;
        this.creditCostRepository = creditCostRepository;
        this.recipeFavoriteRepository = recipeFavoriteRepository;
        this.transactionTemplate = transactionTemplate;
        this.recipeExtractionExecutor = recipeExtractionExecutor;
        this.asyncImageService = asyncImageService;
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
              "cookingTips": "문장으로 3-5개 팁 (불릿 금지)",
              "imageMatchKeywords": ["샘플요리이름", "간략샘플요리이름"]
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
            
            **imageMatchKeywords:**
                - 목적: 네가 생성한 "title" 필드의 값에서 기존 요리 사진을 검색하기 위한 핵심 단어 2개를 추출하여 배열에 담습니다.
                - [1] 수식어 완벽 제거: 조리시간(5분), 인명/브랜드(업소용), 감성어/형용사(초간단, 맛있는, 매콤) 등 요리의 본질과 무관한 단어는 100% 제거합니다.
                - [2] ★핵심 재료 보존 원칙★: 키워드에는 반드시 '요리의 정체성이 되는 재료명'이 포함되어야 합니다. "찌개", "조림", "볶음", "무침", "덮밥" 같은 광범위한 요리 형태 단어만 단독으로 추출하는 것을 절대 금지합니다.
                - [3] 2단어 추출 전략:
                    - 1순위: [모든 핵심 재료 + 조리법]이 포함된 정확한 메뉴명
                    - 2순위: 서브 재료를 하나 빼거나 가장 메인이 되는 [핵심 주재료 + 조리법] 조합 (만약 뺄 서브 재료가 없다면, 요리의 핵심 식재료 1개만 단독 명사로 적습니다.)
                - [추출 예시 (Few-Shot)]
                    - 입력: "10분 완성 초간단 두부계란덮밥" -> 출력: ["두부계란덮밥", "두부덮밥"]
                    - 입력: "업소용 고등어 무조림" -> 출력: ["고등어무조림", "고등어조림"]
                    - 입력: "깊고 고소한 삼겹살 김치찌개" -> 출력: ["삼겹살김치찌개", "김치찌개"]
                    - 입력: "5분 초간단 참치마요 깻잎쌈" -> 출력: ["참치마요깻잎쌈", "깻잎쌈"]
                    - 입력: "과메기 굴 우럭 돼지 보쌈" -> 출력: ["과메기보쌈", "보쌈"]
            
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

    @Transactional
    public Long createYoutubeExtractionJob(String videoUrl, Long userId, String nickname, String idempotencyKey, RecipeDisplayMode mode) {
        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }

        Optional<RecipeGenerationJob> existingJob = jobRepository.findByIdempotencyKey(idempotencyKey);
        if (existingJob.isPresent()) {
            log.info("♻️ [Youtube V2] 기존 작업 재사용 - Key: {}, JobID: {}", idempotencyKey, existingJob.get().getId());
            return existingJob.get().getId();
        }

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.YOUTUBE_EXTRACTION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(idempotencyKey)
                .displayMode(mode)
                .build();
        jobRepository.save(job);

        CreditCost costType;
        if (mode == RecipeDisplayMode.IMAGE_MODE) {
            costType = CreditCost.YOUTUBE_SUMMARY_IMAGE;
        } else {
            costType = CreditCost.YOUTUBE_SUMMARY_TEXT;
        }

        int cost = getCostFromDb(costType);
        String refType = costType.name();

        userCreditService.useCredit(userId, cost, refType, job.getId(), idempotencyKey);

        log.info("🆕 [Youtube V2] 작업 생성 - JobID: {}, Cost: {}, Mode: {}", job.getId(), cost, mode);

        return job.getId();
    }

    @Async("recipeExtractionExecutor")
    public void processYoutubeExtractionAsync(Long jobId, String videoUrl, Long userId, String nickname, RecipeDisplayMode mode) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        String videoId = extractVideoId(videoUrl);
        if (videoId == null || videoId.isBlank()) {
            handleAsyncError(job, videoUrl, userId, new CustomException(ErrorCode.INVALID_URL_FORMAT), mode);
            return;
        }

        String busKey = videoId + "_" + mode.name();

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 5);
            passengersMap.computeIfAbsent(busKey, k -> ConcurrentHashMap.newKeySet()).add(jobId);

            CompletableFuture<Long> sharedTask = processingTasks.computeIfAbsent(busKey, key -> {
                log.info("🚌 [버스 출발] 운전사(Job: {})가 작업 시작. Key: {}", jobId, key);
                return CompletableFuture.supplyAsync(() -> {
                    try {
                        PresignedUrlResponse response = processActualExtractionLogic(videoUrl, userId, videoId, nickname, job, mode);
                        return response.getRecipeId();
                    } finally {
                        processingTasks.remove(key);
                        passengersMap.remove(key);
                        log.info("🏁 [종점 도착] 작업 완료. 맵 정리: {}", key);
                    }
                }, recipeExtractionExecutor);
            });

            Long resultRecipeId = sharedTask.join();
            completeJobInTransaction(jobId, resultRecipeId);

            boolean isPremiumRecipe = recipeRepository.findById(resultRecipeId)
                    .map(r -> r.getVisibility() == RecipeVisibility.PUBLIC && r.getListingStatus() == RecipeListingStatus.LISTED)
                    .orElse(false);

            registerRecipeToUser(userId, resultRecipeId, mode, isPremiumRecipe);

            Set<Long> passengers = passengersMap.get(busKey);
            if (passengers != null) {
                passengers.remove(jobId);
                processPassengers(busKey, resultRecipeId);
            }

            try {
                recipeActivityService.saveLog(userId, nickname, ActivityLogType.YOUTUBE_EXTRACT);
            } catch (Exception e) {
                log.warn("⚠️ 로그 저장 실패: {}", e.getMessage());
            }

        } catch (Exception e) {
            Throwable cause = (e instanceof CompletionException) ? e.getCause() : e;
            handleAsyncError(job, videoUrl, userId, (Exception) cause, mode);
        } finally {
            Set<Long> passengers = passengersMap.get(busKey);
            if (passengers != null) passengers.remove(jobId);
        }
    }

    private PresignedUrlResponse processActualExtractionLogic(String videoUrl, Long userId, String videoId, String nickname, RecipeGenerationJob job, RecipeDisplayMode mode) {
        long startTime = System.currentTimeMillis();

        String busKey = videoId + "_" + mode.name();
        broadcastProgress(busKey, JobStatus.IN_PROGRESS, 10);

        String watchUrl = buildStorageYoutubeUrl(videoId, false);
        String shortsUrl = buildStorageYoutubeUrl(videoId, true);

        Optional<Recipe> existingRecipe = recipeRepository.findFirstByYoutubeUrl(watchUrl)
                .or(() -> recipeRepository.findFirstByYoutubeUrl(shortsUrl));

        if (existingRecipe.isPresent()) {
            Recipe r = existingRecipe.get();
            boolean isPremiumRecipe = (r.getVisibility() == RecipeVisibility.PUBLIC && r.getListingStatus() == RecipeListingStatus.LISTED);

            if (mode == RecipeDisplayMode.TEXT_MODE) {
                if (isPremiumRecipe) {
                    log.info("텍스트 요청인데 이미 이미지 레시피 있음 -> 환불 후 즉시 반환");
                    int cost = getCostFromDb(CreditCost.YOUTUBE_SUMMARY_TEXT);
                    userCreditService.refundCredit(userId, cost, "이미지 레시피 존재로 인한 무료 제공", "EXISTING_RECIPE", r.getId());
                    return handleExistingRecipe(r, job.getId(), userId, true, false).join();
                } else {
                    log.info("♻️ 기존 텍스트 레시피 재사용 -> 연산 없이 즉시 반환 (토큰 소모)");
                    return handleExistingRecipe(r, job.getId(), userId, false, true).join();
                }
            }

            if (mode == RecipeDisplayMode.IMAGE_MODE) {
                if (isPremiumRecipe && !r.getIsPrivate()) {
                    log.info("♻️ [이미지 모드] 기존 이미지 레시피 재사용 -> 환불 후 반환");
                    int cost = getCostFromDb(CreditCost.YOUTUBE_SUMMARY_IMAGE);
                    userCreditService.refundCredit(userId, cost, "기존 이미지 레시피 재사용 환불", "EXISTING_RECIPE", r.getId());
                    return handleExistingRecipe(r, job.getId(), userId, true,false).join();
                }
                log.info("🆕 [신규 생성] 기존 텍스트 레시피가 있지만 이미지 모드이므로 새로 생성 진행");
            }
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

        YtDlpService.YoutubeFullDataDto videoData = null;

        try {
            videoData = ytDlpService.getVideoDataFull(videoUrl);
        } catch (Exception e) {
            log.warn("⚠️ yt-dlp 데이터 추출 에러: {}", safeMsg(e));
            videoData = YtDlpService.YoutubeFullDataDto.builder().build();
            useUrlFallback = true;
        }

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

        try {
            String canonicalUrl = nullToEmpty(videoData.canonicalUrl());
            Optional<Recipe> existingRecipeCanonical = recipeRepository.findFirstByYoutubeUrl(canonicalUrl);
            if (existingRecipeCanonical.isPresent()) {
                Recipe r = existingRecipeCanonical.get();
                boolean isPremiumRecipe = (r.getVisibility() == RecipeVisibility.PUBLIC && r.getListingStatus() == RecipeListingStatus.LISTED);

                if (mode == RecipeDisplayMode.TEXT_MODE) {
                    if (isPremiumRecipe) {
                        log.info("Canonical 중복 - 이미지 레시피 존재로 환불");
                        int cost = getCostFromDb(CreditCost.YOUTUBE_SUMMARY_TEXT);
                        userCreditService.refundCredit(userId, cost, "이미지 레시피 존재로 인한 무료 제공", "EXISTING_RECIPE", r.getId());
                        return handleExistingRecipe(r, job.getId(), userId, true, false).join();
                    } else {
                        log.info("♻️Canonical 중복 - 텍스트 레시피 존재로 연산 생략 (토큰 소모)");
                        return handleExistingRecipe(r, job.getId(), userId, false, true).join();
                    }
                }

                if (mode == RecipeDisplayMode.IMAGE_MODE) {
                    if (isPremiumRecipe && !r.getIsPrivate()) {
                        log.info("♻️ [이미지 모드] Canonical 중복 - 이미지 레시피 존재로 환불");
                        int cost = getCostFromDb(CreditCost.YOUTUBE_SUMMARY_IMAGE);
                        userCreditService.refundCredit(userId, cost, "기존 이미지 레시피 재사용 환불", "EXISTING_RECIPE", r.getId());
                        return handleExistingRecipe(r, job.getId(), userId, true,false).join();
                    }
                }
            }

        } catch (Exception e) {
            log.warn("⚠️ yt-dlp 실패 -> Gemini 모드 전환: {}", safeMsg(e));
            videoData = YtDlpService.YoutubeFullDataDto.builder().build();
            useUrlFallback = true;
        }

        broadcastProgress(busKey, JobStatus.IN_PROGRESS, 30);
        RecipeCreateRequestDto recipeDto = null;

        String fullContext = cap(("""
        영상 URL: %s
        영상 제목: %s
        영상 설명: %s
        고정/인기 댓글: %s
        자막: %s
        """).formatted(
                buildStorageYoutubeUrl(videoId, false),
                title,
                emptyToPlaceholder(description, "(없음)"),
                emptyToPlaceholder(comments, "(없음)"),
                emptyToPlaceholder(scriptPlain, "(없음)")
        ), MAX_CONTEXT_CHARS);

        if (!useUrlFallback && isTextSufficient(description, comments, scriptPlain)) {
            log.info("✅ [텍스트 모드] Step 1: Grok 초안 생성");
            try {
                recipeDto = grokClientService.generateRecipeStep1(getExtractionPrompt(), fullContext).join();

                if (isRecipeResultGarbage(recipeDto)) {
                    log.warn("📉 Grok 품질 미달 -> Fallback");
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

        broadcastProgress(busKey, JobStatus.IN_PROGRESS, 50);

        if (useUrlFallback || recipeDto == null) {
            if (videoDuration > MAX_VIDEO_DURATION_SECONDS) {
                log.warn("🚫 영상 길이 초과 ({}초). 분석 불가.", videoDuration);
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                        "텍스트 정보가 부족하며, 영상이 너무 길어(70분 초과) AI 심층 분석을 진행할 수 없습니다.");
            }

            log.info("🎥 [멀티모달 모드] Step 1: Gemini 초안 생성");
            String promptWithHint = getExtractionPrompt() + "\n\n## [참고용 텍스트 데이터]\n" + fullContext;
            recipeDto = geminiMultimodalService.generateRecipeFromYoutubeUrl(promptWithHint, title, watchUrl).join();
        }

        if (recipeDto == null || !Boolean.TRUE.equals(recipeDto.getIsRecipe())) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피를 추출할 수 없습니다.");
        }

        broadcastProgress(busKey, JobStatus.IN_PROGRESS, 60);

        log.info("⚡ [병렬 처리] 모드: {}", mode);

        CompletableFuture<List<RecipeIngredientRequestDto>> ingredientTask =
                grokClientService.refineIngredientsOnly("식재료 데이터 정제 AI", recipeDto.getIngredients());

        CompletableFuture<String> imageTask;

        if (mode == RecipeDisplayMode.IMAGE_MODE) {
            imageTask = asyncImageService.generateImageFromDto(recipeDto, OFFICIAL_RECIPE_USER_ID)
                    .exceptionally(ex -> {
                        log.warn("⚠️ 이미지 생성 실패: {}", ex.getMessage());
                        return null;
                    });
        } else {
            imageTask = CompletableFuture.completedFuture(null);
        }

        CompletableFuture.allOf(ingredientTask, imageTask).join();

        List<RecipeIngredientRequestDto> refinedIngredients = ingredientTask.join();
        String generatedImageUrl = imageTask.join();

        recipeDto.setIngredients(refinedIngredients);
        mergeDuplicateIngredientsByNameAndUnit(recipeDto);

        // TODO: 나중에 유튜브 데이터가 엔티티 이동시 삭제
        recipeDto.setYoutubeUrl(watchUrl);
        recipeDto.setYoutubeChannelName(channelName);
        recipeDto.setYoutubeChannelId(channelId);
        recipeDto.setYoutubeVideoTitle(originalVideoTitle);
        recipeDto.setYoutubeThumbnailUrl(thumbnailUrl);
        recipeDto.setYoutubeChannelProfileUrl(channelProfileUrl);
        recipeDto.setYoutubeSubscriberCount(subscriberCount);
        recipeDto.setYoutubeVideoViewCount(videoViewCount);
        // TODO: 나중에 유튜브 데이터가 엔티티 이동시 삭제

        if (generatedImageUrl != null && !generatedImageUrl.isBlank()) {
            log.info("🎨 이미지 생성 완료 -> PUBLIC / LISTED");
            recipeDto.setImageKey(extractS3Key(generatedImageUrl));
            recipeDto.setImageStatus(RecipeImageStatus.READY);

            recipeDto.setVisibility(RecipeVisibility.PUBLIC);
            recipeDto.setListingStatus(RecipeListingStatus.LISTED);
        } else {
            log.info("📝 텍스트 모드 유지 -> RESTRICTED / UNLISTED 모드");

            // TODO: 여기서 [키워드 검색 이미지 / 기본 이미지] 매칭 로직이 들어갈 자리입니다.
            DishType currentDishType = DishType.fromDisplayName(recipeDto.getDishType());

            String matchedImageKey = recipeImageMatchingService.findMatchingImageKey(
                    recipeDto.getImageMatchKeywords(), currentDishType);

            if (matchedImageKey != null) {
                log.info("✨ [이미지 매칭] 기존 썸네일 획득 성공: {}", matchedImageKey);
                recipeDto.setImageKey(matchedImageKey);
            } else {
                String defaultCategoryImage = recipeImageMatchingService.getDefaultImageKeyForDishType(
                        recipeDto.getDishType());
                log.info("⚠️ [매칭 실패] 카테고리별 기본 이미지를 적용합니다: {}", defaultCategoryImage);
                recipeDto.setImageKey(defaultCategoryImage);
            }

            recipeDto.setImageStatus(RecipeImageStatus.READY);
            recipeDto.setVisibility(RecipeVisibility.RESTRICTED);
            recipeDto.setListingStatus(RecipeListingStatus.UNLISTED);
        }
        recipeDto.setLifecycleStatus(RecipeLifecycleStatus.ACTIVE);

        broadcastProgress(busKey, JobStatus.IN_PROGRESS, 90);

        PresignedUrlResponse response = saveRecipeTransactional(recipeDto, OFFICIAL_RECIPE_USER_ID, userId);

        saveYoutubeInfo(response.getRecipeId(), videoData, videoId, userId);

        log.info("✅ 전체 추출 완료: {}ms. RecipeID: {}", (System.currentTimeMillis() - startTime), response.getRecipeId());
        return response;
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    protected void grantRecipeOwnership(Long userId, Long recipeId, RecipeAccessRole role) {
        if (recipeAccessRepository.existsByUserIdAndRecipeId(userId, recipeId)) {
            return;
        }
        try {
            User user = userRepository.getReferenceById(userId);
            Recipe recipe = recipeRepository.getReferenceById(recipeId);

            RecipeAccess access = RecipeAccess.builder()
                    .user(user)
                    .recipe(recipe)
                    .role(role)
                    .build();

            recipeAccessRepository.save(access);
            log.info("🔐 권한 부여 완료: User={}, Recipe={}, Role={}", userId, recipeId, role);

        } catch (Exception e) {
            log.warn("⚠️ 권한 부여 실패 (중복 등): {}", e.getMessage());
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    protected void saveYoutubeInfo(Long recipeId, YtDlpService.YoutubeFullDataDto videoData,String videoId, Long userId) {
        try {
            if (recipeYoutubeInfoRepository.existsByRecipeId(recipeId)) {
                return;
            }
            Recipe recipe = recipeRepository.getReferenceById(recipeId);

            RecipeYoutubeInfo info = RecipeYoutubeInfo.builder()
                    .recipe(recipe)
                    .videoId(videoId)
                    .youtubeUrl(nullToEmpty(videoData.canonicalUrl()))
                    .channelName(nullToEmpty(videoData.channelName()))
                    .channelId(nullToEmpty(videoData.channelId()))
                    .videoTitle(nullToEmpty(videoData.title()))
                    .thumbnailUrl(nullToEmpty(videoData.thumbnailUrl()))
                    .channelProfileUrl(nullToEmpty(videoData.channelProfileUrl()))
                    .subscriberCount(videoData.youtubeSubscriberCount())
                    .videoViewCount(videoData.viewCount())
                    .extractorId(userId)
                    .build();

            recipeYoutubeInfoRepository.save(info);
            log.info("📹 Youtube Info 저장 완료: RecipeID={}", recipeId);
        } catch (Exception e) {
            log.warn("⚠️ Youtube Info 저장 실패 (RecipeID={}): {}", recipeId, e.getMessage());
        }
    }

    private PresignedUrlResponse saveRecipeTransactional(RecipeCreateRequestDto recipeDto, Long userId, Long extractorId) {
        return transactionTemplate.execute(status -> {
            RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
            recipeDto.setExtractorId(extractorId);
            request.setRecipe(recipeDto);

            PresignedUrlResponse originalRes = recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.YOUTUBE, null);

            return PresignedUrlResponse.builder()
                    .recipeId(originalRes.getRecipeId())
                    .uploads(originalRes.getUploads())
                    .created(true)
                    .build();
        });
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

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void broadcastProgress(String busKey, JobStatus status, int progress) {
        Set<Long> passengers = passengersMap.get(busKey);

        if (passengers != null && !passengers.isEmpty()) {
            List<RecipeGenerationJob> jobs = jobRepository.findAllById(passengers);
            for (RecipeGenerationJob pJob : jobs) {
                if (pJob.getStatus() != JobStatus.COMPLETED && pJob.getStatus() != JobStatus.FAILED) {
                    pJob.updateProgress(status, progress);
                }
            }
            jobRepository.saveAll(jobs);
        }
    }

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

    private int getCostFromDb(CreditCost costType) {
        return creditCostRepository.findByCode(costType.name())
                .map(CreditCostEntity::getCost)
                .orElseGet(() -> {
                    log.error("⚠️ DB에 CreditCost({}) 정보가 없습니다! 기본값을 사용합니다.", costType);
                    return (costType == CreditCost.YOUTUBE_SUMMARY_IMAGE) ? DEFAULT_COST_IMAGE : DEFAULT_COST_TEXT;
                });
    }

    private void handleAsyncError(RecipeGenerationJob job, String videoUrl, Long userId, Exception e, RecipeDisplayMode mode) {
        log.error("❌ 추출 실패 JobID: {} - {}", job.getId(), e.getMessage());

        ErrorCode errorCode = (e instanceof CustomException ce) ? ce.getErrorCode() : ErrorCode.INTERNAL_SERVER_ERROR;
        String clientMsg = (e instanceof CustomException) ? e.getMessage() : "일시적인 시스템 오류가 발생했습니다.";

        job.setErrorMessage(errorCode.getCode() + "::" + clientMsg);
        updateProgress(job, JobStatus.FAILED, 0);

        String videoId = extractVideoId(videoUrl);
        if (videoId != null) {
            String busKey = videoId + "_" + mode.name();
            broadcastProgress(busKey, JobStatus.FAILED, 0);
        }

        if (errorCode != ErrorCode.INVALID_INPUT_VALUE) {
            CreditCost costType = (mode == RecipeDisplayMode.IMAGE_MODE) ? CreditCost.YOUTUBE_SUMMARY_IMAGE : CreditCost.YOUTUBE_SUMMARY_TEXT;
            int refundAmount = getCostFromDb(costType);
            log.info("💸 시스템 오류로 크레딧 환불 (UserID: {}, Amount: {}, JobID: {})", userId, refundAmount, job.getId());
            String refundReason = "유튜브 추출 실패 환불 (Job ID: " + job.getId() + ")";
            userCreditService.refundCredit(userId, refundAmount, refundReason, "YOUTUBE_EXTRACT", job.getId());
        }
    }

    private CompletableFuture<PresignedUrlResponse> handleExistingRecipe(
            Recipe recipe, Long jobId, Long userId, boolean isRefunded, boolean shouldGrantAccess) {

        completeJobInTransaction(jobId, recipe.getId());

        if (shouldGrantAccess) {
            grantRecipeOwnership(userId, recipe.getId(), RecipeAccessRole.VIEWER);
            log.info("🔐 [권한 기록] 유저 {}에게 레시피 {} 조회 권한 부여", userId, recipe.getId());
        }

        autoAddFavorite(userId, recipe.getId());

        PresignedUrlResponse response = YoutubeExtractionResponse.youtubeBuilder()
                .recipeId(recipe.getId())
                .uploads(Collections.emptyList())
                .created(false)
                .refunded(isRefunded)
                .build();

        return CompletableFuture.completedFuture(response);
    }

    private void registerRecipeToUser(Long userId, Long recipeId, RecipeDisplayMode mode, boolean isPremiumRecipe) {
        if (userId == null || recipeId == null) return;

        if (mode == RecipeDisplayMode.TEXT_MODE && !isPremiumRecipe) {
            grantRecipeOwnership(userId, recipeId, RecipeAccessRole.VIEWER);
        }

        autoAddFavorite(userId, recipeId);
    }

    private void processPassengers(String busKey, Long recipeId) {
        Set<Long> passengers = passengersMap.get(busKey);
        if (passengers != null && !passengers.isEmpty()) {
            List<RecipeGenerationJob> jobs = jobRepository.findAllById(passengers);

            Recipe recipe = recipeRepository.findById(recipeId).orElse(null);
            // 여기서도 PUBLIC & LISTED 검사!
            boolean isPremiumRecipe = (recipe != null && recipe.getVisibility() == RecipeVisibility.PUBLIC && recipe.getListingStatus() == RecipeListingStatus.LISTED);

            for (RecipeGenerationJob pJob : jobs) {
                if (pJob.getStatus() != JobStatus.COMPLETED) {
                    pJob.setResultRecipeId(recipeId);
                    pJob.updateProgress(JobStatus.COMPLETED, 100);
                }

                if (pJob.getDisplayMode() == RecipeDisplayMode.TEXT_MODE && isPremiumRecipe) {
                    int cost = getCostFromDb(CreditCost.YOUTUBE_SUMMARY_TEXT);
                    userCreditService.refundCredit(pJob.getUserId(), cost, "이미지 레시피 무료 제공 (합승 환불)", "EXISTING_RECIPE", recipeId);
                }
                else if (pJob.getDisplayMode() == RecipeDisplayMode.TEXT_MODE && !isPremiumRecipe) {
                    grantRecipeOwnership(pJob.getUserId(), recipeId, RecipeAccessRole.VIEWER);
                }

                autoAddFavorite(pJob.getUserId(), recipeId);
            }
            jobRepository.saveAll(jobs);
        }
    }

    private boolean isTextSufficient(String description, String comments, String scriptPlain) {
        String combinedText = (nullToEmpty(description) + " " + nullToEmpty(comments) + " " + nullToEmpty(scriptPlain)).toLowerCase();
        if (combinedText.length() < 50) return false;
        boolean hasIngredient = INGREDIENT_KEYWORD_PATTERN.matcher(combinedText).find() || UNIT_PATTERN.matcher(combinedText).find();
        boolean hasAction = STEP_ACTION_PATTERN.matcher(combinedText).find();
        boolean hasSubtitleData = scriptPlain != null && scriptPlain.length() > 50;

        if (hasIngredient && hasAction) {
            if (hasSubtitleData) return true;
            if (TIMESTAMP_PATTERN.matcher(combinedText).find()) return true;
        }
        return false;
    }

    private boolean isRecipeResultGarbage(RecipeCreateRequestDto dto) {
        if (dto == null || !Boolean.TRUE.equals(dto.getIsRecipe())) return false;
        List<RecipeIngredientRequestDto> ings = dto.getIngredients();
        if (ings == null || ings.size() < 2) return true;
        long badQuantityCount = ings.stream().filter(i -> {
            String q = i.getQuantity();
            String u = i.getCustomUnit();
            return q == null || q.equals("0") || q.contains("약간") || (u != null && u.contains("약간"));
        }).count();
        return (double) badQuantityCount / ings.size() > 0.5;
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

    private void autoAddFavorite(Long userId, Long recipeId) {
        if (userId == null || recipeId == null) return;

        boolean exists = recipeFavoriteRepository.existsByUserIdAndRecipeId(userId, recipeId);
        if (!exists) {
            RecipeFavorite favorite = RecipeFavorite.builder()
                    .user(userRepository.getReferenceById(userId))
                    .recipe(recipeRepository.getReferenceById(recipeId))
                    .build();
            recipeFavoriteRepository.save(favorite);
            log.info("⭐ 즐겨찾기 자동 추가 완료: User={}, Recipe={}", userId, recipeId);
        }
    }

    private String emptyToPlaceholder(String s, String placeholder) {
        return (s == null || s.isBlank()) ? placeholder : s;
    }

    private String extractVideoId(String url) {
        Matcher matcher = Pattern.compile("(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%5C%2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*").matcher(url);
        return matcher.find() ? matcher.group().trim() : null;
    }

    private String buildStorageYoutubeUrl(String videoId, boolean shorts) {
        return shorts ? "https://www.youtube.com/shorts/" + videoId : "https://www.youtube.com/watch?v=" + videoId;
    }

    private String extractS3Key(String fullUrl) {
        try {
            java.net.URI uri = new java.net.URI(fullUrl);
            String path = uri.getPath();
            return (path != null && path.startsWith("/")) ? path.substring(1) : path;
        } catch (Exception e) {
            int imgIdx = fullUrl.indexOf("images/");
            return (imgIdx != -1) ? fullUrl.substring(imgIdx) : fullUrl;
        }
    }

    private String nullToEmpty(String s) { return s == null ? "" : s; }
    private String cap(String s, int max) { return (s != null && s.length() > max) ? s.substring(0, max) : nullToEmpty(s); }
    private String safeMsg(Throwable t) { return t == null ? "" : (t.getMessage() != null ? t.getMessage() : t.getClass().getSimpleName()); }

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

    private String formatQuantity(double value) {
        double rounded = Math.round(value * 10.0) / 10.0;
        if (rounded == (long) rounded) {
            return String.format("%d", (long) rounded);
        }
        return String.valueOf(rounded);
    }
}