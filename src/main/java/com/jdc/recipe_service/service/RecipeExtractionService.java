package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.YoutubeTargetChannel;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.YoutubeTargetChannelRepository;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.media.YtDlpService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@Slf4j
public class RecipeExtractionService {

    private static final int MAX_CONTEXT_CHARS = 100_000;
    private static final int MAX_SCRIPT_CHARS  = 80_000;
    private static final int MAX_DESC_CHARS    = 10_000;
    private static final int MAX_CMT_CHARS     = 1_000;

    private final YtDlpService ytDlpService;
    private final GrokClientService grokClientService;
    private final GeminiMultimodalService geminiMultimodalService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;
    private final RecipeFavoriteService recipeFavoriteService;


    private final RecipeRepository recipeRepository;
    private final YoutubeTargetChannelRepository youtubeTargetChannelRepository;

    private final TransactionTemplate transactionTemplate;

    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;

    private final AtomicReference<List<YtDlpService.YoutubeSearchDto>> cachedRecommendations
            = new AtomicReference<>(Collections.emptyList());

    private final AtomicBoolean isRefreshing = new AtomicBoolean(false);
    
    private static final List<String> NOISE_KEYWORDS = List.of(
            // 1. 기존 먹방/브이로그
            "먹방", "mukbang", "asmr", "이팅사운드",
            "리뷰", "후기", "탐방", "review", "맛집", "맛있게 먹는",
            "브이로그", "vlog", "일상", "grwm", "what i eat",
            "식단일기", "장보기", "haul", "하울", "언박싱",
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

    public RecipeExtractionService(
            YtDlpService ytDlpService,
            GrokClientService grokClientService,
            GeminiMultimodalService geminiMultimodalService,
            RecipeService recipeService,
            DailyQuotaService dailyQuotaService,
            RecipeRepository recipeRepository,
            RecipeFavoriteService recipeFavoriteService, YoutubeTargetChannelRepository youtubeTargetChannelRepository,
            TransactionTemplate transactionTemplate
    ) {
        this.ytDlpService = ytDlpService;
        this.grokClientService = grokClientService;
        this.geminiMultimodalService = geminiMultimodalService;
        this.recipeService = recipeService;
        this.dailyQuotaService = dailyQuotaService;
        this.recipeRepository = recipeRepository;
        this.recipeFavoriteService = recipeFavoriteService;
        this.youtubeTargetChannelRepository = youtubeTargetChannelRepository;
        this.transactionTemplate = transactionTemplate;
    }

    private String getExtractionPrompt() {
        return """
            [SYSTEM]
            너는 요리 전문가 AI다. 출력은 반드시 "단 하나의 JSON 객체"만 허용한다.
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
            - 광고/링크/쿠폰/잡담은 근거에서 제외
            - 댓글은 자막/설명과 일치할 때만 보조로 참고
            
            [Universal Culinary Principles & Chef's Insight]
            - **[핵심]: 단순한 조리 순서 나열을 넘어, 영상 속 셰프가 강조하는 "이유(Why)"와 "철학(Philosophy)"을 반드시 포함하라.**
            - 셰프가 특정 행동을 하는 이유(맛, 식감, 과학적 원리)를 설명했다면, 이를 누락하지 말고 기록하라.
            - 암묵적 재료: 시각/조리 행위로 "거의 확실"할 때만 포함
            
            ==============================
            3) 성공 JSON 스키마 (반드시 이 형태)
            {
              "isRecipe": true,
              "nonRecipeReason": null,
              "title": "요리 제목",
              "dishType": "볶음",
              "description": "영상 톤의 1~2문장 소개(맛/식감 1개 + 핵심특징 1개 포함)",
              "cookingTime": 0,
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
            - dishType은 아래 중 정확히 1개만 선택:
              "볶음", "국/찌개/탕", "구이", "무침/샐러드", "튀김/부침",
              "찜/조림", "오븐요리", "생식/회", "절임/피클류", "밥/면/파스타", "디저트/간식류"
            - 빈 문자열/공백 금지
            
            [숫자 필드]
            - cookingTime: 0 이상의 정수(분)
            - servings: 정수(반올림하여 출력), 소수 금지
            - marketPrice: 정수, 100원 단위 올림(ceil)
            - quantity: 아래 형식만 허용(문자열이지만 수치로 해석 가능해야 함)
              - 정수: "2"
              - 소수: "0.5"
              - 분수: "1/2"  (혼합분수 "1 1/2" 금지, 공백 금지)
            - quantity/marketPrice/cookingTime/servings는 null/"" 절대 금지
            
            [timeline]
            - "MM:SS" 문자열 또는 null만 허용
            - 자막에 [04:12]가 있으면 우선 매핑
            - 시간을 확실히 못 찾으면 억지로 추측하지 말고 null
            
            [ingredients] (DB 매칭을 위한 핵심 규칙)
            - **[중요] 단일 명사 원칙:** '또는', 'or', '/', '대체', '취향껏' 같은 표현 금지. 영상에서 실제로 사용한 **가장 메인이 되는 재료 하나**만 적어라.
            - quantity: 단위와 수량을 명확히 분리하고, null 금지.
            - **[부재료 포착]:** 파, 깨, 참기름, 후추 등 셰프가 조리 중간에 "향"이나 "마무리"를 위해 소량 첨가하는 재료도 놓치지 말고 포함하라.
            - **[소스 분석]:** 영상에서 별도의 소스(양념장)를 배합하는 과정이 나온다면, 그 배합에 들어가는 재료(간장, 설탕, 식초 등)를 모두 분리하여 적어라.
            
            [steps] (영상 순서 최우선 규칙)
            - stepNumber는 0부터 1씩 증가
            - **[순서 규칙: 타임라인 오름차순]:** 요리의 논리적 순서보다 **'영상의 편집/진행 순서'**를 최우선으로 따르라.
              - 사용자가 영상을 보며 따라 할 수 있도록, `step 0` -> `step 1`으로 갈수록 `timeline` 시간도 반드시 커져야 한다. (시간 역전 금지)
            - **[Instruction 구성]:** 영상 흐름이 끊기지 않도록, 화면에 보이는 동작 위주로 단계를 구성하라.
            - timeline: 해당 Step의 행동이 영상에서 **실제로 시작되는 시간**을 정확히 매핑하라.
              - 확실히 못 찾으면 null (추측 금지)
            - action은 아래 20개 중 1개만:
              "썰기","다지기","채썰기","손질하기","볶기","튀기기","끓이기","찌기","데치기","구이","조림","무치기","절이기","담그기","섞기","젓기","버무리기","로스팅","캐러멜라이즈","부치기"
            
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
            - 3~5개 팁을 "문장"으로만 이어서 작성
            - 숫자/목록표시/접두어("팁:") 금지
            """;
    }

    @Async("recipeExtractionExecutor")
    public CompletableFuture<PresignedUrlResponse> extractAndCreateRecipe(String videoUrl, Long userId) {
        log.info("🚀 유튜브 레시피 추출 요청: URL={}", videoUrl);

        if (!YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }

        String videoId = extractVideoId(videoUrl);

        if (videoId != null) {
            String standardizedUrl = convertToCanonical(videoId);
            Optional<Recipe> existingRecipe = recipeRepository.findByYoutubeUrl(standardizedUrl);
            if (existingRecipe.isPresent()) {
                log.info("♻️ 이미 존재하는 레시피 발견 (ID 기반). 생성 건너뜀: ID={}", existingRecipe.get().getId());
                return handleExistingRecipe(existingRecipe.get(), userId);
            }
        }

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

            Optional<Recipe> existingRecipeCanonical = recipeRepository.findByYoutubeUrl(canonicalUrl);
            if (existingRecipeCanonical.isPresent()) {
                log.info("♻️ 이미 존재하는 레시피 발견 (Canonical URL). 쿼터 환불 및 연결: ID={}", existingRecipeCanonical.get().getId());

                dailyQuotaService.refundIfPolicyAllows(userId, QuotaType.YOUTUBE_EXTRACTION);

                return handleExistingRecipe(existingRecipeCanonical.get(), userId);
            }

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

            PresignedUrlResponse response = saveRecipeTransactional(recipeDto, OFFICIAL_RECIPE_USER_ID);

            addFavoriteToUser(userId, response.getRecipeId());

            log.info("💾 신규 생성 및 즐겨찾기 추가 완료: ID={}, UserID={}", response.getRecipeId(), userId);
            return CompletableFuture.completedFuture(response);

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
    public void refreshRecommendedRecipes() {
        if (!isRefreshing.compareAndSet(false, true)) return;

        log.info("🔄 [스케줄러] 타겟 채널 레시피 갱신 시작...");

        try {
            List<YoutubeTargetChannel> allChannels = youtubeTargetChannelRepository.findAllByIsActiveTrue();

            if (allChannels.isEmpty()) {
                log.warn("⚠️ 수집할 타겟 채널이 DB에 없습니다.");
                return;
            }

            Collections.shuffle(allChannels);
            List<YoutubeTargetChannel> selectedChannels = allChannels.subList(0, Math.min(allChannels.size(), 5));

            log.info("🎯 이번 턴 수집 채널: {}", selectedChannels.stream().map(YoutubeTargetChannel::getChannelName).toList());

            List<YtDlpService.YoutubeSearchDto> combinedResults = new ArrayList<>();

            for (YoutubeTargetChannel channel : selectedChannels) {
                List<YtDlpService.YoutubeSearchDto> results = ytDlpService.getLatestVideosFromChannel(channel.getChannelUrl(), 10);
                combinedResults.addAll(results);
            }

            if (!combinedResults.isEmpty()) {
                Map<String, YtDlpService.YoutubeSearchDto> bestById = new LinkedHashMap<>();
                for (YtDlpService.YoutubeSearchDto dto : combinedResults) {
                    if (dto == null || dto.videoId() == null) continue;

                    if (isNoiseVideo(dto.title())) continue;

                    bestById.put(dto.videoId(), dto);
                }

                List<YtDlpService.YoutubeSearchDto> rankedResults = bestById.values().stream()
                        .sorted(Comparator.comparingLong(YtDlpService.YoutubeSearchDto::viewCount).reversed())
                        .limit(40)
                        .toList();

                this.cachedRecommendations.set(rankedResults);

                if (!rankedResults.isEmpty()) {
                    log.info("🏆 [트렌드 1위] {} (조회수: {})", rankedResults.get(0).title(), rankedResults.get(0).viewCount());
                }
            }

        } catch (Exception e) {
            log.error("❌ 갱신 실패", e);
        } finally {
            isRefreshing.set(false);
        }
    }

    public List<YtDlpService.YoutubeSearchDto> getRecommendedRecipes() {
        List<YtDlpService.YoutubeSearchDto> currentPool = this.cachedRecommendations.get();

        if (currentPool.isEmpty()) {
            refreshRecommendedRecipes();
            currentPool = this.cachedRecommendations.get();
        }

        if (currentPool.isEmpty()) return Collections.emptyList();

        List<YtDlpService.YoutubeSearchDto> shuffledList = new ArrayList<>(currentPool);
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

    private CompletableFuture<PresignedUrlResponse> handleExistingRecipe(Recipe recipe, Long requestingUserId) {
        addFavoriteToUser(requestingUserId, recipe.getId());

        PresignedUrlResponse response = PresignedUrlResponse.builder()
                .recipeId(recipe.getId())
                .uploads(Collections.emptyList())
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

            PresignedUrlResponse originalRes = recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.YOUTUBE);

            return PresignedUrlResponse.builder()
                    .recipeId(originalRes.getRecipeId())
                    .uploads(originalRes.getUploads())
                    .created(true)
                    .build();
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
                double q = parseQuantitySafe(cur.getQuantity());
                cur.setQuantity(formatQuantity(q));
                merged.put(key, cur);
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
}