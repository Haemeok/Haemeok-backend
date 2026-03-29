package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.service.ai.GrokClientService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

@SpringBootTest
class RecipeAccuracyTest {

    @Autowired
    private YtDlpService ytDlpService;

    @Autowired
    private GrokClientService grokClientService;

    @Autowired
    private ObjectMapper objectMapper;

    private static final Pattern UNIT_PATTERN = Pattern.compile("(?i)(큰술|작은술|밥숟가락|티스푼|종이컵|국자|주걱|꼬집|약간|적당량|spoon|tbs|tbsp|tsp|cup|oz|lb|kg|ml|l|cc|liter|개|마리|모|단|통|알|쪽|줌|봉|봉지|팩|장|copy|ea|\\b[0-9]+/[0-9]+\\b|\\b[0-9.]+\\s?(g|kg|ml|l|cc)\\b)");
    private static final Pattern INGREDIENT_KEYWORD_PATTERN = Pattern.compile("(?i)(재료|ingredient|준비물|필요|양념|소스|드레싱|시즈닝|seasoning|sauce|dressing|materials|shopping list)");
    private static final Pattern NUMBER_UNIT_IN_LINE = Pattern.compile("\\d+\\s*[./]?\\s*\\d*\\s*(큰술|작은술|스푼|티스푼|종이컵|국자|개|마리|g|kg|ml|봉지|줌|쪽|알|장|컵|꼬집|약간|적당량|tbsp|tsp|cup|oz)\\b|\\b(약간|적당량|조금)");

    // ✅ 테스트할 URL 목록 - 여기에 추가
    private static final List<String> TEST_URLS = List.of(
            "https://www.youtube.com/watch?v=miuQsqrnewc"
            // 추가 URL을 아래에 넣어주세요:
            // "https://www.youtube.com/watch?v=xxxx",
            // "https://www.youtube.com/watch?v=yyyy"
    );

    private static final String SEPARATOR = "\n" + "=".repeat(70) + "\n";
    private static final String THIN_SEPARATOR = "-".repeat(50);

    @Test
    void testPipelineAccuracy() throws Exception {
        int total = TEST_URLS.size();
        System.out.println(SEPARATOR);
        System.out.printf("🎯 레시피 추출 정확도 테스트 시작 (총 %d개 영상)%n", total);
        System.out.println(SEPARATOR);

        for (int i = 0; i < TEST_URLS.size(); i++) {
            String url = TEST_URLS.get(i);
            System.out.printf("%n[%d/%d] 처리 시작: %s%n", i + 1, total, url);
            System.out.println(THIN_SEPARATOR);

            try {
                runSingleTest(url, i + 1);
            } catch (Exception e) {
                System.out.printf("❌ 오류 발생: %s%n", e.getMessage());
            }

            System.out.println(SEPARATOR);
        }

        System.out.println("✅ 전체 테스트 완료");
    }

    private void runSingleTest(String url, int index) throws Exception {
        long startTime = System.currentTimeMillis();

        // ─── 1. 메타데이터 수집 ───────────────────────────────────────
        System.out.println("📥 [1] 유튜브 데이터 추출 중...");
        YtDlpService.YoutubeFullDataDto videoData = ytDlpService.getVideoDataFull(url);

        System.out.printf("📌 제목: %s%n", videoData.title());

        // ─── 2. 소스 데이터 출력 (원본 기준) ─────────────────────────
        System.out.println("\n📄 [소스 데이터 - 원본]");
        System.out.println(THIN_SEPARATOR);

        String description = videoData.description() != null ? videoData.description() : "";
        String comments    = videoData.comments()    != null ? videoData.comments()    : "";

        System.out.println("▶ 설명글 (앞 1500자):");
        System.out.println(description.length() > 1500 ? description.substring(0, 1500) + "...(생략)" : description);

        System.out.println("\n▶ 댓글 (앞 3000자):");
        System.out.println(comments.length() > 3000 ? comments.substring(0, 3000) + "...(생략)" : comments);

        // ─── 3. 자막 전처리 및 재료 힌트 추출 ────────────────────────
        String[] lines = videoData.scriptTimecoded() != null
                ? videoData.scriptTimecoded().split("\\r?\\n")
                : new String[0];

        List<String> plainLines      = new ArrayList<>();
        List<String> ingredientLines = new ArrayList<>();

        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty() || trimmed.startsWith("WEBVTT") || trimmed.startsWith("NOTE") || trimmed.contains("-->")) continue;
            String content = trimmed.replaceAll("<[^>]+>", "").replace("&nbsp;", " ").trim();
            if (content.isEmpty()) continue;
            plainLines.add(content);
            if (UNIT_PATTERN.matcher(content).find() || NUMBER_UNIT_IN_LINE.matcher(content).find() || INGREDIENT_KEYWORD_PATTERN.matcher(content).find()) {
                ingredientLines.add(content);
            }
        }

        System.out.println("\n▶ 자막에서 추출된 재료·수량 관련 라인 (" + ingredientLines.size() + "줄):");
        if (ingredientLines.isEmpty()) {
            System.out.println("  (재료 관련 자막 없음)");
        } else {
            ingredientLines.forEach(l -> System.out.println("  " + l));
        }

        // ─── 4. AI 추출 ───────────────────────────────────────────────
        String plainScriptForContext = String.join("\n", plainLines);
        String ingredientHintBlock = ingredientLines.isEmpty()
                ? "(재료·양 언급 구간 없음)"
                : "[재료·양 언급 구간]\n" + String.join("\n", ingredientLines);

        String fullContext = String.format("""
                영상 URL: %s
                영상 제목: %s
                영상 설명: %s
                고정/인기 댓글: %s

                %s

                [자막 전문]
                %s
                """, url, videoData.title(), description, comments, ingredientHintBlock, plainScriptForContext);

        System.out.println("\n🤖 [2] Grok Step1 추출 중...");
        RecipeCreateRequestDto recipeDto = grokClientService.generateRecipeStep1(getSystemPrompt(), fullContext).join();

        System.out.println("🤖 [3] Grok Step2 재료 정제 중...");
        List<RecipeIngredientRequestDto> refinedIngredients =
                grokClientService.refineIngredientsOnly("식재료 데이터 정제 AI", recipeDto.getIngredients()).join();

        // ─── 5. 결과 출력 ─────────────────────────────────────────────
        System.out.println("\n📊 [추출 결과 비교]");
        System.out.println(THIN_SEPARATOR);

        System.out.printf("🔴 Step1 재료 (%d개):%n", recipeDto.getIngredients().size());
        recipeDto.getIngredients().forEach(i ->
                System.out.printf("  %-20s | %s %s  [추정=%s]%n",
                        i.getName(), i.getQuantity(), i.getCustomUnit(),
                        Boolean.TRUE.equals(i.getIsEstimated()) ? "Y" : "N"));

        System.out.printf("%n🟢 Step2 재료 (%d개):%n", refinedIngredients.size());
        refinedIngredients.forEach(i ->
                System.out.printf("  %-20s | %s %s%n",
                        i.getName(), i.getQuantity(), i.getCustomUnit()));

        // Step1→Step2 변경 감지
        System.out.println("\n⚠️  Step1→Step2 변경 감지:");
        boolean anyChange = false;
        for (int j = 0; j < Math.min(recipeDto.getIngredients().size(), refinedIngredients.size()); j++) {
            RecipeIngredientRequestDto s1 = recipeDto.getIngredients().get(j);
            RecipeIngredientRequestDto s2 = refinedIngredients.get(j);
            boolean unitChanged = !eq(s1.getCustomUnit(), s2.getCustomUnit());
            boolean qtyChanged  = !eq(s1.getQuantity(),   s2.getQuantity());
            if (unitChanged || qtyChanged) {
                anyChange = true;
                System.out.printf("  %-20s | %s %s  →  %s %s%s%n",
                        s1.getName(),
                        s1.getQuantity(), s1.getCustomUnit(),
                        s2.getQuantity(), s2.getCustomUnit(),
                        unitChanged ? "  ⚡단위변환" : "");
            }
        }
        if (!anyChange) System.out.println("  (변경 없음)");

        long elapsed = System.currentTimeMillis() - startTime;
        System.out.printf("%n⏱  소요시간: %dms%n", elapsed);
    }

    private boolean eq(String a, String b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.trim().equals(b.trim());
    }

    private String getSystemPrompt() {
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
영상에 두 가지 이상의 레시피가 나올 경우:
1. 가장 비중 있거나 제목과 일치하는 메인 레시피 1개만 선택
2. 선택하지 않은 버전의 재료나 조리법 절대 혼합 금지

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

**dishType** - 정확히 1개만 선택:
"볶음", "국/찌개/탕", "구이", "무침/샐러드", "튀김/부침", "찜/조림", "오븐요리", "생식/회", "절임/피클류", "밥/면/파스타", "디저트/간식류"

**ingredients** - 핵심 규칙:
1. 단일 명사 원칙
2. quantity 형식: "2", "0.5", "1/2"
3. [단위 보존]: 영상에서 나온 단위 그대로 (컵, 국자, 개 등 무리하게 변환 금지)
4. 소스 분해, 부재료 포착 필수
5. [🚨필수 플래그🚨]: 수량이 영상에 명시됐으면 isEstimated:false, 추론이면 isEstimated:true

**steps**:
- stepNumber: 0부터 시작
- timeline: "MM:SS" 또는 null
- instruction: 2-3문장 상세
- action: "썰기","다지기","볶기","튀기기","끓이기","찌기","데치기","굽기","조림","무치기","씻기","부치기" 중 1개

**tags** - 최대 3개:
"🏠 홈파티","🌼 피크닉","🏕️ 캠핑","🥗 다이어트 / 건강식","👶 아이와 함께","🍽️ 혼밥","🍶 술안주","🥐 브런치","🌙 야식","⚡ 초스피드 / 간단 요리","🎉 기념일 / 명절","🍱 도시락","🔌 에어프라이어","🍲 해장","👨‍🍳 셰프 레시피"

## 절대 금지
- 코드펜스, 설명문
- 근거 없는 추측
- 빈 문자열/null (허용 필드 제외)
- 중복 재료
""";
    }
}
