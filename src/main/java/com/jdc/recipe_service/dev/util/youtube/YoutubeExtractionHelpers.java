package com.jdc.recipe_service.dev.util.youtube;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;

import java.net.URI;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * V2 prod (RecipeExtractionService)의 pure static helpers를 dev V3 facade에서 재사용 가능하게 분리.
 *
 * 운영 V2의 private helpers는 그대로 두고 (prod 무수정), 동일 시그너처로 dev에서 사용한다.
 * dev → prod swap 시 V2의 사본을 제거하고 이 클래스를 정식으로 옮긴다.
 *
 * 모든 메서드는 instance state를 사용하지 않는 pure function.
 */
public final class YoutubeExtractionHelpers {

    private YoutubeExtractionHelpers() {}

    // ---------- 길이 상수 (V2와 동일) ----------
    public static final int MAX_CONTEXT_CHARS = 100_000;
    public static final int MAX_SCRIPT_CHARS  = 80_000;
    public static final int MAX_DESC_CHARS    = 10_000;
    public static final int MAX_CMT_CHARS     = 3_000;
    public static final long MAX_VIDEO_DURATION_SECONDS = 70L * 60;

    // ---------- 패턴 (V2와 동일) ----------
    public static final Pattern YOUTUBE_URL_PATTERN = Pattern.compile(
            "(?i)^(https?://)?(www\\.)?(youtube\\.com|youtu\\.be)/.+$"
    );

    public static final Pattern INGREDIENT_KEYWORD_PATTERN = Pattern.compile(
            "(?i)(재료|ingredient|준비물|필요|양념|소스|드레싱|시즈닝|seasoning|sauce|dressing|materials|shopping list)"
    );

    public static final Pattern UNIT_PATTERN = Pattern.compile(
            "(?i)(큰술|작은술|밥숟가락|티스푼|종이컵|국자|주걱|꼬집|약간|적당량|" +
                    "spoon|tbs|tbsp|tsp|cup|oz|lb|kg|ml|l|cc|liter|" +
                    "개|마리|모|단|통|알|쪽|줌|봉|봉지|팩|장|copy|ea|" +
                    "\\b[0-9]+/[0-9]+\\b|" +
                    "\\b[0-9.]+\\s?(g|kg|ml|l|cc)\\b)"
    );

    public static final Pattern STEP_ACTION_PATTERN = Pattern.compile(
            "(?i)(만드는|방법|순서|조리|과정|레시피|recipe|step|direction|how to|" +
                    "넣|볶|끓|굽|튀기|섞|다지|채썰|썰|자르|데치|삶|찌|무치|부치|재우|간하|손질|씻|헹구|" +
                    "chop|mix|boil|fry|stir|bake|roast|grill|simmer|poach|slice|mince|dice)"
    );

    public static final Pattern TIMESTAMP_PATTERN = Pattern.compile(
            "(?i)(\\d{1,2}:\\d{2}|\\d{1,2}\\s?(분|min)|\\d{1,2}\\s?(초|sec))"
    );

    private static final Pattern VIDEO_ID_PATTERN = Pattern.compile(
            "(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|" +
                    "watch\\?v%3D|watch\\?feature=player_embedded&v=|" +
                    "%2Fvideos%2F|embed%5C%2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*"
    );

    /** YouTube video ID 정규 형식: base64url 11자 (A-Z, a-z, 0-9, _, -). */
    private static final Pattern VALID_VIDEO_ID_PATTERN = Pattern.compile("^[A-Za-z0-9_-]{11}$");

    // ---------- URL 처리 ----------

    /** V2와 동일한 video ID 추출 정규식. */
    public static String extractVideoId(String url) {
        if (url == null || url.isBlank()) return null;
        Matcher matcher = VIDEO_ID_PATTERN.matcher(url);
        return matcher.find() ? matcher.group().trim() : null;
    }

    /** 추출된 video ID가 유효한 형식인지 (11자 base64url) — channel/playlist URL을 사전에 거른다. */
    public static boolean isValidVideoId(String videoId) {
        return videoId != null && VALID_VIDEO_ID_PATTERN.matcher(videoId).matches();
    }

    public static boolean isShortsUrl(String url) {
        return url != null && (url.contains("youtube.com/shorts/") || url.contains("/shorts/"));
    }

    public static String buildStorageYoutubeUrl(String videoId, boolean shorts) {
        return shorts
                ? "https://www.youtube.com/shorts/" + videoId
                : "https://www.youtube.com/watch?v=" + videoId;
    }

    /** S3 full URL → key (path 부분, leading slash 제거). 실패 시 원본 반환. */
    public static String extractS3Key(String fullUrl) {
        if (fullUrl == null || fullUrl.isBlank()) return null;
        try {
            URI uri = new URI(fullUrl);
            String path = uri.getPath();
            return (path != null && path.startsWith("/")) ? path.substring(1) : path;
        } catch (Exception e) {
            return fullUrl;
        }
    }

    // ---------- 문자열 유틸 ----------

    public static String nullToEmpty(String s) {
        return s == null ? "" : s;
    }

    public static String emptyToPlaceholder(String s, String placeholder) {
        return (s == null || s.isBlank()) ? placeholder : s;
    }

    /**
     * V2와 동일하게 head 70% + tail 30% 보존하는 truncation.
     */
    public static String cap(String s, int max) {
        if (s == null) return "";
        if (s.length() <= max) return s;
        int head = (int) (max * 0.7);
        int tail = max - head;
        return s.substring(0, head) + "\n...(truncated)...\n" + s.substring(s.length() - tail);
    }

    public static String safeMsg(Throwable t) {
        if (t == null) return "";
        return t.getMessage() != null ? t.getMessage() : t.getClass().getSimpleName();
    }

    // ---------- 결과 품질 검증 (V2 isRecipeResultGarbage와 동일 정책) ----------

    /**
     * Grok이 isRecipe=true를 줬더라도 결과가 사용 불가능한 garbage인지 판정.
     * V2 isRecipeResultGarbage와 동일 기준:
     *  - DTO null → garbage
     *  - isRecipe=false → garbage 아님 (별도 reject 경로)
     *  - 재료 2개 미만 → garbage
     *  - bad quantity (null/0/약간) 비율이 50% 초과 → garbage
     *
     * dev V3에서 garbage 판정되면 Gemini fallback 진행 → cost 5 + usedGeminiAnalysis=true 기록.
     */
    public static boolean isRecipeResultGarbage(RecipeCreateRequestDto dto) {
        if (dto == null) return true;
        if (!Boolean.TRUE.equals(dto.getIsRecipe())) return false; // 별도 처리 경로

        List<RecipeIngredientRequestDto> ings = dto.getIngredients();
        if (ings == null || ings.size() < 2) return true;

        long badQuantityCount = ings.stream()
                .filter(i -> {
                    String q = i.getQuantity();
                    String u = i.getCustomUnit();
                    return q == null || "0".equals(q) || (q.contains("약간"))
                            || (u != null && u.contains("약간"));
                })
                .count();
        return (double) badQuantityCount / ings.size() > 0.5;
    }
}
