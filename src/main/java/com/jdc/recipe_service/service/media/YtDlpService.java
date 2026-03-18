package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Builder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@Slf4j
@RequiredArgsConstructor
public class YtDlpService {

    private final ObjectMapper objectMapper;

    private static final Pattern VIDEO_ID_PAT = Pattern.compile(
            "(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|shorts/|embed/))([a-zA-Z0-9_-]{11})"
    );

    private final Random random = new Random();

    private static final List<String> COOKIE_FILES = List.of(
            "/home/ec2-user/cookies/cookies_1.txt",
            "/home/ec2-user/cookies/cookies_2.txt",
            "/home/ec2-user/cookies/cookies_3.txt",
            "/home/ec2-user/cookies/cookies_4.txt"
    );

    private static final List<String> CLIENT_FALLBACK = List.of("android", "web", "ios");

    @Value("${app.ytdlp.path:yt-dlp}")
    private String ytdlpPath;

    @Value("${app.ytdlp.cacheDir:/tmp/yt-dlp-cache}")
    private String cacheDir;

    @Value("${app.ytdlp.tmpBaseDir:/tmp}")
    private String tmpBaseDir;

    @Value("${app.ytdlp.timeoutSeconds:60}")
    private long timeoutSeconds;

    @Value("${app.ytdlp.enableRemoteComponents:true}")
    private boolean enableRemoteComponents;

    @Value("${app.ytdlp.jsRuntimes:deno}")
    private String jsRuntimes;

    @Value("${app.ytdlp.userAgent:Mozilla/5.0 (iPhone; CPU iPhone OS 16_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.6 Mobile/15E148 Safari/604.1}")
    private String userAgent;

    @Value("${app.ytdlp.subtitleLangs:ko-orig,ko,en}")
    private String subtitleLangs;

    @Value("${app.ytdlp.maxComments:3}")
    private int maxComments;

    @Value("${app.ytdlp.proxy:}")
    private String proxyUrl;

    @Value("#{'${youtube.api-keys:}'.split(',')}")
    private List<String> youtubeApiKeys;

    /* =========================================================
     * Public APIs: 오직 텍스트 데이터만 가져옵니다.
     * ========================================================= */

    public YoutubeFullDataDto getVideoDataFull(String anyYoutubeUrl) {
        NormalizedYoutube n = normalize(anyYoutubeUrl);
        Path workDir = null;

        try {
            workDir = Files.createTempDirectory(Paths.get(tmpBaseDir), "yt-full-" + n.videoId);

            IntegratedData rawData = executeIntegratedExtraction(n.canonicalUrl, n.videoId, workDir);

            ChannelInfo channelInfo;
            if (rawData.metaJson != null) {
                channelInfo = fetchChannelMetadata(rawData.metaJson);
            } else {
                channelInfo = new ChannelInfo("", "", 0L, false);
            }

            return new YoutubeFullDataDto(
                    n.videoId,
                    n.canonicalUrl,
                    nullToEmpty(rawData.title),
                    nullToEmpty(rawData.description),
                    nullToEmpty(rawData.comments),
                    nullToEmpty(rawData.subtitles.timecodedText()),
                    nullToEmpty(rawData.subtitles.plainText()),
                    isBlank(channelInfo.name) ? nullToEmpty(rawData.channelName) : channelInfo.name,
                    nullToEmpty(rawData.channelId),
                    nullToEmpty(rawData.thumbnailUrl),
                    channelInfo.profileUrl,
                    channelInfo.subscriberCount,
                    rawData.viewCount,
                    rawData.duration
            );

        } catch (IOException e) {
            log.error("❌ 임시 디렉토리 생성 실패 또는 I/O 에러", e);
            throw new RuntimeException("Youtube processing failed", e);
        } finally {
            deleteDirQuietly(workDir);
        }
    }

    /* =========================================================
     * Internal Logic: Integrated Execution
     * ========================================================= */

    /**
     * ✅ [업그레이드] yt-dlp 통합 실행 로직
     * 전략: 1차 시도(No Cookie) -> 실패 시 2차 시도(With Cookie)
     */
    private IntegratedData executeIntegratedExtraction(String url, String videoId, Path workDir) {
        for (String client : CLIENT_FALLBACK) {
            try {
                List<String> args = buildIntegratedArgs(url, client, workDir, false);
                ExecResult res = execForJson(args, workDir);
                return parseAndReturnResult(res, client, workDir, videoId);

            } catch (Exception e) {
                log.warn("⚠️ 1차 시도 실패 (쿠키X, Proxy O, client={}): {}", client, e.getMessage());
                cleanDirectoryFiles(workDir);

                try {
                    log.info("🔄 2차 시도 시작 (쿠키O, Proxy X, client={})", client);

                    List<String> retryArgs = buildIntegratedArgs(url, client, workDir, true);
                    ExecResult retryRes = execForJson(retryArgs, workDir);

                    IntegratedData result = parseAndReturnResult(retryRes, client, workDir, videoId);
                    log.info("🎉 2차 시도 성공 (With Cookie & No Proxy)!");
                    return result;

                } catch (Exception e2) {
                    log.error("❌ 2차 시도(쿠키O)도 실패: {}", e2.getMessage());
                    cleanDirectoryFiles(workDir);
                }
            }
        }
        return new IntegratedData("", "", "", new SubtitleTexts("", ""), "", "", "", 0L, 0L, null);
    }

    /**
     * 실행 결과를 파싱하여 IntegratedData 객체로 변환하는 헬퍼 메서드
     */
    private IntegratedData parseAndReturnResult(ExecResult res, String client, Path workDir, String videoId) throws Exception {
        JsonNode root = objectMapper.readTree(res.stdout);

        Optional<Path> vttPath = findPreferredVtt(workDir, videoId);
        SubtitleTexts subs = vttPath.isPresent() ? parseVtt(vttPath.get()) : new SubtitleTexts("", "");

        StringBuilder commentsBuilder = new StringBuilder();
        JsonNode commentsNode = root.get("comments");

        if (commentsNode != null && commentsNode.isArray()) {
            for (JsonNode c : commentsNode) {
                boolean isPinned = c.path("is_pinned").asBoolean(false);
                boolean isUploader = c.path("author_is_uploader").asBoolean(false);

                if (isPinned || isUploader) {
                    String text = extractCommentText(c);
                    if (!isBlank(text)) {
                        commentsBuilder.append("[크리에이터 레시피]:\n").append(text).append("\n");
                        break;
                    }
                }
            }
        }

        String title = optText(root, "title");
        String desc = optDescription(root);
        String channelId = optText(root, "channel_id");
        if (isBlank(channelId)) channelId = optText(root, "uploader_id");

        String originalChannelName = optText(root, "channel");
        if (isBlank(originalChannelName)) originalChannelName = optText(root, "uploader");

        String thumbnailUrl = optThumbnailUrl(root);

        log.info("✅ [추출 성공] client={}, title={}, subs={}", client, shrink(title, 20), (subs.plainText.isEmpty() ? "없음" : "있음"));

        return new IntegratedData(
                title,
                desc,
                commentsBuilder.toString(),
                subs,
                originalChannelName,
                channelId,
                thumbnailUrl,
                root.path("view_count").asLong(0),
                root.path("duration").asLong(0),
                root
        );
    }

    /**
     * 비용 절감을 위한 핵심: 한 번에 모든 옵션을 때려박은 인자 리스트 생성
     */
    private List<String> buildIntegratedArgs(String url, String client, Path dir, boolean useCookie) {
        List<String> a = buildBaseArgs(!useCookie);

        if (useCookie) {
            String randomCookieFile = COOKIE_FILES.get(random.nextInt(COOKIE_FILES.size()));
            if (Files.exists(Paths.get(randomCookieFile))) {
                a.add("--cookies");
                a.add(randomCookieFile);
                log.info("🍪 [Cookie] 적용됨 (Proxy OFF): {}", randomCookieFile);
            } else {
                log.warn("⚠️ 쿠키 파일을 찾을 수 없음: {}", randomCookieFile);
            }
        }

        a.add("--extractor-args");
        a.add("youtube:player_client=" + client + ";max_comments=" + maxComments);
        a.add("--dump-single-json");
        a.add("--no-simulate");
        a.add("--write-comments");
        a.add("--write-subs");
        a.add("--write-auto-subs");
        a.add("--sub-langs"); a.add(subtitleLangs);
        a.add("--sub-format"); a.add("vtt");
        a.add("-o"); a.add(dir.resolve("sub_%(id)s.%(ext)s").toString());
        a.add("--skip-download");
        a.add(url);
        return a;
    }

    private List<String> buildBaseArgs(boolean useProxy) {
        List<String> a = new ArrayList<>();
        a.add(ytdlpPath);
        a.add("--ignore-config");
        a.add("--no-playlist");
        a.add("--cache-dir"); a.add(cacheDir);
        //a.add("--user-agent"); a.add(userAgent);

        if (useProxy && proxyUrl != null && !proxyUrl.isBlank()) {
            a.add("--proxy");
            a.add(proxyUrl);
        }

        if (enableRemoteComponents) {
            a.add("--remote-components");
            a.add("ejs:github");
        }
        if (jsRuntimes != null && !jsRuntimes.isBlank()) {
            a.add("--js-runtimes");
            a.add(jsRuntimes);
        }
        return a;
    }

    private ChannelInfo fetchChannelMetadata(JsonNode root) {
        String originalName = optText(root, "channel");
        if (isBlank(originalName)) originalName = optText(root, "uploader");

        Long fallbackSubscribers = root.path("channel_follower_count").asLong(0);
        String channelId = optText(root, "channel_id");

        log.info("🔎 [API 시도] channelId='{}'", channelId);

        if (!isBlank(channelId) && youtubeApiKeys != null && !youtubeApiKeys.isEmpty()) {
            for (int i = 0; i < youtubeApiKeys.size(); i++) {
                String currentKey = youtubeApiKeys.get(i).trim();
                if (isBlank(currentKey)) continue;

                try {
                    String apiUrl = "https://youtube.googleapis.com/youtube/v3/channels?part=snippet,statistics&id=" + channelId + "&key=" + currentKey;

                    log.info("🌐 [API 요청] {}", apiUrl);

                    String response = new String(new java.net.URL(apiUrl).openStream().readAllBytes(), StandardCharsets.UTF_8);
                    JsonNode apiRoot = objectMapper.readTree(response);

                    if (apiRoot.has("error")) {
                        log.warn("⚠️ [API 에러] Key[{}] 실패: {}", i, apiRoot.toPrettyString());
                        continue;
                    }

                    JsonNode items = apiRoot.path("items");
                    if (items.isArray() && items.size() > 0) {
                        JsonNode item = items.get(0);

                        JsonNode snippet = item.path("snippet");
                        JsonNode statistics = item.path("statistics");

                        String realName = optText(snippet, "title");

                        JsonNode thumbs = snippet.path("thumbnails");
                        String profileUrl = optText(thumbs.path("high"), "url");
                        if (isBlank(profileUrl)) profileUrl = optText(thumbs.path("medium"), "url");
                        if (isBlank(profileUrl)) profileUrl = optText(thumbs.path("default"), "url");

                        Long subscriberCount = statistics.path("subscriberCount").asLong(0);
                        if (subscriberCount == 0) subscriberCount = fallbackSubscribers;

                        log.info("✅ [API 성공] 이름='{}', 구독자={}, 프로필='{}'", realName, subscriberCount, profileUrl);

                        if (!isBlank(realName)) {
                            return new ChannelInfo(realName, profileUrl, subscriberCount, true);
                        }
                    } else {
                        log.warn("⚠️ [API 실패] 검색 결과 없음 (items is empty). Response: {}", shrink(response, 200));
                    }
                    break;
                } catch (Exception e) {
                    log.warn("⚠️ [API 예외] Key index: {}, Error: {}", i, e.getMessage());
                }
            }
        } else {
            log.warn("🚫 [API 스킵] channelId 없음 또는 Key 없음 (keys={})", (youtubeApiKeys == null ? "null" : youtubeApiKeys.size()));
        }

        String fallbackName = originalName;
        String handle = optText(root, "uploader_id");
        if ((isBlank(fallbackName) || fallbackName.startsWith("@")) && handle.startsWith("@") && handle.length() > 1) {
            fallbackName = handle.substring(1);
        }

        return new ChannelInfo(fallbackName, "", fallbackSubscribers, false);
    }

    /* =========================================================
     * exec (JSON Only)
     * ========================================================= */

    private ExecResult execForJson(List<String> args, Path workDir) {
        try {
            ProcessBuilder pb = new ProcessBuilder(args);
            if (workDir != null) pb.directory(workDir.toFile());
            pb.redirectErrorStream(false);

            Process p = pb.start();

            ByteArrayOutputStream out = new ByteArrayOutputStream(64 * 1024);
            ByteArrayOutputStream err = new ByteArrayOutputStream(64 * 1024);

            Thread t1 = new Thread(() -> {
                try { p.getInputStream().transferTo(out); } catch (IOException ignored) {}
            });
            Thread t2 = new Thread(() -> {
                try { p.getErrorStream().transferTo(err); } catch (IOException ignored) {}
            });
            t1.start();
            t2.start();

            if (!p.waitFor(timeoutSeconds, TimeUnit.SECONDS)) {
                p.destroyForcibly();
                throw new RuntimeException("yt-dlp timeout");
            }

            t1.join();
            t2.join();

            int code = p.exitValue();
            if (code != 0) {
                String stderr = err.toString(StandardCharsets.UTF_8);
                throw new RuntimeException("yt-dlp error: " + shrink(stderr, 200));
            }

            return new ExecResult(
                    code,
                    out.toString(StandardCharsets.UTF_8),
                    err.toString(StandardCharsets.UTF_8)
            );

        } catch (Exception e) {
            log.error("💥 yt-dlp 실행 중 치명적 오류 발생! 명령: {}", args);
            log.error("💥 원인:", e);
            throw new RuntimeException("yt-dlp execution failed", e);
        }
    }

    /**
     * 📺 특정 채널의 최신 영상 가져오기 (New!)
     * @param channelUrl 채널의 동영상 탭 URL (예: .../videos)
     * @param limit 가져올 영상 개수
     */
    public List<YoutubeSearchDto> getLatestVideosFromChannel(String channelUrl, int limit) {
        log.info("📡 채널 수집 시작: URL={}, 개수={}", channelUrl, limit);

        List<String> commands = new ArrayList<>();
        commands.add(ytdlpPath);

        if (proxyUrl != null && !proxyUrl.isBlank()) {
            commands.add("--proxy");
            commands.add(proxyUrl.trim());
        }

        commands.add(channelUrl);

        commands.add("--playlist-end");
        commands.add(String.valueOf(limit));

        addCommonListOptions(commands);

        return executeYtDlpListCommand(commands, "채널 수집");
    }

    /**
     * 🔍 키워드 검색 (기존 유지 - 필요시 사용)
     */
    public List<YoutubeSearchDto> searchVideoList(String keyword, int limit) {
        log.info("🔍 유튜브 검색 시작: 키워드={}, 개수={}", keyword, limit);

        List<String> commands = new ArrayList<>();
        commands.add(ytdlpPath);

        if (proxyUrl != null && !proxyUrl.isBlank()) {
            commands.add("--proxy");
            commands.add(proxyUrl.trim());
        }

        commands.add("ytsearch" + limit + ":" + keyword);
        commands.add("--dateafter");
        commands.add("now-1month");

        addCommonListOptions(commands);

        return executeYtDlpListCommand(commands, "키워드 검색");
    }

    private void addCommonListOptions(List<String> commands) {
        commands.add("--dump-json");
        commands.add("--no-warnings");
        commands.add("--ignore-config");
        commands.add("--skip-download");
        commands.add("--ignore-errors");
    }

    private List<YoutubeSearchDto> executeYtDlpListCommand(List<String> commands, String jobName) {
        ProcessBuilder pb = new ProcessBuilder(commands);
        pb.redirectErrorStream(false);

        Process p = null;
        try {
            p = pb.start();

            StringBuilder errBuf = new StringBuilder(8192);
            Process finalP = p;
            Thread errDrain = new Thread(() -> {
                try (BufferedReader er = new BufferedReader(
                        new InputStreamReader(finalP.getErrorStream(), StandardCharsets.UTF_8))) {
                    String line;
                    while ((line = er.readLine()) != null) {
                        if (errBuf.length() < 20000) errBuf.append(line).append('\n');
                    }
                } catch (Exception ignore) {}
            });
            errDrain.setDaemon(true);
            errDrain.start();

            List<YoutubeSearchDto> results = new ArrayList<>();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(p.getInputStream(), StandardCharsets.UTF_8))) {

                String line;
                while ((line = reader.readLine()) != null) {
                    try {
                        JsonNode node = objectMapper.readTree(line);

                        String videoId = node.path("id").asText("");
                        if (videoId.length() != 11) continue;

                        String title = node.path("title").asText("");
                        if (title.isBlank()) continue;

                        String channel = node.path("uploader").asText("");
                        long viewCount = node.path("view_count").asLong(0);

                        String thumbnail = "https://i.ytimg.com/vi/" + videoId + "/mqdefault.jpg";

                        results.add(new YoutubeSearchDto(title, videoId, channel, thumbnail, viewCount));
                    } catch (Exception ignoreJson) {
                    }
                }
            }

            boolean finished = p.waitFor(timeoutSeconds, TimeUnit.SECONDS);
            if (!finished) {
                p.destroyForcibly();
                try { p.waitFor(2, TimeUnit.SECONDS); } catch (Exception ignore) {}
                log.warn("⚠️ yt-dlp timeout: [{}]", jobName);
                return results;
            }

            int code = p.exitValue();
            if (code != 0) {
                log.warn("⚠️ yt-dlp nonzero exit: code={}, job={}, err={}", code, jobName, errBuf.toString());
            }

            return results;

        } catch (Exception e) {
            log.error("[{}] 실행 실패", jobName, e);
            if (p != null) p.destroyForcibly();
            return Collections.emptyList();
        }
    }

    public record YoutubeSearchDto(
            String title,
            String videoId,
            String channelName,
            String thumbnailUrl,
            long viewCount
    ) {
        public String getVideoUrl() {
            return "https://www.youtube.com/watch?v=" + videoId;
        }
    }

    /* =========================================================
     * Utils / Builders
     * ========================================================= */

    private void cleanDirectoryFiles(Path dir) {
        if (dir == null) return;
        try (DirectoryStream<Path> ds = Files.newDirectoryStream(dir)) {
            for (Path p : ds) Files.deleteIfExists(p);
        } catch (IOException ignored) {}
    }

    private NormalizedYoutube normalize(String url) {
        if (url == null) throw new IllegalArgumentException("URL null");
        String u = url.trim();
        Matcher m = VIDEO_ID_PAT.matcher(u);
        if (!m.find()) throw new IllegalArgumentException("유효하지 않은 URL");
        String id = m.group(1);
        return new NormalizedYoutube(id, "https://www.youtube.com/watch?v=" + id);
    }

    private Optional<Path> findPreferredVtt(Path dir, String videoId) throws IOException {
        try (DirectoryStream<Path> ds = Files.newDirectoryStream(dir, "sub_" + videoId + "*.vtt")) {
            for (Path p : ds) return Optional.of(p);
        }
        return Optional.empty();
    }

    private SubtitleTexts parseVtt(Path vttFile) throws IOException {
        List<String> lines = Files.readAllLines(vttFile, StandardCharsets.UTF_8);

        StringBuilder plain = new StringBuilder();
        StringBuilder timecoded = new StringBuilder();

        String currentTime = null;

        for (String raw : lines) {
            String line = raw.trim();
            if (line.isBlank() || line.startsWith("WEBVTT") || line.startsWith("NOTE")) continue;

            if (line.contains("-->")) {
                currentTime = toMmSs(line.split("-->")[0].trim());
                continue;
            }

            if (line.matches("^\\d+$")) continue;

            String text = line
                    .replaceAll("<[^>]+>", "")
                    .replace("&amp;", "&")
                    .replace("&lt;", "<")
                    .replace("&gt;", ">")
                    .trim();

            if (text.isBlank()) continue;

            if (plain.length() > 0) plain.append(' ');
            plain.append(text);

            if (currentTime != null) {
                timecoded.append('[').append(currentTime).append("] ").append(text).append('\n');
            } else {
                timecoded.append(text).append('\n');
            }
        }

        String rawPlainText = plain.toString().trim();
        String[] words = rawPlainText.split("\\s+");
        StringBuilder dedupPlain = new StringBuilder();
        String lastWord = "";

        for (String w : words) {
            if (!w.equals(lastWord)) {
                dedupPlain.append(w).append(" ");
                lastWord = w;
            }
        }

        return new SubtitleTexts(
                timecoded.toString().trim(),
                dedupPlain.toString().trim()
        );
    }

    private String toMmSs(String hhmmss) {
        String[] parts = hhmmss.split(":");
        if (parts.length == 3) {
            int mm = safeInt(parts[1]);
            int ss = safeSec(parts[2]);
            return String.format("%02d:%02d", mm, ss);
        }
        if (parts.length == 2) {
            int mm = safeInt(parts[0]);
            int ss = safeSec(parts[1]);
            return String.format("%02d:%02d", mm, ss);
        }
        return "00:00";
    }

    private int safeInt(String s) {
        try { return Integer.parseInt(s.trim()); } catch (Exception e) { return 0; }
    }

    private int safeSec(String s) {
        try {
            double v = Double.parseDouble(s.trim());
            return (int) Math.floor(v);
        } catch (Exception e) {
            return 0;
        }
    }

    private String optText(JsonNode root, String field) {
        JsonNode n = root.get(field);
        if (n == null || n.isNull()) {
            return "";
        }
        return n.asText();
    }

    /**
     * yt-dlp/유튜브 댓글 구조 대응.
     * - 최상위 "text", "content", "message"
     * - nested "runs" (유튜브 API: [{ "text": "..." }])
     */
    private String extractCommentText(JsonNode c) {
        if (c == null) return "";
        for (String key : List.of("text", "content", "message")) {
            String v = optText(c, key);
            if (!isBlank(v)) return v;
        }
        JsonNode runs = c.get("runs");
        if (runs != null && runs.isArray()) {
            StringBuilder sb = new StringBuilder();
            for (JsonNode r : runs) {
                String t = optText(r, "text");
                if (!isBlank(t)) sb.append(t);
            }
            if (sb.length() > 0) return sb.toString();
        }
        return "";
    }

    /**
     * 클라이언트별로 description 필드가 비어 있거나 다른 키로 올 수 있음. 여러 후보 시도.
     */
    private String optDescription(JsonNode root) {
        for (String key : List.of("description", "full_description", "summary")) {
            String v = optText(root, key);
            if (!isBlank(v)) return v;
        }
        return "";
    }

    /**
     * yt-dlp는 thumbnail(문자열) 또는 thumbnails(배열) 반환 가능. 배열이면 마지막(고해상도) URL 사용.
     */
    private String optThumbnailUrl(JsonNode root) {
        String single = optText(root, "thumbnail");
        if (!isBlank(single)) return single;
        JsonNode arr = root.get("thumbnails");
        if (arr != null && arr.isArray() && arr.size() > 0) {
            JsonNode last = arr.get(arr.size() - 1);
            String url = optText(last, "url");
            if (!isBlank(url)) return url;
        }
        return "";
    }

    private boolean isBlank(String s) {
        return s == null || s.isBlank();
    }

    private String nullToEmpty(String s) {
        return s == null ? "" : s;
    }

    private String shrink(String s, int max) {
        if (s == null) return "";
        if (s.length() <= max) return s;
        return s.substring(0, max) + "...(truncated)";
    }

    private void deleteDirQuietly(Path dir) {
        if (dir == null) return;
        try {
            if (!Files.exists(dir)) return;
            Files.walk(dir)
                    .sorted(Comparator.reverseOrder())
                    .forEach(p -> {
                        try { Files.deleteIfExists(p); } catch (IOException ignored) {}
                    });
        } catch (Exception ignored) {}
    }

    /* =========================================================
     * DTOs
     * ========================================================= */

    private record IntegratedData(
            String title,
            String description,
            String comments,
            SubtitleTexts subtitles,
            String channelName,
            String channelId,
            String thumbnailUrl,
            Long viewCount,
            Long duration,
            JsonNode metaJson
    ) {}

    public record NormalizedYoutube(String videoId, String canonicalUrl) {}

    private record ChannelInfo(String name, String profileUrl, Long subscriberCount, boolean isApiUsed) {}

    private record SubtitleTexts(String timecodedText, String plainText) {}

    private static class ExecResult {
        final int exitCode;
        final String stdout;
        final String stderr;

        ExecResult(int c, String o, String e) {
            this.exitCode = c;
            this.stdout = o;
            this.stderr = e;
        }
    }

    @Builder
    public record YoutubeFullDataDto(
            String videoId,
            String canonicalUrl,
            String title,
            String description,
            String comments,
            String scriptTimecoded,
            String scriptPlain,
            String channelName,
            String channelId,
            String thumbnailUrl,
            String channelProfileUrl,
            Long youtubeSubscriberCount,
            Long viewCount,
            Long duration
    ) {}
}