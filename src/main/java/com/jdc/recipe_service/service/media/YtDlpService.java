package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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

    @Value("${app.ytdlp.subtitleLangs:ko-orig,ko}")
    private String subtitleLangs;

    @Value("${app.ytdlp.maxComments:1}")
    private int maxComments;

    @Value("${app.ytdlp.proxy:}")
    private String proxyUrl;

    @Value("#{'${youtube.api-keys:}'.split(',')}")
    private List<String> youtubeApiKeys;

    @jakarta.annotation.PostConstruct
    public void logYoutubeApiKeys() {
        log.info("==================================================");
        log.info("🎥 [DEBUG] YouTube API Key 설정 확인");

        if (youtubeApiKeys == null || youtubeApiKeys.isEmpty() || (youtubeApiKeys.size() == 1 && youtubeApiKeys.get(0).isBlank())) {
            log.error("❌ YouTube API Key 리스트가 비어있습니다! (환경변수 YOUTUBE_API_KEYS 확인 필요)");
            log.error("👉 현재 설정값: {}", youtubeApiKeys);
        } else {
            log.info("👉 로드된 키 개수: {}개", youtubeApiKeys.size());
            for (int i = 0; i < youtubeApiKeys.size(); i++) {
                String key = youtubeApiKeys.get(i).trim();
                // 보안상 앞 10자리만 출력 (AIzaSy... 확인용)
                String masked = key.length() > 10 ? key.substring(0, 10) + "..." : key;

                if (key.isBlank()) {
                    log.warn("⚠️ Key[{}] 는 빈 문자열입니다!", i);
                } else if (!key.startsWith("AIzaSy")) {
                    log.warn("⚠️ Key[{}] 형식이 수상합니다 (AIzaSy로 시작 안함): {}", i, masked);
                } else {
                    log.info("✅ Key[{}] 정상 로드: {}", i, masked);
                }
            }
        }
        log.info("==================================================");
    }

    /* =========================================================
     * Public APIs: 오직 텍스트 데이터만 가져옵니다.
     * ========================================================= */

    /**
     * 유튜브 영상의 모든 텍스트 정보(제목, 설명, 댓글, 자막)를 추출합니다.
     * 파일 다운로드를 하지 않으므로 매우 빠릅니다.
     */
    public YoutubeFullDataDto getVideoDataFull(String anyYoutubeUrl) {
        NormalizedYoutube n = normalize(anyYoutubeUrl);

        MetaAndComment mc = extractMetaAndCommentsWithFallback(n.canonicalUrl);

        SubtitleTexts subs = downloadKoreanSubtitlesWithFallback(n.canonicalUrl, n.videoId);

        return new YoutubeFullDataDto(
                n.videoId,
                n.canonicalUrl,
                nullToEmpty(mc.title),
                nullToEmpty(mc.description),
                nullToEmpty(mc.comments),
                nullToEmpty(subs.timecodedText),
                nullToEmpty(subs.plainText),
                nullToEmpty(mc.channelName),
                nullToEmpty(mc.channelId),
                nullToEmpty(mc.thumbnailUrl),
                nullToEmpty(mc.channelProfileUrl),
                mc.subscriberCount,
                mc.viewCount
        );
    }


    /* =========================================================
     * Internal Logic (Meta, Comment, Subtitle)
     * ========================================================= */

    private MetaAndComment extractMetaAndCommentsWithFallback(String canonicalUrl) {
        MetaAndComment bestResult = null;

        for (String client : CLIENT_FALLBACK) {
            try {
                ExecResult metaRes = execForJson(buildMetaArgs(canonicalUrl, client), null);
                JsonNode metaRoot = objectMapper.readTree(metaRes.stdout);

                String title = optText(metaRoot, "title");
                String desc = optText(metaRoot, "description");
                String thumbnail = optText(metaRoot, "thumbnail");
                String channelId = optText(metaRoot, "channel_id");
                if (isBlank(channelId)) channelId = optText(metaRoot, "uploader_id");
                Long viewCount = metaRoot.path("view_count").asLong(0);

                ChannelInfo channelInfo = fetchChannelMetadata(metaRoot);

                ExecResult cRes = execForJson(buildCommentArgs(canonicalUrl, client, maxComments), null);
                JsonNode cRoot = objectMapper.readTree(cRes.stdout);

                StringBuilder commentsBuilder = new StringBuilder();
                JsonNode comments = cRoot.get("comments");
                if (comments != null && comments.isArray()) {
                    for (JsonNode c : comments) {
                        String text = optText(c, "text");
                        if (!isBlank(text)) commentsBuilder.append("- ").append(text).append("\n");
                    }
                }

                MetaAndComment current = new MetaAndComment(
                        title,
                        desc,
                        commentsBuilder.toString(),
                        channelInfo.name(),
                        channelId,
                        thumbnail,
                        channelInfo.profileUrl(),
                        channelInfo.subscriberCount(),
                        viewCount
                );

                log.info("[client={}] uploader='{}', channel='{}', uploader_id='{}', channel_id='{}'",
                        client,
                        optText(metaRoot, "uploader"),
                        optText(metaRoot, "channel"),
                        optText(metaRoot, "uploader_id"),
                        optText(metaRoot, "channel_id"));

                if ((!isBlank(current.title) || !isBlank(current.description)) && !isBlank(current.channelName)) {
                    return current;
                }

                if (bestResult == null || isBetter(current, bestResult)) {
                    bestResult = current;
                }

            } catch (Exception ignored) {
            }
        }
        return bestResult != null ? bestResult : new MetaAndComment("", "", "", "", "", "", "", 0L,0L);
    }

    private boolean isBetter(MetaAndComment a, MetaAndComment b) {
        if (!isBlank(a.channelName) && isBlank(b.channelName)) return true;
        if (isBlank(a.channelName) && !isBlank(b.channelName)) return false;

        int aText = (isBlank(a.title) ? 0 : 1) + (isBlank(a.description) ? 0 : 1);
        int bText = (isBlank(b.title) ? 0 : 1) + (isBlank(b.description) ? 0 : 1);
        if (aText != bText) return aText > bText;

        return !isBlank(a.thumbnailUrl) && isBlank(b.thumbnailUrl);
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

    private List<String> buildMetaArgs(String url, String client) {
        List<String> a = buildBaseArgs();
        a.add("--extractor-args");
        a.add("youtube:player_client=" + client);
        a.add("--dump-single-json");
        a.add("--skip-download");
        a.add(url);
        return a;
    }

    private String pickChannelName(JsonNode root) {
        String v = optText(root, "channel");
        if (!v.isBlank()) return v;
        v = optText(root, "uploader");
        if (!v.isBlank()) return v;
        v = optText(root, "channel_name");
        if (!v.isBlank()) return v;
        String handle = optText(root, "uploader_id");
        if (handle.startsWith("@") && handle.length() > 1) {
            v = handle.substring(1);
        }
        String channelId = optText(root, "channel_id");
        if (!isBlank(channelId) && youtubeApiKeys != null && !youtubeApiKeys.isEmpty()) {
            for (int i = 0; i < youtubeApiKeys.size(); i++) {
                String currentKey = youtubeApiKeys.get(i).trim();
                if (isBlank(currentKey)) continue;
                try {
                    String apiUrl = "https://youtube.googleapis.com/youtube/v3/channels?part=snippet&id=" + channelId + "&key=" + currentKey;

                    String response = new String(new java.net.URL(apiUrl).openStream().readAllBytes(), StandardCharsets.UTF_8);
                    JsonNode apiRoot = objectMapper.readTree(response);

                    if (apiRoot.has("error")) {
                        String errMsg = apiRoot.path("error").path("message").asText();
                        log.warn("⚠️ YouTube API Key[{}] 실패: {} -> 다음 키 시도...", i, errMsg);
                        continue;
                    }

                    JsonNode items = apiRoot.path("items");
                    if (items.isArray() && items.size() > 0) {
                        String realName = optText(items.get(0).path("snippet"), "title");
                        if (!isBlank(realName)) {
                            log.info("✅ YouTube API 성공 (Key index: {}): {}", i, realName);
                            return realName;
                        }
                    }
                    break;

                } catch (Exception e) {
                    log.warn("⚠️ YouTube API 호출 중 예외 발생 (Key index: {}): {}", i, e.getMessage());
                }
            }
        }

        return isBlank(v) ? optText(root, "uploader_id") : v;
    }

    private SubtitleTexts downloadKoreanSubtitlesWithFallback(String url, String videoId) {
        Path workDir = null;
        try {
            workDir = Files.createTempDirectory(Paths.get(tmpBaseDir), "yt-sub-");
            for (String client : CLIENT_FALLBACK) {
                try {
                    execForJson(buildSubtitleArgs(url, client, workDir), workDir);
                    Optional<Path> vtt = findPreferredVtt(workDir, videoId);
                    if (vtt.isPresent()) return parseVtt(vtt.get());
                } catch (Exception ignored) {}
            }
            return new SubtitleTexts("", "");
        } catch (IOException e) {
            return new SubtitleTexts("", "");
        } finally {
            if (workDir != null) deleteDirQuietly(workDir);
        }
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

    private NormalizedYoutube normalize(String url) {
        if (url == null) throw new IllegalArgumentException("URL null");
        String u = url.trim();
        Matcher m = VIDEO_ID_PAT.matcher(u);
        if (!m.find()) throw new IllegalArgumentException("유효하지 않은 URL");
        String id = m.group(1);
        return new NormalizedYoutube(id, "https://www.youtube.com/watch?v=" + id);
    }

    private List<String> buildBaseArgs() {
        List<String> a = new ArrayList<>();
        a.add(ytdlpPath);
        a.add("--ignore-config");
        a.add("--no-playlist");
        a.add("--cache-dir"); a.add(cacheDir);
        a.add("--user-agent"); a.add(userAgent);

        if (proxyUrl != null && !proxyUrl.isBlank()) {
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

    private List<String> buildCommentArgs(String url, String client, int max) {
        List<String> a = buildBaseArgs();
        a.add("--extractor-args");
        a.add("youtube:player_client=" + client + ";max_comments=" + max);
        a.add("--get-comments");
        a.add("--dump-single-json");
        a.add("--skip-download");
        a.add(url);
        return a;
    }

    private List<String> buildSubtitleArgs(String url, String client, Path dir) {
        List<String> a = buildBaseArgs();
        a.add("--extractor-args");
        a.add("youtube:player_client=" + client);
        a.add("--write-subs");
        a.add("--write-auto-subs");
        a.add("--sub-langs"); a.add(subtitleLangs);
        a.add("--sub-format"); a.add("vtt");
        a.add("--skip-download");
        a.add("-o"); a.add(dir.resolve("sub_%(id)s.%(ext)s").toString());
        a.add(url);
        return a;
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

        return new SubtitleTexts(
                timecoded.toString().trim(),
                plain.toString().trim()
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

    public record NormalizedYoutube(String videoId, String canonicalUrl) {}

    private record MetaAndComment(
            String title,
            String description,
            String comments,
            String channelName,
            String channelId,
            String thumbnailUrl,
            String channelProfileUrl,
            Long subscriberCount,
            Long viewCount
    ) {}

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
            Long viewCount
    ) {}
}