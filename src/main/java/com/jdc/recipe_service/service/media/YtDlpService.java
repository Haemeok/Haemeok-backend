package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
public class YtDlpService {

    private static final ObjectMapper OM = new ObjectMapper();

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
                nullToEmpty(subs.plainText)
        );
    }


    /* =========================================================
     * Internal Logic (Meta, Comment, Subtitle)
     * ========================================================= */

    private MetaAndComment extractMetaAndCommentsWithFallback(String canonicalUrl) {
        for (String client : CLIENT_FALLBACK) {
            try {
                ExecResult r = execForJson(buildCommentArgs(canonicalUrl, client, maxComments), null);
                JsonNode root = OM.readTree(r.stdout);

                String title = optText(root, "title");
                String desc = optText(root, "description");

                StringBuilder commentsBuilder = new StringBuilder();
                JsonNode comments = root.get("comments");
                if (comments != null && comments.isArray()) {
                    for (JsonNode c : comments) {
                        String text = optText(c, "text");
                        if (!isBlank(text)) {
                            commentsBuilder.append("- ").append(text).append("\n");
                        }
                    }
                }

                if (!isBlank(title) || !isBlank(desc)) {
                    return new MetaAndComment(title, desc, commentsBuilder.toString());
                }
            } catch (Exception ignored) {
            }
        }
        return new MetaAndComment("", "", "");
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
        return n == null ? "" : n.asText();
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

    private record MetaAndComment(String title, String description, String comments) {}

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
            String scriptPlain
    ) {}
}