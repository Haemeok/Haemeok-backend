package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.service.media.YtDlpService.YoutubeFullDataDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class YtDlpServiceTest {

    private YtDlpService ytDlpService;

    private String ytdlpPath = "/Library/Frameworks/Python.framework/Versions/3.11/bin/yt-dlp";

    @BeforeEach
    void setUp() {
        ytDlpService = new YtDlpService(new ObjectMapper());

        File f = new File(ytdlpPath);
        System.out.println("🔎 [진단] 파일 경로: " + ytdlpPath);
        System.out.println("   - 존재 여부: " + f.exists());
        System.out.println("   - 실행 가능: " + f.canExecute());

        ReflectionTestUtils.setField(ytDlpService, "ytdlpPath", ytdlpPath);
        ReflectionTestUtils.setField(ytDlpService, "userAgent", "Mozilla/5.0 (Test Agent)");
        ReflectionTestUtils.setField(ytDlpService, "cacheDir", "./yt-cache");
        ReflectionTestUtils.setField(ytDlpService, "tmpBaseDir", System.getProperty("java.io.tmpdir"));
        ReflectionTestUtils.setField(ytDlpService, "timeoutSeconds", 60L);
        ReflectionTestUtils.setField(ytDlpService, "maxComments", 5);
        ReflectionTestUtils.setField(ytDlpService, "proxyUrl", "");
        ReflectionTestUtils.setField(ytDlpService, "subtitleLangs", "ko,en");
        ReflectionTestUtils.setField(ytDlpService, "youtubeApiKeys", List.of());
    }

    @Disabled("로컬 디버그용 — yt-dlp 설치 필요")
    @Test
    @DisplayName("🚀 로컬 통합 테스트: 에러 추적 모드")
    void testRealExecution() {
        String videoUrl = "https://www.youtube.com/shorts/JNXHKtDBF48";

        System.out.println("⏳ yt-dlp 실행 시도 중...");

        try {
            YoutubeFullDataDto result = ytDlpService.getVideoDataFull(videoUrl);

            System.out.println("✅ 성공!");
            System.out.println("제목: " + result.title());
            assertThat(result.title()).isNotBlank();

        } catch (Exception e) {
            System.err.println("❌ 테스트 실패! 진짜 원인은 아래와 같습니다:");
            e.printStackTrace();
        }
    }

    @Disabled("로컬 디버그용 — yt-dlp 설치 필요")
    @Test
    void testGetVideoDataFull() {
        String targetUrl = "https://www.youtube.com/watch?v=CiNtYiBt2oQ";

        System.out.println("🚀 유튜브 데이터 추출 시작...");
        long startTime = System.currentTimeMillis();

        YtDlpService.YoutubeFullDataDto result = ytDlpService.getVideoDataFull(targetUrl);

        long endTime = System.currentTimeMillis();

        System.out.println("\n==================================================");
        System.out.println("✅ 추출 완료 (소요시간: " + (endTime - startTime) + "ms)");
        System.out.println("==================================================");
        System.out.println("📌 [영상 제목]: " + result.title());
        System.out.println("📌 [채널명]: " + result.channelName());
        System.out.println("📌 [조회수]: " + result.viewCount());
        System.out.println("--------------------------------------------------");
        System.out.println("📝 [설명글(Description)]:\n" + result.description());
        System.out.println("--------------------------------------------------");
        System.out.println("💬 [고정/작성자 댓글(Comments)]:\n" + result.comments());
        System.out.println("--------------------------------------------------");
        System.out.println("🔤 [중복 제거된 깔끔한 자막(Subtitles)]:\n" + result.scriptPlain());
        System.out.println("==================================================\n");
    }
}