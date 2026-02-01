package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.service.media.YtDlpService.YoutubeFullDataDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class YtDlpManualTest {

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
}