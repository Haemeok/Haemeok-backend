package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.service.media.YoutubeStatsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@Slf4j
@RestController
@RequestMapping("/api/admin/sync")
@RequiredArgsConstructor
@Profile("local")
public class YoutubeSyncController {

    private final YoutubeStatsService youtubeStatsService;

    /**
     * ⚡ 수동 동기화 트리거
     * 권한: ADMIN 롤을 가진 사용자만 실행 가능
     * 사용법: POST /api/admin/sync/stats (Header에 Authorization 토큰 필요)
     */
    @PostMapping("/stats")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<String> triggerSync() {
        log.info("👮 [Admin] 유튜브 통계 동기화 요청 승인됨");
        String result = youtubeStatsService.updateAllRecipeStats();
        return ResponseEntity.ok(result);
    }

    /**
     * null 채널 정보(채널명·프로필·channelId) 보정
     * POST /api/admin/sync/channel-info
     */
    @PostMapping("/channel-info")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<String> repairNullChannelInfo() {
        log.info("👮 [Admin] null 채널 정보 갱신 요청");
        String result = youtubeStatsService.refreshNullChannelInfo();
        return ResponseEntity.ok(result);
    }
}