package com.jdc.recipe_service.service.media;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.LinkedHashMap;

@Slf4j
@Service
@RequiredArgsConstructor
@Profile("local")
public class YoutubeStatsService {

    private final RecipeRepository recipeRepository;
    private final ObjectMapper objectMapper;
    private final TransactionTemplate transactionTemplate;

    @Value("${youtube.api-keys}")
    private List<String> apiKeys;

    private static final Pattern VIDEO_ID_PATTERN = Pattern.compile(
            "(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|shorts/|embed/))([a-zA-Z0-9_-]{11})"
    );

    /**
     * 🔧 [null 갱신] youtubeChannelName / channelProfileUrl / channelId 가 null인 레시피를
     *    1개씩 개별 처리 — 하나 실패해도 나머지 계속 진행
     */
    public String refreshNullChannelInfo() {
        List<Recipe> targets = recipeRepository.findAllWithNullYoutubeChannelInfo();
        if (targets.isEmpty()) return "null 채널 정보 레시피 없음";

        log.info("🔧 null 채널 정보 레시피 {}개 갱신 시작", targets.size());

        int successCount = 0;
        int failCount = 0;

        for (Recipe recipe : targets) {
            try {
                boolean updated = Boolean.TRUE.equals(transactionTemplate.execute(status -> {
                    try {
                        return repairOne(recipe);
                    } catch (Exception ex) {
                        throw new RuntimeException(ex);
                    }
                }));
                if (updated) {
                    successCount++;
                } else {
                    failCount++;
                    log.warn("⚠️ 채널 정보 없음 (영상 비공개/삭제 가능성): recipeId={}, url={}",
                            recipe.getId(), recipe.getYoutubeUrl());
                }
            } catch (Exception e) {
                failCount++;
                log.error("❌ recipeId={} 갱신 실패: {}", recipe.getId(), e.getMessage());
            }

            try { Thread.sleep(100); } catch (InterruptedException ignored) {}
        }

        return String.format("null 채널 정보 %d개 — 성공 %d / 실패 %d", targets.size(), successCount, failCount);
    }

    private boolean repairOne(Recipe recipe) throws Exception {
        String vid = extractVideoId(recipe.getYoutubeUrl());
        if (vid == null) return false;

        // 1) 영상 정보로 channelId 확보
        Map<String, VideoInfo> videoInfos = fetchVideoDetails(List.of(vid));
        VideoInfo info = videoInfos.get(vid);
        if (info == null || info.channelId() == null) return false;

        // 2) 채널 정보 조회
        Map<String, ChannelDetail> channelDetails = fetchChannelDetails(List.of(info.channelId()));
        ChannelDetail detail = channelDetails.get(info.channelId());
        if (detail == null) return false;

        recipe.updateYoutubeInfo(
                detail.name(),
                info.channelId(),
                recipe.getYoutubeVideoTitle(),
                recipe.getYoutubeThumbnailUrl(),
                detail.profileUrl(),
                detail.subscriberCount(),
                recipe.getYoutubeVideoViewCount()
        );
        log.info("✅ recipeId={} 채널 정보 갱신 완료: channelName={}", recipe.getId(), detail.name());
        return true;
    }

    private Map<String, ChannelDetail> fetchChannelDetails(List<String> channelIds) throws Exception {
        String url = "https://www.googleapis.com/youtube/v3/channels"
                + "?part=snippet,statistics"
                + "&id=" + String.join(",", channelIds)
                + "&key=" + getApiKey();

        JsonNode root = objectMapper.readTree(new URL(url));
        Map<String, ChannelDetail> result = new HashMap<>();

        for (JsonNode item : root.path("items")) {
            String cid = item.path("id").asText();
            String name = item.path("snippet").path("title").asText(null);
            JsonNode thumbs = item.path("snippet").path("thumbnails");
            String profileUrl = thumbs.path("high").path("url").asText(null);
            if (profileUrl == null || profileUrl.isBlank())
                profileUrl = thumbs.path("medium").path("url").asText(null);
            if (profileUrl == null || profileUrl.isBlank())
                profileUrl = thumbs.path("default").path("url").asText(null);
            long subs = item.path("statistics").path("subscriberCount").asLong(0);
            result.put(cid, new ChannelDetail(name, profileUrl, subs));
        }
        return result;
    }

    /**
     * 🚀 [메인] 모든 레시피의 유튜브 통계(조회수, 구독자) 업데이트
     * 주의: 전체 메서드에 @Transactional을 걸지 않습니다! (배치별로 끊어서 저장하기 위해)
     */
    public String updateAllRecipeStats() {
        List<Recipe> recipes = recipeRepository.findAllByYoutubeUrlIsNotNull();
        if (recipes.isEmpty()) return "업데이트할 레시피가 없습니다.";

        log.info("🔄 총 {}개의 레시피 유튜브 통계 갱신 시작...", recipes.size());

        int batchSize = 50;
        int successCount = 0;

        for (int i = 0; i < recipes.size(); i += batchSize) {
            int end = Math.min(recipes.size(), i + batchSize);
            List<Recipe> batchRecipes = recipes.subList(i, end);

            Integer batchSuccess = transactionTemplate.execute(status -> {
                return processBatch(batchRecipes);
            });

            if (batchSuccess != null) {
                successCount += batchSuccess;
            }

            log.info("✅ 배치 진행 중: {}/{} 완료", end, recipes.size());

            try { Thread.sleep(100); } catch (InterruptedException e) {}
        }

        return String.format("총 %d개 중 %d개 업데이트 완료", recipes.size(), successCount);
    }

    private int processBatch(List<Recipe> recipes) {
        Map<String, Recipe> mapByVideoId = new HashMap<>();
        List<String> validVideoIds = new ArrayList<>();

        for (Recipe r : recipes) {
            String vid = extractVideoId(r.getYoutubeUrl());
            if (vid != null) {
                mapByVideoId.put(vid, r);
                validVideoIds.add(vid);
            }
        }

        if (validVideoIds.isEmpty()) return 0;

        try {
            Map<String, VideoInfo> videoInfos = fetchVideoDetails(validVideoIds);
            Set<String> channelIdsToFetch = new HashSet<>();

            for (String vid : validVideoIds) {
                VideoInfo info = videoInfos.get(vid);
                Recipe recipe = mapByVideoId.get(vid);

                if (info != null && recipe != null) {
                    recipe.updateYoutubeVideoViewCount(info.viewCount);

                    if (info.channelId != null) {
                        recipe.updateYoutubeChannelId(info.channelId);
                        channelIdsToFetch.add(info.channelId);
                    }
                }
            }

            if (!channelIdsToFetch.isEmpty()) {
                Map<String, ChannelDetail> channelDetails = fetchChannelDetails(new ArrayList<>(channelIdsToFetch));

                for (Recipe recipe : recipes) {
                    String chId = recipe.getYoutubeChannelId();
                    if (chId != null && channelDetails.containsKey(chId)) {
                        recipe.updateYoutubeSubscriberCount(channelDetails.get(chId).subscriberCount());
                    }
                }
            }

            return validVideoIds.size();

        } catch (Exception e) {
            log.error("❌ 배치 처리 중 API 오류 발생", e);
            return 0;
        }
    }

    private Map<String, VideoInfo> fetchVideoDetails(List<String> videoIds) throws Exception {
        String url = "https://www.googleapis.com/youtube/v3/videos" +
                "?part=statistics,snippet" +
                "&id=" + String.join(",", videoIds) +
                "&key=" + getApiKey();

        JsonNode root = objectMapper.readTree(new URL(url));
        Map<String, VideoInfo> result = new HashMap<>();

        for (JsonNode item : root.path("items")) {
            String vid = item.path("id").asText();
            long viewCount = item.path("statistics").path("viewCount").asLong(0);
            String channelId = item.path("snippet").path("channelId").asText(null);

            result.put(vid, new VideoInfo(viewCount, channelId));
        }
        return result;
    }

    private String getApiKey() {
        if (apiKeys == null || apiKeys.isEmpty()) throw new RuntimeException("API Key Missing");
        return apiKeys.get(0);
    }

    private String extractVideoId(String url) {
        if (url == null) return null;
        Matcher matcher = VIDEO_ID_PATTERN.matcher(url);
        return matcher.find() ? matcher.group(1) : null;
    }

    private record VideoInfo(long viewCount, String channelId) {}

    private record ChannelDetail(String name, String profileUrl, long subscriberCount) {}
}