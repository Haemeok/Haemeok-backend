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
                Map<String, Long> channelSubscribers = fetchChannelSubscribers(new ArrayList<>(channelIdsToFetch));

                for (Recipe recipe : recipes) {
                    String chId = recipe.getYoutubeChannelId();
                    if (chId != null && channelSubscribers.containsKey(chId)) {
                        recipe.updateYoutubeSubscriberCount(channelSubscribers.get(chId));
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

    private Map<String, Long> fetchChannelSubscribers(List<String> channelIds) throws Exception {
        String url = "https://www.googleapis.com/youtube/v3/channels" +
                "?part=statistics" +
                "&id=" + String.join(",", channelIds) +
                "&key=" + getApiKey();

        JsonNode root = objectMapper.readTree(new URL(url));
        Map<String, Long> result = new HashMap<>();

        for (JsonNode item : root.path("items")) {
            String cid = item.path("id").asText();
            long subs = item.path("statistics").path("subscriberCount").asLong(0);
            result.put(cid, subs);
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
}