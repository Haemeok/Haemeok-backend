package com.jdc.recipe_service.util;

import org.springframework.stereotype.Component;

import java.util.Set;

@Component
public class ActionImageService {

    private static final String ACTION_BASE_PATH = "action";
    private static final String BASE_URL = "https://s3.amazonaws.com/haemeok";

    private static final Set<String> SUPPORTED_ACTIONS = Set.of(
            "썰기", "다지기", "채썰기", "손질하기", "볶기", "튀기기", "끓이기",
            "찌기(스팀)", "데치기", "구이", "조림", "무치기", "절이기",
            "담그기(마리네이드)", "섞기", "젓기", "버무리기", "로스팅", "캐러멜라이즈", "부치기"
    );

    public String generateImageKey(String action, int index) {
        if (!isSupportedAction(action)) return null;
        return String.format("%s/%s/%d.jpg", ACTION_BASE_PATH, action, index);
    }

    public String generateImageUrl(String imageKey) {
        return imageKey != null ? String.format("%s/%s", BASE_URL, imageKey) : null;
    }

    public boolean isSupportedAction(String action) {
        return action != null && SUPPORTED_ACTIONS.contains(action);
    }
}
