package com.jdc.recipe_service.util;

import org.springframework.stereotype.Component;

import java.util.Random;

@Component
public class ActionImageService {

    private static final int IMAGE_COUNT_PER_ACTION = 5;
    private static final String ACTION_BASE_PATH = "action";
    private static final String BASE_URL = "https://s3.amazonaws.com/haemeok";

    private final Random random = new Random();

    public int generateRandomIndex() {
        return random.nextInt(IMAGE_COUNT_PER_ACTION) + 1; // 1~5
    }

    public String generateImageKey(String action, int index) {
        if (action == null || action.isBlank()) return null;
        return String.format("%s/%s/%d.jpg", ACTION_BASE_PATH, action, index);
    }

    public String generateImageUrl(String imageKey) {
        return String.format("%s/%s", BASE_URL, imageKey);
    }

    public boolean isSupportedAction(String action) {
        return action != null && !action.isBlank();
    }
}
