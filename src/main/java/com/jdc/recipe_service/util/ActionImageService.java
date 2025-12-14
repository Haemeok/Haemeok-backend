
package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
@Slf4j
public class ActionImageService {

    private static final String BASE_PATH = "images/actions";
    private static final String BASE_URL  =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com";

    private static final Set<String> SUPPORTED_ACTIONS = Set.of(
            "썰기","다지기","채썰기","손질하기","볶기","튀기기","끓이기",
            "찌기(스팀)","데치기","구이","조림","무치기","절이기",
            "담그기(마리네이드)","섞기","젓기","버무리기","로스팅",
            "캐러멜라이즈","부치기"
    );

    public boolean isSupportedAction(String action) {
        return action != null && SUPPORTED_ACTIONS.contains(action);
    }
    public String generateImageKey(AiRecipeConcept concept, String action) {
        if (!isSupportedAction(action)) {
            log.warn("지원하지 않는 조리 액션입니다: {}", action);
            return null;
        }
        String typeFolder = getFolderByConcept(concept);
        return String.format("%s/%s/%s.webp",
                BASE_PATH,
                typeFolder,
                action
        );
    }

    public String generateImageUrl(String imageKey) {
        if (imageKey == null) {
            return null;
        }
        return String.format("%s/%s", BASE_URL, imageKey);
    }

    private String getFolderByConcept(AiRecipeConcept concept) {
        return switch (concept) {
            case INGREDIENT_FOCUS -> "classic";
            case COST_EFFECTIVE -> "creative";
            case NUTRITION_BALANCE -> "healthy";
            case FINE_DINING -> "indulgent";
        };
    }
}
