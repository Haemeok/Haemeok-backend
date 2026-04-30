package com.jdc.recipe_service.dev.service.image;

import com.jdc.recipe_service.dev.domain.type.image.DevImageGenModel;
import com.jdc.recipe_service.service.image.GeminiImageService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 이미지 생성 모델 식별자(string)를 받아 적절한 provider로 라우팅한다.
 * 호출자(dev AI/YouTube generation service)는 이 service 하나만 의존하면 된다.
 *
 * 화이트리스트 검증은 DevImageGenModel.fromIdentifier()에서 수행 (UNSUPPORTED_IMAGE_MODEL).
 * 응답 contract: List<String> S3 webp URL.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevImageGenRouterService {

    private final GeminiImageService geminiImageService;
    private final DevGptImageService devGptImageService;

    public List<String> generate(String modelIdentifier, String prompt, Long userId, Object recipeId) {
        DevImageGenModel model = DevImageGenModel.fromIdentifier(modelIdentifier);

        return switch (model.provider()) {
            case GEMINI -> {
                log.info("[DevImageGenRouter] -> Gemini. recipeId={}, identifier={}, apiModel={}",
                        recipeId, model.identifier(), model.apiModel());
                yield geminiImageService.generateImageUrls(prompt, userId, recipeId);
            }
            case GPT -> {
                log.info("[DevImageGenRouter] -> OpenAI gpt-image. recipeId={}, identifier={}, apiModel={}, quality={}",
                        recipeId, model.identifier(), model.apiModel(), model.quality());
                yield devGptImageService.generateImageUrls(prompt, model.apiModel(), model.quality(), userId, recipeId);
            }
        };
    }
}
