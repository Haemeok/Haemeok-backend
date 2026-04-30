package com.jdc.recipe_service.dev.domain.type.image;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;

import java.util.Arrays;

/**
 * Dev 이미지 생성 화이트리스트.
 * recipes.image_generation_model 컬럼에는 identifier 문자열을 그대로 저장한다.
 *
 * - identifier : 외부(프론트/DB) 문자열. 안정적인 컨트랙트.
 * - apiModel   : provider에게 실제로 보낼 모델 이름. provider 모델 네이밍 변경 시 여기만 수정.
 * - quality    : provider에게 보낼 품질 옵션 (Gemini는 null).
 *
 * 새 모델을 허용하려면 enum 상수를 추가한다. 임의 문자열은 BAD_REQUEST(703) UNSUPPORTED_IMAGE_MODEL.
 */
public enum DevImageGenModel {
    GEMINI_2_5_FLASH_IMAGE("gemini-2.5-flash-image", Provider.GEMINI, "gemini-2.5-flash-image", null),
    GPT_IMAGE_2_LOW("gpt-image-2-low", Provider.GPT, "gpt-image-2", "low"),
    GPT_IMAGE_2_MEDIUM("gpt-image-2-medium", Provider.GPT, "gpt-image-2", "medium"),
    GPT_IMAGE_2_HIGH("gpt-image-2-high", Provider.GPT, "gpt-image-2", "high");

    public enum Provider { GEMINI, GPT }

    private final String identifier;
    private final Provider provider;
    private final String apiModel;
    private final String quality;

    DevImageGenModel(String identifier, Provider provider, String apiModel, String quality) {
        this.identifier = identifier;
        this.provider = provider;
        this.apiModel = apiModel;
        this.quality = quality;
    }

    public String identifier() { return identifier; }
    public Provider provider() { return provider; }
    public String apiModel() { return apiModel; }
    public String quality() { return quality; }

    public static DevImageGenModel fromIdentifier(String id) {
        if (id == null || id.isBlank()) {
            throw new CustomException(ErrorCode.UNSUPPORTED_IMAGE_MODEL);
        }
        return Arrays.stream(values())
                .filter(m -> m.identifier.equals(id))
                .findFirst()
                .orElseThrow(() -> new CustomException(ErrorCode.UNSUPPORTED_IMAGE_MODEL));
    }
}
