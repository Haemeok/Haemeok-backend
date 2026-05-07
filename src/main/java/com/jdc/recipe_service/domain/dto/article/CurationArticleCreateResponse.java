package com.jdc.recipe_service.domain.dto.article;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

/**
 * 큐레이션 아티클 생성 응답.
 *
 * <p>응답에 최종 {@code slug}를 포함하는 이유: V1.1부터 백엔드가 자동으로 unique suffix({@code -2}, {@code -3} …)를
 * 붙이므로 프론트가 보낸 base slug와 실제 저장된 slug가 다를 수 있다. 프론트는 응답의 slug를 그대로 사용해
 * 상세 URL/링크를 만든다.
 */
@Getter
@Builder
@Schema(description = "큐레이션 아티클 생성 응답")
public class CurationArticleCreateResponse {

    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    @Schema(description = "생성된 아티클 ID (HashID 문자열)", example = "xJvY7aBp", type = "string")
    private Long articleId;

    @Schema(description = "최종 확정된 slug. 프론트가 보낸 base slug와 다를 수 있다 (자동 suffix 적용 시)",
            example = "summer-diet-cucumber-recipes-2")
    private String slug;
}
