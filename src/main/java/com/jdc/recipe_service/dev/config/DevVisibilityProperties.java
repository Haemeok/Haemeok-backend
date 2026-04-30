package com.jdc.recipe_service.dev.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Dev V3 visibility 관련 properties.
 *
 * yaml prefix: {@code dev.visibility}
 *
 * RESTRICTED는 운영 검색/목록이 listingStatus 필터를 흡수하기 전까지는 운영 환경에서 노출 위험이 있다.
 * 사용자가 dev API({@code PATCH /api/dev/recipes/*}/visibility})로 PRIVATE→RESTRICTED를 호출하면
 * {@code applyVisibility(RESTRICTED)}가 {@code isPrivate=false}로 매핑하므로, 운영 V1/V2 검색
 * (예: {@code OpenSearchService.buildCommonQuery}, {@code RecipeQueryRepositoryImplV2.searchStatic})에
 * RESTRICTED 레시피가 갑자기 노출됨.
 *
 * 이 flag로 RESTRICTED 활성화를 환경별로 통제 — prod default false, dev 검증 환경에서만 true.
 */
@ConfigurationProperties(prefix = "dev.visibility")
@Component
@Getter
@Setter
public class DevVisibilityProperties {

    /**
     * RESTRICTED visibility 활성화 여부.
     *  - false (default): RESTRICTED 요청 시 INVALID_INPUT_VALUE(901). PUBLIC/PRIVATE만 허용.
     *  - true: RESTRICTED 활성화. dev 검증 환경(application-local.yml 등)에서만 켜야 함.
     *
     * 운영 검색/목록이 listingStatus를 흡수하면 이 flag 자체가 불필요해짐.
     */
    private boolean restrictedEnabled = false;
}
