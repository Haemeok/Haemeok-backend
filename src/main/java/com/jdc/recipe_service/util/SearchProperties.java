package com.jdc.recipe_service.util;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 검색 관련 properties.
 *
 * yaml prefix: {@code search}
 *
 * dev-index는 운영 검색 인덱스에 영향을 주지 않으면서 dev V3 검색 필드(visibility/listingStatus 등)를
 * 시험하기 위한 별도 OpenSearch alias 설정. 기본값 disabled — dev 검증 환경에서만 enable.
 *
 * 운영 alias 이름은 코드에 하드코딩(현재 "recipes")되어 있고 이 properties로 옮기는 작업은 별도 phase.
 */
@ConfigurationProperties(prefix = "search")
@Component
@Getter
@Setter
public class SearchProperties {

    private String engine;

    private DevIndex devIndex = new DevIndex();

    @Getter
    @Setter
    public static class DevIndex {
        /**
         * dev mirror indexing 활성화 여부.
         *  - false (default): RecipeIndexingService는 dev alias에 dual-write하지 않음
         *  - true: 운영 인덱스 write 성공 후 dev alias에도 mirror write (실패 swallow + log + metric)
         *
         * dev mirror 실패는 절대 운영 recipe 저장에 영향을 주지 않는다.
         */
        private boolean enabled = false;

        /**
         * dev OpenSearch alias 이름. 실제 concrete index는 버전 suffix를 붙여 별도 생성:
         *   alias:           recipes_v3_dev
         *   concrete index:  recipes_v3_dev_20260426_001
         *
         * 인덱스/alias 생성 절차는 docs/dev/v3-search-index.md 참고.
         */
        private String alias = "recipes_v3_dev";
    }
}
