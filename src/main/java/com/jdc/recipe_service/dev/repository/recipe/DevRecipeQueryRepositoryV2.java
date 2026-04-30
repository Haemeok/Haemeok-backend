package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;

import java.util.List;

/**
 * Dev V3 search repository.
 *
 * 두 path를 모두 지원:
 *  - {@link #searchStatic}: QueryDSL fallback (OpenSearch unhealthy 또는 dev mirror disabled 시)
 *  - {@link #findAllByIds}: OpenSearch path 후처리 — search hits에서 받은 id로 DB DTO 보강
 *
 * 두 메서드 모두 {@link com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryPredicates#accessibleBy}
 * 의미를 따른다 (RESTRICTED 누수 차단). 운영 V2의 {@code isPrivate=false} 필터와는 다른 정책.
 */
public interface DevRecipeQueryRepositoryV2 {

    /**
     * QueryDSL 기반 검색 (OpenSearch 미사용 경로).
     *
     * @param viewerId 로그인한 사용자 id (anonymous면 null) — owner 분기에 사용
     */
    Page<DevRecipeSimpleStaticDto> searchStatic(RecipeSearchCondition condition,
                                                 Pageable pageable,
                                                 @Nullable Long viewerId);

    /**
     * OpenSearch search hits로 받은 id 목록을 DB DTO로 보강.
     * 호출자(DevRecipeSearchService)가 OpenSearch 결과 순서를 유지하도록 매핑.
     *
     * 주의: id-only fetch에 그치지 않고 {@link DevRecipeQueryPredicates#accessibleBy} 정책을 한 번 더 적용한다.
     * 이유: OpenSearch dev index가 stale하거나 visibility 변경 mirror가 누락되면, accessibleByFilter를 통과한
     * id 중 실제 DB 상태는 PRIVATE/RESTRICTED/non-ACTIVE인 row가 있을 수 있다. DB가 source of truth이므로
     * 응답 직전에 한 번 더 거른다. imageReady 조건도 동일 이유로 적용.
     *
     * @param viewerId 로그인한 사용자 id (anonymous면 null) — owner 분기에 사용
     */
    List<DevRecipeSimpleStaticDto> findAllByIds(List<Long> ids, @Nullable Long viewerId);
}
