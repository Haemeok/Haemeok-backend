package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.IngredientUnit;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

/**
 * normalized unit label → 매칭되는 {@link IngredientUnit}.
 *
 * <p><b>1.1 정책</b>: repository 의존하지 않는 pure resolver.
 *
 * <p><b>LAZY association invariant</b>: 이 클래스는 {@code unit.getIngredient()}를 절대
 * 호출하지 않는다. {@link IngredientUnit#getIngredient()}는 LAZY 관계라 트랜잭션 밖이나
 * batch prefetch 시 LazyInitializationException / N+1을 유발할 수 있다. 후보를 ingredient별로
 * 미리 좁히는 책임은 caller(service/normalizer 측의 lookup map 구성)에 있다.
 *
 * <p>caller 패턴:
 * <pre>{@code
 *   Map<Long, List<IngredientUnit>> unitsByIngredientId = ...; // ingredient_id → units
 *   List<IngredientUnit> candidates = unitsByIngredientId.get(ingredient.getId());
 *   resolver.resolve(normalizedUnitLabel, candidates);
 * }</pre>
 */
@Component
public class IngredientUnitResolver {

    /**
     * @param normalizedUnitLabel {@link UnitNormalizer}의 출력. blank면 즉시 empty.
     * @param preFilteredCandidates 호출자가 이미 ingredient별로 좁혀둔 unit 후보.
     */
    public Optional<IngredientUnit> resolve(String normalizedUnitLabel,
                                            List<IngredientUnit> preFilteredCandidates) {
        if (normalizedUnitLabel == null || normalizedUnitLabel.isBlank()) return Optional.empty();
        if (preFilteredCandidates == null || preFilteredCandidates.isEmpty()) return Optional.empty();

        return preFilteredCandidates.stream()
                .filter(u -> normalizedUnitLabel.equals(u.getNormalizedUnitLabel()))
                .findFirst();
    }
}
