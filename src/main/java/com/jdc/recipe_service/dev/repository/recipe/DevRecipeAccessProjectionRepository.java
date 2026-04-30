package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

/**
 * Recipe 4-enum + ownerId만 batch로 조회하는 dev 전용 repository.
 *
 * 운영 {@code RecipeRepository}는 zero-touch 유지를 위해 신규 method를 추가하지 않고 dev 전용 인터페이스를 둔다.
 * 같은 Recipe 엔티티를 가리키지만 dev V3 access matrix용 좁은 projection만 노출.
 */
@Repository
public interface DevRecipeAccessProjectionRepository extends JpaRepository<Recipe, Long> {

    @Query("""
            SELECT new com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection(
                r.id, r.user.id, r.lifecycleStatus, r.visibility, r.listingStatus, r.imageStatus)
            FROM Recipe r
            WHERE r.id IN :ids
            """)
    List<DevRecipeAccessProjection> findAccessProjectionsByIds(@Param("ids") Collection<Long> ids);
}
